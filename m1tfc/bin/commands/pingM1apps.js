'use strict';

const delay = require('delay');
const nodePortScanner = require('node-port-scanner');
const exitCodes = require('../../src/exitCodes');
const errorCodes = require('../../bin/errorCodes');
const m1boot = require('../../tests/m1boot');
const testBoardLink = require('../../src/testBoardLink');
const { loadConfigData, ensureSerialOption, createCommandLogger, initDb, powerDownTarget, exitCommandFailure } = require('../m1tfcShared');

module.exports = function registerPingM1Apps(program) {
    program.command('pingM1apps')
        .description('try to establish connection to port 80')
        .option('-s, --serial <string>', 'vendor serial number')
        .option('-d, --debug <level>', 'set debug level, 0 error, 1 - info, 2 - debug ')
        .action(async (options) => {
            const configData = await loadConfigData();
            let logfile;
            let db;
            try {
                await ensureSerialOption(options, console);
                logfile = createCommandLogger(options.serial, '   apps', configData, options.debug);
                if (!configData.pingPorts) {
                    logfile.info('pinging port 80 is disabled in config file');
                    return;
                }
                logfile.info('--------------------------------------------');
                logfile.info('checking port 80 ...');
                db = initDb(logfile);
                await delay(5);
                await testBoardLink.initSerial(configData.testBoardTerminalDev, configData.serialBaudrate, logfile);
                await testBoardLink.retrieveIoDef();
                testBoardLink.getIoDef();
                logfile.info('M1-3200 power is on');
                await m1boot.deActivateDFU();
                await testBoardLink.targetPower(true);
                let timerCount = 10;
                const interval = setInterval(async () => {
                    const results = await nodePortScanner(configData.m1defaultIP, [80]);
                    if (!timerCount) {
                        clearInterval(interval);
                        logfile.error('test failed');
                        await delay(100);
                        await powerDownTarget();
                        db.updateErrorCode(options.serial, errorCodes.codes.APP80.errorCode, 'E');
                        process.exit(exitCodes.commandFailed);
                    }
                    timerCount -= 1;
                    if (results.ports.open.includes(80) && results.ports.open.includes(80)) {
                        logfile.info('Test passed, M1 web app is alive');
                        clearInterval(interval);
                        await powerDownTarget();
                        delay(500);
                        process.exit(exitCodes.normalExit);
                    }
                    logfile.debug(`port 80 open = ${results.ports.open.includes(80)}`);
                }, 5000);
            }
            catch (err) {
                if (!logfile) logfile = console;
                await powerDownTarget();
                if (db) db.updateErrorCode(options.serial, errorCodes.codes.APP80.errorCode, 'T');
                await exitCommandFailure(err, logfile);
            }
        });
};

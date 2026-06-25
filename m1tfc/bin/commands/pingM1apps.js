'use strict';

const delay = require('delay');
const nodePortScanner = require('node-port-scanner');
const logger = require('../../utils/logger');
const testBoardLink = require('../../src/testBoardLink');
const m1boot = require('../../tests/m1boot');
const sqliteDriver = require('../../utils/sqliteDriver');
const exitCodes = require('../../src/exitCodes');
const errorCodes = require('../errorCodes');
const { loadConfig, errorAndExit, applyRuntime } = require('../commandSupport');

function register(program) {
    program
        .command('pingM1apps')
        .description('try to establish connection to port 80')
        .option('-s, --serial <string>', 'vendor serial number')
        .option('-d, --debug <level>', 'set debug level, 0 error, 1 - info, 2 - debug ')
        .action(async options => {
            const configData = await loadConfig();
            let logfile;
            let db;
            applyRuntime(configData, { serial: options.serial, debugLevel: options.debug || '0' });
            try {
                if (!options.serial) {
                    await errorAndExit('must define vendor serial number', console);
                }
                logfile = logger.getLogger(
                    options.serial,
                    '   apps',
                    options.serial,
                    configData.mtfDir,
                    options.debug
                );
                if (!configData.pingPorts) {
                    logfile.info('pinging port 80 is disabled in config file');
                    return;
                }
                logfile.info('--------------------------------------------');
                logfile.info('checking port 80 ...');
                db = sqliteDriver.initialize(logfile);
                await delay(5);
                await testBoardLink.initSerial(
                    configData.testBoardTerminalDev,
                    configData.serialBaudrate,
                    logfile
                );
                await testBoardLink.retrieveIoDef();
                testBoardLink.getIoDef();
                await m1boot.deActivateDFU();
                await testBoardLink.targetPower(true);
                let timerCount = 10;
                const interval = setInterval(async() => {
                    const results = await nodePortScanner(configData.m1defaultIP, [80]);
                    if (!timerCount) {
                        clearInterval(interval);
                        logfile.error('test failed');
                        await delay(100);
                        await testBoardLink.targetPower(false);
                        db.updateErrorCode(options.serial, errorCodes.codes.APP80.errorCode, 'E');
                        process.exit(exitCodes.commandFailed);
                    }
                    timerCount -= 1;
                    if (results.ports.open.includes(80)) {
                        logfile.info('Test passed, M1 web app is alive');
                        clearInterval(interval);
                        await testBoardLink.targetPower(false);
                        await delay(500);
                        process.exit(exitCodes.normalExit);
                    }
                    logfile.debug(`port 80 open = ${results.ports.open.includes(80)}`);
                }, 5000);
            } catch (err) {
                if (!logfile) {
                    logfile = console;
                }
                logfile.error(err);
                await testBoardLink.targetPower(false);
                if (db && options.serial) {
                    db.updateErrorCode(options.serial, errorCodes.codes.APP80.errorCode, 'T');
                }
                await delay(100);
                process.exit(exitCodes.commandFailed);
            }
        });
}

module.exports = {
    register
};

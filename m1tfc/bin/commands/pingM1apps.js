'use strict';

const delay = require('delay');
const logger = require('../../utils/logger');
const os = require('../../utils/os');
const testBoardLink = require('../../src/testBoardLink');
const m1boot = require('../../tests/m1boot');
const sqliteDriver = require('../../utils/sqliteDriver');
const exitCodes = require('../../src/exitCodes');
const errorCodes = require('../errorCodes');
const { loadConfig, errorAndExit, applyRuntime } = require('../commandSupport');

async function waitForPort80(ipAddress, logfile) {
    let timerCount = 30;
    while (timerCount > 0) {
        try {
            await os.executeShellCommand(`nc -z -w 2 ${ipAddress} 80`, logfile, false, true);
            return true;
        } catch (err) {
            logfile.debug(`port 80 open = false`);
            timerCount -= 1;
            await delay(5000);
        }
    }

    return false;
}

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
                logfile.debug('Setting boot target to runtime');
                await testBoardLink.targetPower(false);
                await testBoardLink.batteryOn(false);
                await m1boot.deActivateDFU();
                await delay(100);
                logfile.debug('Power cycling target');
                await testBoardLink.targetPower(true);
                await testBoardLink.batteryOn(true);
                logfile.info('Waiting for target app port 80');

                const portIsOpen = await waitForPort80(configData.m1defaultIP, logfile);
                if (!portIsOpen) {
                    logfile.error('test failed');
                    await testBoardLink.targetPower(false);
                    db.updateErrorCode(options.serial, errorCodes.codes.APP80.errorCode, 'E');
                    process.exit(exitCodes.commandFailed);
                }

                logfile.info('Test passed, M1 web app is alive');
                await testBoardLink.targetPower(false);
                await delay(500);
                process.exit(exitCodes.normalExit);
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

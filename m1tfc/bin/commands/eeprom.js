'use strict';

const delay = require('delay');
const logger = require('../../utils/logger');
const Eeprom = require('../../tests/programEeprom');
const common = require('../../tests/common');
const sqliteDriver = require('../../utils/sqliteDriver');
const exitCodes = require('../../src/exitCodes');
const errorCodes = require('../errorCodes');
const { loadConfig, errorAndExit, applyRuntime } = require('../commandSupport');

function register(program) {
    program
        .command('eeprom')
        .description('Program I2C EEPROM.')
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
                    ' eeprom',
                    options.serial,
                    configData.mtfDir,
                    options.debug
                );
                db = sqliteDriver.initialize(logfile);
                if (!configData.progEEPROM) {
                    logfile.error('Prog EEPROM is disabled');
                    await delay(100);
                    process.exit(exitCodes.normalExit);
                }
                if (!configData.vendorSite) {
                    await errorAndExit(
                        'must define vendor site in $SNAP_DATA/config.json',
                        logfile
                    );
                }

                logfile.info('--------------------------------------------');
                logfile.info('Executing writing I2C EEPROM command ...');

                const eeprom = new Eeprom(
                    `${configData.mtfDir}/${configData.ictFWFilePath}`,
                    logfile
                );
                await eeprom.init(
                    configData.testBoardTerminalDev,
                    configData.serialBaudrate,
                    configData.m1SerialDev,
                    configData.serialBaudrate
                );
                await delay(400);
                await eeprom.program(
                    configData.programmingCommand,
                    options.serial,
                    configData.vendorSite,
                    configData.forceEppromOverwrite
                );
                await delay(100);
                await common.testEndSuccess();
                process.exit(exitCodes.normalExit);
            } catch (err) {
                if (!logfile) {
                    logfile = console;
                }
                logfile.error(err);
                if (db && options.serial) {
                    db.updateErrorCode(
                        options.serial,
                        errorCodes.codes.EEPROMUPDATE.errorCode,
                        'E'
                    );
                }
                await delay(100);
                process.exit(exitCodes.commandFailed);
            }
        });
}

module.exports = {
    register
};

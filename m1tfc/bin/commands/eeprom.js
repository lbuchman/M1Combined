'use strict';

const delay = require('delay');
const exitCodes = require('../../src/exitCodes');
const errorCodes = require('../../bin/errorCodes');
const Eeprom = require('../../tests/programEeprom');
const common = require('../../tests/common');
const {
    loadConfigData,
    ensureSerialOption,
    applyFirmwareDir,
    createCommandLogger,
    initDb
} = require('../m1tfcShared');

module.exports = function registerEeprom(program) {
    program.command('eeprom')
        .description('Program I2C EEPROM.')
        .option('-s, --serial <string>', 'vendor serial number')
        .option('-d, --debug <level>', 'set debug level, 0 error, 1 - info, 2 - debug ')
        .action(async (options) => {
            const configData = await loadConfigData();
            let logfile;
            let db;
            let exitCode = exitCodes.normalExit;
            try {
                applyFirmwareDir(configData);
                await ensureSerialOption(options, console);
                logfile = createCommandLogger(options.serial, ' eeprom', configData, options.debug);
                db = initDb(logfile);
                if (!configData.progEEPROM) {
                    logfile.error('Prog EEPROM is disabled');
                    return;
                }
                if (!configData.vendorSite) throw new Error('must define vendor site in $SNAP_DATA/config.json');

                logfile.info('--------------------------------------------');
                logfile.info('Executing writing I2C EEPROM command ...');

                const eeprom = new Eeprom(`${configData.mtfDir}/${configData.ictFWFilePath}`, logfile);
                await eeprom.init(configData.testBoardTerminalDev, configData.serialBaudrate, configData.m1SerialDev, configData.serialBaudrate);
                await delay(400);
                await eeprom.program(configData.programmingCommand, options.serial, configData.vendorSite, configData.forceEppromOverwrite);
                await delay(100);
                await common.testEndSuccess();
            }
            catch (err) {
                if (!logfile) logfile = console;
                logfile.error(err);
                if (db) db.updateErrorCode(options.serial, errorCodes.codes.EEPROMUPDATE.errorCode, 'E');
                exitCode = exitCodes.commandFailed;
            }
            finally {
                process.exit(exitCode);
            }
        });
};

'use strict';

const delay = require('delay');
const exitCodes = require('../../src/exitCodes');
const errorCodes = require('../../bin/errorCodes');
const ProgramMac = require('../../tests/programMAC');
const Eeprom = require('../../tests/programEeprom');
const buzzer = require('../../tests/buzzer');
const utils = require('../../utils/utils');
const testBoardLink = require('../../src/testBoardLink');
const {
    loadConfigData, ensureSerialOption, applyFirmwareDir,
    createCommandLogger, initDb, powerDownTarget
} = require('../m1tfcShared');

module.exports = function registerMakeLabel(program) {
    program.command('makelabel')
        .description('prints the M1-3200 Label')
        .option('-s, --serial <string>', 'vendor serial number')
        .option('-d, --debug <level>', 'set debug level, 0 error, 1 - info, 2 - debug ')
        .option('-e, --error', 'error print from the database')
        .action(async (options) => {
            await ensureSerialOption(options, console);
            const configData = await loadConfigData();
            const logfile = createCommandLogger(options.serial, '  label', configData, options.debug);
            const db = initDb(logfile);
            let exitCode = exitCodes.normalExit;
            if (!configData.makeLabel) {
                logfile.info('Make Label is disabled');
                process.exit(exitCodes.normalExit);
            }
            try {
                applyFirmwareDir(configData);
                let eepromData = {};
                await ensureSerialOption(options, logfile);

                await testBoardLink.initSerial(configData.testBoardTerminalDev, configData.serialBaudrate, logfile);
                let dbError = db.getErrorCode(options.serial);
                let isErrorPrint = options.error;
                if (dbError && dbError.length) {
                    isErrorPrint = true;
                }
                if (isErrorPrint) {
                    dbError = db.getErrorCode(options.serial);
                    if (!dbError.length) {
                        dbError = ['0000ERR_UNDEF'];
                    }
                    await utils.printLabel(configData.productName, '00:00:00:00:00:00', options.serial, configData.vendorSite, dbError, logfile);
                    await powerDownTarget();
                }
                else {
                    logfile.info('--------------------------------------------');
                    logfile.info('Printing Label ...');
                    const macProgram = new ProgramMac(configData, options.serial, logfile);
                    await macProgram.init(configData.testBoardTerminalDev, configData.serialBaudrate);
                    const retValue = await macProgram.getMac(configData.programmingCommand);
                    if (retValue.exitCode !== exitCodes.normalExit) throw new Error('Could not read MP1 OTP');
                    const uid = retValue.mac.toUpperCase();

                    const eeprom = new Eeprom(`${configData.mtfDir}/${configData.ictFWFilePath}`, logfile);
                    await eeprom.init(configData.testBoardTerminalDev, configData.serialBaudrate, configData.m1SerialDev, configData.serialBaudrate);
                    await delay(400);
                    eepromData = await eeprom.get(configData.programmingCommand);

                    if (eepromData.serial === '' || uid === '' || uid === '00:00:00:00:00:00') {
                        await buzzer.buzzerBeepFailed();
                        await buzzer.testFailed();
                        exitCode = exitCodes.commandFailed;
                        return;
                    }

                    logfile.debug('Sending data to the printer');
                    const eepromSerial = eepromData.serial.substring(3).slice(0, -2);
                    if (eepromSerial !== options.serial) {
                        db.updateErrorCode(options.serial, errorCodes.codes.SERIAL_MISSMATH.errorCode, 'E');
                        throw new Error(`serial number ${options.serial} does not match EEPROM value ${eepromData.serial.substring(3).slice(0, -2)}`);
                    }

                    const productName = configData.productName || 'M1-3200';
                    await utils.printLabel(productName, uid, eepromData.serial.substring(3).slice(0), '', [], logfile);
                    logfile.debug('Label is printed');
                    await powerDownTarget();
                }
            }
            catch (err) {
                if (err.message) logfile.error(err.message);
                else logfile.error(err);
                await buzzer.buzzerBeepFailed();
                await powerDownTarget();
                exitCode = exitCodes.commandFailed;
            }
            finally {
                process.exit(exitCode);
            }
        });
};

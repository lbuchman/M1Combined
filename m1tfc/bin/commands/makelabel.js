'use strict';

const delay = require('delay');
const ProgramMac = require('../../tests/programMAC');
const Eeprom = require('../../tests/programEeprom');
const testBoardLink = require('../../src/testBoardLink');
const buzzer = require('../../tests/buzzer');
const utils = require('../../utils/utils');
const exitCodes = require('../../src/exitCodes');
const errorCodes = require('../errorCodes');
const { runCommand } = require('../commandRunner');

function register(program) {
    program
        .command('makelabel')
        .description('prints the M1-3200 Label')
        .option('-s, --serial <string>', 'vendor serial number')
        .option('-d, --debug <level>', 'set debug level, 0 error, 1 - info, 2 - debug ')
        .option('-e, --error', 'error print from the database')
        .action(options => {
            runCommand(options, '  label', null, async (configData, logfile, db) => {
                if (!configData.makeLabel) {
                    logfile.info('Make Label is disabled');
                    await delay(100);
                    process.exit(exitCodes.normalExit);
                }

                await testBoardLink.initSerial(
                    configData.testBoardTerminalDev,
                    configData.serialBaudrate,
                    logfile
                );
                let dbError = db.getErrorCode(options.serial);
                if (dbError && dbError.length) {
                    options.error = true;
                }

                if (options.error) {
                    dbError = dbError.length ? dbError : ['0000ERR_UNDEF'];
                    await utils.printLabel(
                        configData.productName,
                        '00:00:00:00:00:00',
                        options.serial,
                        configData.vendorSite,
                        dbError,
                        logfile
                    );
                    await testBoardLink.targetPower(false);
                    await testBoardLink.batteryOn(false);
                    await delay(100);
                    process.exit(exitCodes.normalExit);
                }

                logfile.info('--------------------------------------------');
                logfile.info('Printing Label ...');
                const macProgram = new ProgramMac(configData, options.serial, logfile);
                await macProgram.init(configData.testBoardTerminalDev, configData.serialBaudrate);
                const retValue = await macProgram.getMac(configData.programmingCommand);
                if (retValue.exitCode !== exitCodes.normalExit) {
                    throw new Error('Could not read MP1 OTP');
                }
                const mac = retValue.mac.toUpperCase();

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
                const eepromData = await eeprom.get(configData.programmingCommand);
                if (!eepromData.serial || !mac || mac === '00:00:00:00:00:00') {
                    await buzzer.buzzerBeepFailed();
                    await delay(100);
                    process.exit(exitCodes.commandFailed);
                }

                const eepromSerial = eepromData.serial.substring(3).slice(0, -2);
                if (eepromSerial !== options.serial) {
                    db.updateErrorCode(
                        options.serial,
                        errorCodes.codes.SERIAL_MISSMATH.errorCode,
                        'E'
                    );
                    throw new Error(
                        `serial number ${options.serial} does not match EEPROM value ${eepromSerial}`
                    );
                }

                await utils.printLabel(
                    configData.productName.toUpperCase(),
                    mac,
                    options.serial,
                    configData.vendorSite,
                    [],
                    logfile
                );
                logfile.debug('Label is printed');
                await testBoardLink.targetPower(false);
                await testBoardLink.batteryOn(false);
                await delay(100);
                process.exit(exitCodes.normalExit);
            }).catch(async () => {
                // handle specific error behavior from old makelabel.js
                await buzzer.buzzerBeepFailed();
                await delay(100);
                await testBoardLink.targetPower(false);
                await testBoardLink.batteryOn(false);
                process.exit(exitCodes.commandFailed);
            });
        });
}

module.exports = {
    register
};

'use strict';

const delay = require('delay');
const Eeprom = require('../../tests/programEeprom');
const common = require('../../tests/common');
const exitCodes = require('../../src/exitCodes');
const { errorAndExit } = require('../commandSupport');
const { runCommand } = require('../commandRunner');

function register(program) {
    program
        .command('eeprom')
        .description('Program I2C EEPROM.')
        .option('-s, --serial <string>', 'vendor serial number')
        .option('-d, --debug <level>', 'set debug level, 0 error, 1 - info, 2 - debug ')
        .action(options => {
            runCommand(options, ' eeprom', 'EEPROMUPDATE', async (configData, logfile, _db) => {
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
                    options.serial, // use runCommand serial? using options.serial works too.
                    configData.vendorSite,
                    configData.forceEppromOverwrite
                );
                await delay(100);
                await common.testEndSuccess();
                process.exit(exitCodes.normalExit);
            });
        });
}

module.exports = {
    register
};

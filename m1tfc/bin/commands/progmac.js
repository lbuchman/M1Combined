'use strict';

const exitCodes = require('../../src/exitCodes');
const errorCodes = require('../../bin/errorCodes');
const ProgramMac = require('../../tests/programMAC');
const {
    loadConfigData,
    ensureSerialOption,
    applyFirmwareDir,
    createCommandLogger,
    initDb
} = require('../m1tfcShared');

module.exports = function registerProgMac(program) {
    program.command('progmac')
        .description('program MAC to MP1 OTP, cannot be undone')
        .option('-s, --serial <string>', 'vendor serial number')
        .option('-d, --debug <level>', 'set debug level, 0 error, 1 - info, 2 - debug')
        .action(async (options) => {
            const configData = await loadConfigData();
            applyFirmwareDir(configData);
            let logfile;
            let db;
            let exitCode = exitCodes.normalExit;
            try {
                await ensureSerialOption(options, console);
                logfile = createCommandLogger(options.serial, 'progmac', configData, options.debug);
                db = initDb(logfile);
                logfile.info('--------------------------------------------');
                logfile.info('Executing program MAC command ...');
                const macProgram = new ProgramMac(configData, options.serial, logfile);
                await macProgram.init(configData.testBoardTerminalDev, configData.serialBaudrate);
                await macProgram.run(configData.programmingCommand);
                await macProgram.runProgSecret(configData.programmingCommand);
            }
            catch (err) {
                if (!logfile) logfile = console;
                if (err.message) logfile.error(err.message);
                else logfile.error(err);
                if (db) db.updateErrorCode(options.serial, errorCodes.codes.MAC.errorCode, 'E');
                exitCode = err.level || exitCodes.commandFailed;
            }
            finally {
                process.exit(exitCode);
            }
        });
};

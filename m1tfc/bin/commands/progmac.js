'use strict';

const delay = require('delay');
const logger = require('../../utils/logger');
const ProgramMac = require('../../tests/programMAC');
const sqliteDriver = require('../../utils/sqliteDriver');
const exitCodes = require('../../src/exitCodes');
const errorCodes = require('../errorCodes');
const { loadConfig, errorAndExit, applyRuntime } = require('../commandSupport');

function register(program) {
    program.command('progmac')
        .description('program MAC to MP1 OTP, cannot be undone')
        .option('-s, --serial <string>', 'vendor serial number')
        .option('-d, --debug <level>', 'set debug level, 0 error, 1 - info, 2 - debug')
        .action(async (options) => {
            const configData = await loadConfig();
            let logfile;
            let db;
            applyRuntime(configData, { serial: options.serial, debugLevel: options.debug || '0' });
            try {
                if (!options.serial) await errorAndExit('must define vendor serial number', console);
                logfile = logger.getLogger(options.serial, 'progmac', options.serial, configData.mtfDir, options.debug);
                db = sqliteDriver.initialize(logfile);
                logfile.info('--------------------------------------------');
                logfile.info('Executing program MAC command ...');
                const macProgram = new ProgramMac(configData, options.serial, logfile);
                await macProgram.init(configData.testBoardTerminalDev, configData.serialBaudrate);
                await macProgram.run(configData.programmingCommand);
                await macProgram.runProgSecret(configData.programmingCommand);
                process.exit(exitCodes.normalExit);
            }
            catch (err) {
                logfile.error(err.message);
                if (db && options.serial) db.updateErrorCode(options.serial, errorCodes.codes.MAC.errorCode, 'E');
                await delay(100);
                process.exit(err.level || exitCodes.commandFailed);
            }
        });
}

module.exports = {
    register
};

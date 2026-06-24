'use strict';

const fs = require('fs-extra');
const delay = require('delay');
const logger = require('../../utils/logger');
const FlashEmmc = require('../../tests/flashEmmc');
const sqliteDriver = require('../../utils/sqliteDriver');
const exitCodes = require('../../src/exitCodes');
const errorCodes = require('../errorCodes');
const { loadConfig, errorAndExit, applyRuntime } = require('../commandSupport');

function register(program) {
    program.command('flash')
        .description('program STM32M1 with the flash layout file')
        .option('-s, --serial <string>', 'vendor serial number')
        .option('-d, --debug <level>', 'set debug level, 0 error, 1 - info, 2 - debug ')
        .action(async (options) => {
            const configData = await loadConfig();
            let logfile;
            let db;
            applyRuntime(configData, { serial: options.serial, debugLevel: options.debug || '0' });
            try {
                if (!options.serial) await errorAndExit('must define vendor serial number', console);
                logfile = logger.getLogger(options.serial, '   eMMC', options.serial, configData.mtfDir, options.debug);
                db = sqliteDriver.initialize(logfile);
                logfile.info('--------------------------------------------');
                const revisionFile = fs.readFileSync(`${configData.mtfDir}/${configData.fwDir}/VERSION`);
                logfile.info(`Flashing eMMC revision: ${revisionFile.toString()}`);
                const tsvPath = `${configData.mtfDir}/${configData.fwDir}/${configData.layoutFilePath}`;
                const flashEmmc = new FlashEmmc(tsvPath, options.serial, logfile);
                await flashEmmc.init(configData.testBoardTerminalDev, configData.serialBaudrate);
                await flashEmmc.run(configData.programmingCommand, tsvPath);
            }
            catch (err) {
                if (!logfile) logfile = console;
                logfile.error(err);
                if (db && options.serial) db.updateErrorCode(options.serial, errorCodes.codes.FLASH.errorCode, 'E');
                await delay(100);
                process.exit(exitCodes.commandFailed);
            }
        });
}

module.exports = {
    register
};

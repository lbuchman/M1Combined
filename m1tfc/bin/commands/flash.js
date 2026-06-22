'use strict';

const errorCodes = require('../../bin/errorCodes');
const fs = require('fs-extra');
const FlashEmmc = require('../../tests/flashEmmc');
const {
    loadConfigData,
    ensureSerialOption,
    applyFirmwareDir,
    createCommandLogger,
    initDb,
    exitCommandFailure
} = require('../m1tfcShared');

module.exports = function registerFlash(program) {
    program.command('flash')
        .description('program STM32M1 with the flash layout file')
        .option('-s, --serial <string>', 'vendor serial number')
        .option('-d, --debug <level>', 'set debug level, 0 error, 1 - info, 2 - debug ')
        .action(async (options) => {
            const configData = await loadConfigData();
            let logfile;
            let db;
            try {
                await ensureSerialOption(options, console);
                applyFirmwareDir(configData);
                logfile = createCommandLogger(options.serial, '   eMMC', configData, options.debug);
                db = initDb(logfile);
                logfile.info('--------------------------------------------');
                const revisionFile = fs.readFileSync(`${configData.mtfDir}/${configData.fwDir}/VERSION`);
                logfile.info(`Flashing eMMC revision: ${revisionFile.toString()}`);
                const flashEmmc = new FlashEmmc(`${configData.mtfDir}/${configData.fwDir}/${configData.layoutFilePath}`, options.serial, logfile);
                await flashEmmc.init(configData.testBoardTerminalDev, configData.serialBaudrate);
                const exitCode = await flashEmmc.run(
                    configData.programmingCommand,
                    `${configData.mtfDir}/${configData.fwDir}/${configData.layoutFilePath}`
                );
                process.exit(exitCode);
            }
            catch (err) {
                if (!logfile) logfile = console;
                if (db) db.updateErrorCode(options.serial, errorCodes.codes.FLASH.errorCode, 'E');
                await exitCommandFailure(err, logfile);
            }
        });
};

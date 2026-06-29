'use strict';

const delay = require('delay');
const FlashEmmc = require('../../tests/flashEmmc');
const common = require('../../tests/common');
const exitCodes = require('../../src/exitCodes');
const { runCommand } = require('../commandRunner');

function register(program) {
    program
        .command('flash')
        .description('Program FW on eMMC flash.')
        .option('-s, --serial <string>', 'vendor serial number')
        .option('-d, --debug <level>', 'set debug level, 0 error, 1 - info, 2 - debug ')
        .action(options => {
            runCommand(options, ' flash', 'FLASHEMMC', async (configData, logfile, _db) => {
                if (!configData.progEmmc) {
                    logfile.error('Prog EMMC is disabled');
                    await delay(100);
                    process.exit(exitCodes.normalExit);
                }

                logfile.info('--------------------------------------------');
                logfile.info('Executing eMMC flashing command ...');

                const flashEmmc = new FlashEmmc(
                    `${configData.mtfDir}/${configData.ictFWFilePath}`,
                    logfile
                );
                await flashEmmc.init(
                    configData.testBoardTerminalDev,
                    configData.serialBaudrate,
                    configData.m1SerialDev,
                    configData.serialBaudrate
                );
                await delay(400);

                await flashEmmc.program(configData.programmingCommand, options.serial);
                await delay(100);
                await common.testEndSuccess();
                process.exit(exitCodes.normalExit);
            });
        });
}

module.exports = {
    register
};

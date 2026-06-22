'use strict';

const delay = require('delay');
const exitCodes = require('../../src/exitCodes');
const FuncTest = require('../../tests/funcTest');
const { loadConfigData, ensureSerialOption, applyFirmwareDir, createCommandLogger, exitCommandFailure } = require('../m1tfcShared');

module.exports = function registerFuncTest(program) {
    program.command('functest')
        .description('executes m1-3200 function test')
        .option('-s, --serial <string>', 'vendor serial number')
        .option('-d, --debug <level>', 'set debug level, 0 error, 1 - info, 2 - debug ')
        .action(async (options) => {
            const configData = await loadConfigData();
            let logfile;
            try {
                await ensureSerialOption(options, console);
                applyFirmwareDir(configData);
                process.env.m1defaultIP = configData.m1defaultIP;
                logfile = createCommandLogger(options.serial, '   func', configData, options.debug);
                if (configData.funcTestDisable) {
                    logfile.error('Func test is disabled');
                    await delay(100);
                    process.exit(exitCodes.normalExit);
                }
                logfile.info('--------------------------------------------');
                logfile.info('Executing m1-3200 functional test ...');
                process.env.SERIAL = options.serial;
                process.env.logDir = `${configData.mtfDir}/logs/${options.serial}`;
                const funcTest = new FuncTest(options.serial, configData, logfile);
                await funcTest.init(configData.testBoardTerminalDev, configData.serialBaudrate);
                const exitCode = await funcTest.run(
                    configData.programmingCommand,
                    `${configData.mtfDir}/${configData.fwDir}/${configData.layoutFilePath}`,
                    configData.login,
                    configData.password,
                    configData.m1SerialDev,
                    configData.skipUSBPenDriveTest,
                    '115200'
                );
                process.exit(exitCode);
            }
            catch (err) {
                if (!logfile) logfile = console;
                await exitCommandFailure(err, logfile);
            }
        });
};

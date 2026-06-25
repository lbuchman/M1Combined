'use strict';

const delay = require('delay');
const logger = require('../../utils/logger');
const FuncTest = require('../../tests/funcTest');
const exitCodes = require('../../src/exitCodes');
const { loadConfig, errorAndExit, applyRuntime } = require('../commandSupport');

function register(program) {
    program
        .command('functest')
        .description('executes m1-3200 function test')
        .option('-s, --serial <string>', 'vendor serial number')
        .option('-d, --debug <level>', 'set debug level, 0 error, 1 - info, 2 - debug ')
        .action(async options => {
            const configData = await loadConfig();
            let logfile;
            applyRuntime(configData, {
                serial: options.serial,
                debugLevel: options.debug || '0',
                logDir: options.serial ? `${configData.mtfDir}/logs/${options.serial}` : null
            });
            try {
                if (!options.serial) {
                    await errorAndExit('must define vendor serial number', console);
                }
                logfile = logger.getLogger(
                    options.serial,
                    '   func',
                    options.serial,
                    configData.mtfDir,
                    options.debug
                );
                if (configData.funcTestDisable) {
                    logfile.error('Func test is disabled');
                    await delay(100);
                    process.exit(exitCodes.normalExit);
                }
                logfile.info('--------------------------------------------');
                logfile.info('Executing m1-3200 functional test ...');
                const funcTest = new FuncTest(options.serial, configData, logfile);
                await funcTest.init(configData.testBoardTerminalDev, configData.serialBaudrate);
                await funcTest.run(
                    configData.programmingCommand,
                    `${configData.mtfDir}/${configData.fwDir}/${configData.layoutFilePath}`,
                    configData.login,
                    configData.password,
                    configData.m1SerialDev,
                    configData.skipUSBPenDriveTest,
                    '115200'
                );
                process.exit(exitCodes.normalExit);
            } catch (err) {
                if (!logfile) {
                    logfile = console;
                }
                logfile.error(err);
                await delay(100);
                process.exit(exitCodes.commandFailed);
            }
        });
}

module.exports = {
    register
};

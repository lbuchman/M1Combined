'use strict';

const exitCodes = require('../../src/exitCodes');
const testBoardLink = require('../../src/testBoardLink');
const { loadConfigData, createIctRunner } = require('../m1tfcShared');

module.exports = function registerTbCmd(program) {
    program.command('tbcmd')
        .description('execute test board raw command')
        .option('-c, --command <string>', 'test board command, make sure to inclose the command in ""')
        .action(async (options) => {
            const configData = await loadConfigData();
            const logfile = console;
            let exitCode = exitCodes.normalExit;
            try {
                await createIctRunner(configData, logfile, configData.ictFWFilePath);
                const output = await testBoardLink.sendCommand(options.command);
                logfile.log(JSON.stringify(output));
            }
            catch (err) {
                logfile.error(err);
                exitCode = exitCodes.commandFailed;
            }
            finally {
                process.exit(exitCode);
            }
        });
};

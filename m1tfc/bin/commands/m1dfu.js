'use strict';

const { loadConfigData, createIctRunner, exitCommandFailure } = require('../m1tfcShared');

module.exports = function registerM1Dfu(program) {
    program.command('m1dfu')
        .description('Start M1 in DFU mode and program bootstrap FW')
        .action(async () => {
            const configData = await loadConfigData();
            const logfile = console;
            try {
                const ictTestRunner = await createIctRunner(configData, logfile);
                const exitCode = await ictTestRunner.runTest(configData.programmingCommand, 'debug', 0, false, true);
                process.exit(exitCode);
            }
            catch (err) {
                await exitCommandFailure(err, logfile);
            }
        });
};

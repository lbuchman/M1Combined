'use strict';

const ProgramMac = require('../../tests/programMAC');
const exitCodes = require('../../src/exitCodes');
const { runCommand } = require('../commandRunner');

function register(program) {
    program
        .command('progmac')
        .description('program MAC to MP1 OTP, cannot be undone')
        .option('-s, --serial <string>', 'vendor serial number')
        .option('-d, --debug <level>', 'set debug level, 0 error, 1 - info, 2 - debug')
        .action(options => {
            runCommand(options, 'progmac', 'MAC', async (configData, logfile, _db) => {
                logfile.info('--------------------------------------------');
                logfile.info('Executing program MAC command ...');
                const macProgram = new ProgramMac(configData, options.serial, logfile);
                await macProgram.init(configData.testBoardTerminalDev, configData.serialBaudrate);
                await macProgram.run(configData.programmingCommand);
                await macProgram.runProgSecret(configData.programmingCommand);

                // Keep the default exit status or explicitly process.exit if custom err handling was done
                // runCommand handles default success via the executionFn resolving and normal error capturing
                // but the original explicity exit-ed with code 0 on the try block
                process.exit(exitCodes.normalExit);
            }).catch(err => {
                // To maintain custom catch block behavior that runCommand may obscure for specific custom err.level issues.
                if (err.level) {
                    process.exit(err.level);
                }
            });
        });
}

module.exports = {
    register
};

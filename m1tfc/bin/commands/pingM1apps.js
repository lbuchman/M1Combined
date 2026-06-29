'use strict';

// This command has been removed due to dependency on node-port-scanner
// which had transitive dependency on raw-socket (incompatible with Node 24+)

const exitCodes = require('../../src/exitCodes');

function register(program) {
    program
        .command('pingM1apps')
        .description('[DISABLED] try to establish connection to port 80')
        .option('-s, --serial <string>', 'vendor serial number')
        .option('-d, --debug <level>', 'set debug level, 0 error, 1 - info, 2 - debug ')
        .action(async() => {
            // eslint-disable-next-line no-console
            console.log('The pingM1apps command has been removed (network diagnostic functionality removed)');
            process.exit(exitCodes.normalExit);
        });
}

module.exports = {
    register
};

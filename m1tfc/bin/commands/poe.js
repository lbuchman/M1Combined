'use strict';

const delay = require('delay');
const testBoardLink = require('../../src/testBoardLink');
const exitCodes = require('../../src/exitCodes');
const { loadConfig, applyRuntime } = require('../commandSupport');

function normalizeState(state) {
    const value = String(state || '').trim().toLowerCase();
    if (value === 'on') return true;
    if (value === 'off' || value === 'auto') return false;
    return null;
}

function register(program) {
    program
        .command('poe')
        .description('Set POE supply on or off from the test fixture')
        .requiredOption('--state <on|off|auto>', 'POE state')
        .action(async options => {
            const configData = await loadConfig();
            const logfile = console;
            const poeOn = normalizeState(options.state);
            applyRuntime(configData);

            if (poeOn === null) {
                logfile.error(`Invalid POE state "${options.state}"`);
                process.exit(exitCodes.commandFailed);
            }

            try {
                await testBoardLink.initSerial(
                    configData.testBoardTerminalDev,
                    configData.serialBaudrate,
                    logfile
                );
                await testBoardLink.poeOn(poeOn);
                logfile.log(JSON.stringify({
                    status: 'OK',
                    errorCode: 0,
                    ErrorDescription: `POE ${poeOn ? 'on' : 'off'}`
                }));
                await delay(100);
                process.exit(exitCodes.normalExit);
            } catch (err) {
                logfile.error(`POE command failed: ${err.message}`);
                await delay(100);
                process.exit(exitCodes.commandFailed);
            }
        });
}

module.exports = {
    register
};
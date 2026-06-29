'use strict';

const delay = require('delay');
const logger = require('../utils/logger');
const sqliteDriver = require('../utils/sqliteDriver');
const exitCodes = require('../src/exitCodes');
const errorCodes = require('./errorCodes');
const { loadConfig, errorAndExit, applyRuntime } = require('./commandSupport');

/**
 * Standardizes the boilerplate required for command execution
 *
 * @param {Object} options - Commander options passed to the action
 * @param {string} commandName - The name of the command for logging (e.g. ' eeprom')
 * @param {string} defaultErrorCodeName - Default error code to set on unhandled exception
 * @param {Function} executionFn - Async function taking (configData, logfile, db)
 */
async function runCommand(options, commandName, defaultErrorCodeName, executionFn) {
    const configData = await loadConfig();
    let logfile;
    let db;
    // Some commands might not take `-s` or might be named differently
    const serial = options.serial || options.sn || '';

    // Some commands might not take a `-d`
    const debugLevel = options.debug || '0';

    applyRuntime(configData, { serial: serial, debugLevel: debugLevel });

    try {
        if (!serial && executionFn.requiresSerial !== false) {
            await errorAndExit('must define vendor serial number', console);
        }

        logfile = logger.getLogger(
            serial || 'UNKNOWN',
            commandName,
            serial || 'UNKNOWN',
            configData.mtfDir,
            debugLevel
        );

        db = sqliteDriver.initialize(logfile);

        await executionFn(configData, logfile, db);
    } catch (err) {
        if (!logfile) {
            logfile = console;
        }
        logfile.error(err);

        if (db && serial && errorCodes.codes[defaultErrorCodeName]) {
            db.updateErrorCode(serial, errorCodes.codes[defaultErrorCodeName].errorCode, 'E');
        }
        await delay(100);
        process.exit(exitCodes.commandFailed);
    }
}

module.exports = {
    runCommand
};

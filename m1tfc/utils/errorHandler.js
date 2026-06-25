'use strict';

const exitCodes = require('../src/exitCodes');

/**
 * Wraps an async command action with centralized error handling.
 * Ensures all unhandled errors are logged consistently and the process
 * exits with the correct exit code.
 * @param {Function} fn - Async command action to wrap
 * @param {object} [log=console] - Logger instance
 * @returns {Function} Wrapped action suitable for commander `.action()`
 */
function wrapAction(fn, log = console) {
    return async (...args) => {
        try {
            await fn(...args);
        } catch (err) {
            log.error(`Command failed: ${err.message}`);
            if (log.debug) {
                log.debug(err.stack);
            }
            process.exit(exitCodes.commandFailed);
        }
    };
}

/**
 * Handles uncaught exceptions and unhandled promise rejections globally.
 * Call once at application startup.
 * @param {object} [log=console] - Logger instance
 */
function registerGlobalHandlers(log = console) {
    process.on('uncaughtException', (err) => {
        log.error(`Uncaught exception: ${err.message}`);
        if (log.debug) log.debug(err.stack);
        process.exit(exitCodes.commandFailed);
    });

    process.on('unhandledRejection', (reason) => {
        const message = reason instanceof Error ? reason.message : String(reason);
        log.error(`Unhandled promise rejection: ${message}`);
        process.exit(exitCodes.commandFailed);
    });
}

module.exports = {
    wrapAction,
    registerGlobalHandlers
};

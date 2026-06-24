'use strict';

const errorCodes = require('../bin/errorCodes');
const runtimeContext = require('./runtimeContext');

/**
 * Unified command execution helper with consistent error handling
 * Handles both target board and test board commands
 */
class CommandHelper {
    constructor(logger, db) {
        this.logger = logger;
        this.db = db;
    }

    /**
     * Execute command and handle errors consistently
     * @param {Function} commandFn - Async function that executes the command
     * @param {string} commandDesc - Human-readable description
     * @param {string} errorCodeName - Error code key in errorCodes
     * @param {string} errorSeverity - 'T' (temp) or 'E' (error)
     * @returns {Object|boolean} - Command result or false if failed
     */
    async execute(commandFn, commandDesc, errorCodeName = null, errorSeverity = 'T') {
        try {
            const ret = await commandFn();
            
            if (!ret.status) {
                this.logger.error(`${commandDesc} failed: ${ret.error}`);
                if (errorCodeName && this.db) {
                    const errorCode = errorCodes.codes[errorCodeName]?.errorCode;
                    if (errorCode) {
                        this.db.updateErrorCode(runtimeContext.getRuntime().serial, errorCode, errorSeverity);
                    }
                }
                return false;
            }
            
            return ret;
        } catch (err) {
            this.logger.error(`${commandDesc} exception: ${err.message}`);
            if (errorCodeName && this.db) {
                const errorCode = errorCodes.codes[errorCodeName]?.errorCode;
                if (errorCode) {
                    this.db.updateErrorCode(runtimeContext.getRuntime().serial, errorCode, 'T');
                }
            }
            return false;
        }
    }

    /**
     * Execute test command (pass/fail pattern)
     * @param {Function} testFn - Async function that executes test
     * @param {string} testName - Test name for logging
     * @param {string} errorCodeName - Error code key
     * @returns {boolean} - Test passed
     */
    async executeTest(testFn, testName, errorCodeName = null) {
        try {
            const ret = await testFn();
            
            if (!ret.status) {
                this.logger.error(`Failed ${testName}: ${ret.error}`);
                if (errorCodeName && this.db) {
                    const errorCode = errorCodes.codes[errorCodeName]?.errorCode;
                    if (errorCode) {
                        this.db.updateErrorCode(runtimeContext.getRuntime().serial, errorCode, 'E');
                    }
                }
                return false;
            }
            
            this.logger.info(`Passed ${testName}`);
            return true;
        } catch (err) {
            this.logger.error(`${testName} exception: ${err.message}`);
            if (errorCodeName && this.db) {
                const errorCode = errorCodes.codes[errorCodeName]?.errorCode;
                if (errorCode) {
                    this.db.updateErrorCode(runtimeContext.getRuntime().serial, errorCode, 'T');
                }
            }
            return false;
        }
    }
}

module.exports = CommandHelper;

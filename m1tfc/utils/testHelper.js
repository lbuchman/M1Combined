'use strict';

/* eslint-disable no-await-in-loop */

const CommandHelper = require('./commandHelper');

/**
 * Test execution helper - centralizes common test patterns
 */
class TestHelper {
    constructor(logger, db) {
        this.logger = logger;
        this.db = db;
        this.cmdHelper = new CommandHelper(logger, db);
    }

    /**
     * Execute test with standardized pass/fail pattern
     * @param {Function} testFn - Test function that returns {status, error}
     * @param {string} testName - Name for logging
     * @param {string} errorCodeName - Error code key for failures
     * @returns {boolean} - Test passed
     */
    async runTest(testFn, testName, errorCodeName = null) {
        return await this.cmdHelper.executeTest(testFn, testName, errorCodeName);
    }

    /**
     * Run multiple tests in sequence, collecting results
     * @param {Array} tests - Array of {fn, name, errorCode}
     * @returns {Object} - {passed: count, failed: count, total: count}
     */
    async runMultiple(tests) {
        const results = {
            passed: 0,
            failed: 0,
            total: tests.length
        };

        for (const test of tests) {
            const success = await this.runTest(test.fn, test.name, test.errorCode);
            if (success) {
                results.passed++;
            } else {
                results.failed++;
            }
        }

        return results;
    }

    /**
     * Wrap test function with error handling and logging
     * @param {Function} fn - Test function
     * @param {string} testName - Test name
     * @returns {Function} - Wrapped function
     */
    wrapTest(fn, testName) {
        return async(...args) => {
            try {
                this.logger.info(`Starting ${testName}`);
                const result = await fn(...args);
                if (result) {
                    this.logger.info(`Completed ${testName}`);
                }
                return result;
            } catch (err) {
                this.logger.error(`${testName} failed with exception: ${err.message}`);
                return false;
            }
        };
    }

    /**
     * Run test with retry logic
     * @param {Function} testFn - Test function
     * @param {string} testName - Name for logging
     * @param {number} maxRetries - Maximum retry attempts
     * @param {number} delayMs - Delay between retries
     * @returns {boolean} - Test passed
     */
    async runWithRetry(testFn, testName, maxRetries = 3, delayMs = 1000) {
        let lastError = null;

        for (let attempt = 1; attempt <= maxRetries; attempt++) {
            try {
                this.logger.info(`${testName} attempt ${attempt}/${maxRetries}`);
                const result = await testFn();
                if (result) {
                    this.logger.info(`${testName} passed on attempt ${attempt}`);
                    return true;
                }
            } catch (err) {
                lastError = err;
            }

            if (attempt < maxRetries) {
                await new Promise(resolve => setTimeout(resolve, delayMs));
            }
        }

        this.logger.error(`${testName} failed after ${maxRetries} attempts`);
        if (lastError) {
            this.logger.error(`Last error: ${lastError.message}`);
        }
        return false;
    }

    /**
     * Create test result object
     * @param {boolean} passed - Test passed
     * @param {string} message - Optional message
     * @param {Object} data - Optional data
     * @returns {Object} - Result object
     */
    createResult(passed, message = '', data = {}) {
        return {
            passed,
            message,
            ...data
        };
    }
}

module.exports = TestHelper;

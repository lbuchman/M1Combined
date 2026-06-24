'use strict';

const errorCodes = require('../bin/errorCodes');
const runtimeContext = require('./runtimeContext');

const VOLTAGE_DECIMAL_PLACES = 2;

/**
 * Voltage testing helper - centralizes voltage reading, validation, calibration
 */
class VoltageHelper {
    constructor(testBoardLink, logger, db) {
        this.testBoardLink = testBoardLink;
        this.logger = logger;
        this.db = db;
    }

    /**
     * Read voltage from test point
     * @param {string} testPointName - Name of test point
     * @returns {number|false} - Voltage or false if failed
     */
    async readVoltage(testPointName) {
        try {
            const pinId = this.testBoardLink.findPinIdByName(testPointName);
            const ret = await this.testBoardLink.sendCommand(`getiopin ${pinId}`);
            
            if (!ret.status) {
                this.logger.error(`Failed to read voltage from ${testPointName}: ${ret.error}`);
                const errorCode = errorCodes.codes[testPointName]?.errorCode;
                if (errorCode && this.db) {
                    this.db.updateErrorCode(runtimeContext.getRuntime().serial, errorCode, 'T');
                }
                return false;
            }
            
            return ret.value;
        } catch (err) {
            this.logger.error(`Error reading ${testPointName}: ${err.message}`);
            return false;
        }
    }

    /**
     * Calculate voltage error percentage
     * @param {number} measuredValue - Measured voltage
     * @param {number} expectedValue - Expected voltage
     * @param {number} scale - Scale factor (default 1)
     * @returns {number} - Error as decimal (0-1)
     */
    calculateError(measuredValue, expectedValue, scale = 1) {
        const scaledMeasured = measuredValue * scale;
        return Math.abs(scaledMeasured - expectedValue) / expectedValue;
    }

    /**
     * Test voltage against tolerance
     * @param {Object} testPoint - {name, voltage, scale, tolerance}
     * @param {number} tolerance - Tolerance threshold
     * @param {boolean} calibrate - Calibration mode
     * @returns {Object|boolean} - {passed, error, scaledValue} or false if failed
     */
    async testVoltage(testPoint, tolerance, calibrate = false) {
        const measuredValue = await this.readVoltage(testPoint.name);
        if (measuredValue === false) return false;

        const pointTolerance = testPoint.tolerance || tolerance;
        const error = this.calculateError(measuredValue, testPoint.voltage, testPoint.scale);
        const scaledValue = measuredValue * testPoint.scale;

        if (error > pointTolerance) {
            if (calibrate) {
                this.logger.error(
                    `Voltage out of tolerance (cannot calibrate). TP=${testPoint.name}, ` +
                    `measured=${scaledValue.toFixed(VOLTAGE_DECIMAL_PLACES)}V, ` +
                    `expected=${testPoint.voltage.toFixed(VOLTAGE_DECIMAL_PLACES)}V, ` +
                    `error=${(error * 100).toFixed(1)}%`
                );
                return { passed: false, error, scaledValue };
            }

            const errorCode = errorCodes.codes[testPoint.name]?.errorCode;
            if (errorCode && this.db) {
                this.db.updateErrorCode(runtimeContext.getRuntime().serial, errorCode, 'E');
            }
            this.logger.error(
                `Failed: Voltage out of tolerance. TP=${testPoint.name}, ` +
                `measured=${scaledValue.toFixed(VOLTAGE_DECIMAL_PLACES)}V, ` +
                `expected=${testPoint.voltage.toFixed(VOLTAGE_DECIMAL_PLACES)}V`
            );
            return { passed: false, error, scaledValue };
        }

        this.logger.info(
            `Passed TP=${testPoint.name}. ` +
            `Measured=${scaledValue.toFixed(VOLTAGE_DECIMAL_PLACES)}V, ` +
            `Expected=${testPoint.voltage.toFixed(VOLTAGE_DECIMAL_PLACES)}V, ` +
            `Error=${(error * 100).toFixed(1)}%`
        );

        return { passed: true, error, scaledValue };
    }

    /**
     * Calibrate voltage scale factor
     * @param {Object} testPoint - Test point to calibrate
     * @param {number} expectedVoltage - Actual voltage measured manually
     * @returns {number} - New scale factor
     */
    calibrateScale(testPoint, expectedVoltage) {
        const newScale = expectedVoltage / testPoint.measuredRaw;
        this.logger.info(`Calibrated TP=${testPoint.name} scale=${newScale.toFixed(4)}`);
        return newScale;
    }

    /**
     * Format voltage for display
     * @param {number} voltage - Voltage value
     * @returns {string} - Formatted string
     */
    formatVoltage(voltage) {
        return `${voltage.toFixed(VOLTAGE_DECIMAL_PLACES)}V`;
    }
}

module.exports = VoltageHelper;

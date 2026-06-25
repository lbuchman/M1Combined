'use strict';

/* eslint-disable no-await-in-loop */

const testBoardLink = require('../src/testBoardLink');
const errorCodes = require('../bin/errorCodes');
const mnpHwIo = require('../tests/mnpHW');
const targetICTLink = require('../src/m1ICTLink');
const delay = require('delay');
const runtimeContext = require('../utils/runtimeContext');

const LEVERAGE_LOCK_VOLTAGE = { name: 'LeverSensor', voltage: 0, tolerance: 0.2 };
const DELAY_CAPACITOR_CHARGE_MS = 1000;
const VOLTAGE_DECIMAL_PLACES = 2;

/**
 * Voltage test context - holds configuration for current test session
 */
class VoltageTestContext {
    constructor(calibrationData) {
        this.runtime = runtimeContext.getRuntime();
        this.calibrationData = calibrationData;
        this.isM1Plus = this.runtime.productName === 'mnplus';
    }

    getTestPoints() {
        return this.isM1Plus
            ? this.calibrationData.testPointsMnp
            : this.calibrationData.testPointsM1;
    }

    getDdrVoltage() {
        return this.isM1Plus
            ? this.calibrationData.ddrVoltageMnp
            : this.calibrationData.ddrVoltageM1;
    }

    updateCalibrationData(property, value) {
        if (this.isM1Plus) {
            this.calibrationData.defaults[`${property}Mnp`] = value;
        } else {
            this.calibrationData.defaults[`${property}M1`] = value;
        }
    }
}

/**
 * Helper: Retrieve voltage from test point
 */
async function readVoltageFromTestPoint(testPointName) {
    const pinId = testBoardLink.findPinIdByName(testPointName);
    const result = await testBoardLink.sendCommand(`getiopin ${pinId}`);

    if (!result.status) {
        throw new Error(`Failed to read voltage from ${testPointName}: ${result.error}`);
    }

    return result.value;
}

/**
 * Helper: Calculate voltage error percentage
 */
function calculateVoltageError(measuredValue, expectedValue, scale = 1) {
    const scaledMeasured = measuredValue * scale;
    return Math.abs(scaledMeasured - expectedValue) / expectedValue;
}

/**
 * Initialize with calibration data (maintains backward compatibility)
 */
function init(calibrationParam) {
    // This function now just validates the calibration data
    // The actual context is created per test function for better isolation
    if (!calibrationParam) {
        throw new Error('Calibration parameters required');
    }
}

/**
 * Check if cover lever is locked (safety check before testing)
 */
async function checkLeverState(logger, db) {
    const runtime = runtimeContext.getRuntime();
    if (runtime.debugLevel === '2') {
        return true;
    }

    try {
        const leverVoltage = await readVoltageFromTestPoint(LEVERAGE_LOCK_VOLTAGE.name);
        const leverLocked = Math.abs(leverVoltage - LEVERAGE_LOCK_VOLTAGE.voltage) <= LEVERAGE_LOCK_VOLTAGE.tolerance;

        if (!leverLocked) {
            const errorCode = errorCodes.codes[LEVERAGE_LOCK_VOLTAGE.name]?.errorCode;
            if (errorCode) {
                db.updateErrorCode(runtime.serial, errorCode, 'T');
            }
            throw new Error('Failed: The Cover Lever is Not Locked!!!');
        }

        return true;
    } catch (error) {
        if (db) {
            const errorCode = errorCodes.codes[LEVERAGE_LOCK_VOLTAGE.name]?.errorCode;
            if (errorCode) {
                db.updateErrorCode(runtime.serial, errorCode, 'T');
            }
        }
        throw error;
    }
}

/**
 * Generic voltage test with optional calibration
 */
async function testVoltagePoint(testPoint, tolerance, context, logger, db, calibrate, calibrateData) {
    const runtime = context.runtime;
    const measuredVoltage = await readVoltageFromTestPoint(testPoint.name);
    const error = calculateVoltageError(measuredVoltage, testPoint.voltage, testPoint.scale);
    const pointTolerance = testPoint.tolerance || tolerance;

    if (error > pointTolerance) {
        const errorCode = errorCodes.codes[testPoint.name]?.errorCode;

        if (calibrate) {
            logger.error(
                `Voltage out of tolerance (cannot calibrate). TP=${testPoint.name}, ` +
                `measured=${(measuredVoltage * testPoint.scale).toFixed(VOLTAGE_DECIMAL_PLACES)}V, ` +
                `expected=${testPoint.voltage.toFixed(VOLTAGE_DECIMAL_PLACES)}V, ` +
                `error=${(error * 100).toFixed(1)}%`
            );
            return false;
        }

        if (errorCode) {
            db.updateErrorCode(runtime.serial, errorCode, 'E');
        }
        throw new Error(
            `Failed: Voltage out of tolerance. TP=${testPoint.name}, ` +
            `measured=${(measuredVoltage * testPoint.scale).toFixed(VOLTAGE_DECIMAL_PLACES)}V, ` +
            `expected=${testPoint.voltage.toFixed(VOLTAGE_DECIMAL_PLACES)}V`
        );
    }

    if (calibrate) {
        const newScale = testPoint.voltage / measuredVoltage;
        testPoint.scale = newScale;
        logger.info(`Calibrated TP=${testPoint.name} scale=${newScale.toFixed(4)}`);
        await calibrateData.saveConfigFile();
    } else {
        logger.info(
            `Passed TP=${testPoint.name}. ` +
            `Measured=${(measuredVoltage * testPoint.scale).toFixed(VOLTAGE_DECIMAL_PLACES)}V, ` +
            `Expected=${testPoint.voltage.toFixed(VOLTAGE_DECIMAL_PLACES)}V, ` +
            `Error=${(error * 100).toFixed(1)}%`
        );
    }

    return true;
}

/**
 * Test DDR voltage with calibration support
 */
async function testDDRVoltage(tolerance, logger, db, calibrate, calibrateData) {
    const context = new VoltageTestContext(calibrateData);
    const ddrVoltage = context.getDdrVoltage();
    const errorCode = errorCodes.codes[ddrVoltage.name]?.errorCode;

    try {
        return await testVoltagePoint(ddrVoltage, tolerance, context, logger, db, calibrate, calibrateData);
    } catch (error) {
        if (errorCode && db) {
            db.updateErrorCode(context.runtime.serial, errorCode, 'T');
        }
        throw error;
    }
}

/**
 * Test coin cell battery voltage (age-aware: new vs used)
 */
async function cellBatTest(logger, db, calibrate, calibrateData) {
    const context = new VoltageTestContext(calibrateData);
    const runtime = context.runtime;

    if (runtime.skipBatteryTest) {
        return true;
    }

    const batConfig = calibrateData.coinCellBattery;
    const minVoltage = runtime.cellBatTol === 'used'
        ? calibrateData.defaults.coinCellBattery.minVoltageAged
        : calibrateData.defaults.coinCellBattery.minVoltageNew;

    try {
        const measuredVoltage = await readVoltageFromTestPoint(batConfig.name);
        const scaledVoltage = measuredVoltage * batConfig.scale;

        if (calibrate) {
            if (!runtime.cellBatVoltage) {
                throw new Error('Calibration requires voltage flag: -v voltage');
            }
            batConfig.scale = runtime.cellBatVoltage / measuredVoltage;
            logger.info(`Calibrated coin cell battery scale=${batConfig.scale.toFixed(4)}`);
            await calibrateData.saveConfigFile();
            return true;
        }

        if (scaledVoltage < minVoltage) {
            const errorCode = errorCodes.codes[batConfig.name]?.errorCode;
            if (errorCode) {
                db.updateErrorCode(runtime.serial, errorCode, 'E');
            }
            throw new Error(
                'Coin cell battery voltage below minimum. ' +
                `Measured=${scaledVoltage.toFixed(VOLTAGE_DECIMAL_PLACES)}V, ` +
                `Minimum=${minVoltage.toFixed(VOLTAGE_DECIMAL_PLACES)}V`
            );
        }

        logger.info(
            'Passed coin cell battery test. ' +
            `Measured=${scaledVoltage.toFixed(VOLTAGE_DECIMAL_PLACES)}V, ` +
            `Minimum=${minVoltage.toFixed(VOLTAGE_DECIMAL_PLACES)}V`
        );
        return true;
    } catch (error) {
        const errorCode = errorCodes.codes[batConfig.name]?.errorCode;
        if (errorCode && db) {
            db.updateErrorCode(runtime.serial, errorCode, 'T');
        }
        throw error;
    }
}

/**
 * Helper: Enable capacitor charging for strike regulator
 */
async function enableCapCharging(testPoint, logger) {
    const command = mnpHwIo.getCommand('write', testPoint.functnameAux, 1, logger);
    return await targetICTLink.sendCommand(command);
}

/**
 * Helper: Disable capacitor charging for strike regulator
 */
async function disableCapCharging(testPoint, logger) {
    const command = mnpHwIo.getCommand('write', testPoint.functnameAux, 0, logger);
    return await targetICTLink.sendCommand(command);
}

/**
 * Helper: Trigger kicker relay
 */
async function triggerKicker(testPoint, logger) {
    const command = mnpHwIo.getCommand('write', testPoint.funcName, 1, logger);
    return await targetICTLink.sendCommand(command);
}

/**
 * Test strike boost regulators (high voltage output)
 */
async function strikeBoostReg(tolerance, logger, db, calibrate, calibrateData) {
    const context = new VoltageTestContext(calibrateData);
    const runtime = context.runtime;
    let allTestsPassed = true;

    for (const testPoint of calibrateData.strikeReg) {
        try {
            logger.info(`Testing ${testPoint.name} (enabling ${testPoint.funcName})`);

            // Enable capacitor charging
            await enableCapCharging(testPoint, logger);
            await delay(DELAY_CAPACITOR_CHARGE_MS);

            // Trigger kicker
            await triggerKicker(testPoint, logger);

            // Read voltage
            await readVoltageFromTestPoint(testPoint.name);
            await disableCapCharging(testPoint, logger);

            // Test voltage
            const testPassed = await testVoltagePoint(testPoint, tolerance, context, logger, db, calibrate, calibrateData);
            if (!testPassed) {
                allTestsPassed = false;
            }
        } catch (error) {
            logger.error(`Strike regulator test failed for ${testPoint.name}: ${error.message}`);
            const errorCode = errorCodes.codes[testPoint.name]?.errorCode;
            if (errorCode) {
                db.updateErrorCode(runtime.serial, errorCode, 'T');
            }
            allTestsPassed = false;

            // Attempt to disable capacitor charging on error
            try {
                await disableCapCharging(testPoint, logger);
            } catch (disableError) {
                logger.error(`Failed to disable capacitor charging: ${disableError.message}`);
            }
        }
    }

    return allTestsPassed;
}


/**
 * Test all configured voltage test points
 */
async function test(tolerance, logger, db, calibrate, calibrateData) {
    const context = new VoltageTestContext(calibrateData);
    const testPoints = context.getTestPoints();

    let allTestsPassed = true;

    try {
        // Enable POE if MNPlus variant
        if (context.isM1Plus) {
            await testBoardLink.poeOn(true);
        }

        for (const testPoint of testPoints) {
            try {
                const testPassed = await testVoltagePoint(testPoint, tolerance, context, logger, db, calibrate, calibrateData);
                if (!testPassed) {
                    allTestsPassed = false;
                }
            } catch (error) {
                logger.error(`Test point ${testPoint.name} failed: ${error.message}`);
                allTestsPassed = false;
            }
        }

        return allTestsPassed;
    } finally {
        // Disable POE if MNPlus variant
        if (context.isM1Plus) {
            try {
                await testBoardLink.poeOn(false);
            } catch (error) {
                logger.error(`Failed to disable POE: ${error.message}`);
            }
        }
    }
}

module.exports = {
    test,
    testDDRVoltage,
    cellBatTest,
    checkLeverState,
    init,
    strikeBoostReg
};

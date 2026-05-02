'use strict';

const testBoardLink = require('../src/testBoardLink');
const errorCodes = require('../bin/errorCodes');
const mnpHwIo = require('../tests/mnpHW');
const targetICTLink = require('../src/m1ICTLink');
const delay = require('delay');

const leverLockVoltage = { name: 'LeverSensor', voltage: 0 };

let testPoints;
let ddrVoltage;


function init(calibrationParam) {
    if (process.env.productName === 'mnplus') {
        testPoints = calibrationParam.testPointsMnp;
        ddrVoltage = calibrationParam.ddrVoltageMnp;
        return;
    }
    testPoints = calibrationParam.testPointsM1;
    ddrVoltage = calibrationParam.ddrVoltageM1;
}

async function checkLeverState(logger, db) {
    if (process.env.debug === '2') return true;
    const ret = await testBoardLink.sendCommand(`getiopin ${testBoardLink.findPinIdByName(leverLockVoltage.name)}`);
    if (!ret.status) {
        throw new Error(`Test Board control command failed on pinName=${leverLockVoltage.name}, ${ret.error}`);
    }
    if ((Math.abs(ret.value - leverLockVoltage.voltage)) > 0.2) {
        db.updateErrorCode(process.env.serial, errorCodes.codes[leverLockVoltage.name].errorCode, 'T');
        throw new Error('Failed: The Cover Lever is Not Locked!!!');
    }

    return true;
}

async function testDDRVoltage(tolerance, logger, db, calibrate, calibrateData) {
    const ret = await testBoardLink.sendCommand(`getiopin ${testBoardLink.findPinIdByName(ddrVoltage.name)}`);
    if (!ret.status) {
        db.updateErrorCode(process.env.serial, errorCodes.codes[ddrVoltage.name].errorCode, 'T');
        throw new Error(`Test Board control command failed on pinName=${ddrVoltage.name}, ${ret.error}`);
    }
    const error = ((Math.abs(ret.value * ddrVoltage.scale - ddrVoltage.voltage)) / (ddrVoltage.voltage));
    if (error > tolerance) {
        if (calibrate) {
            logger.error(`Voltage is out of tolerance and cannot be calibrated. Check A/D HW, TP=${ddrVoltage.voltage}, value=${(ret.value - ddrVoltage.voltage).toFixed(2)}, reqValue=${calibrateData.ddrVoltage.voltage} Error = ${(error * 100).toFixed(1)}%`);
            return false;
        }
        db.updateErrorCode(process.env.serial, errorCodes.codes[ddrVoltage.name].errorCode, 'E');
        throw new Error(`Failed: Voltage is out of tolerance, TP=${ddrVoltage.name}, value=${(ret.value * ddrVoltage.scale).toFixed(2)}, reqValue=${ddrVoltage.voltage.toFixed(2)} Error = ${(error * 200).toFixed(2)}%`);
    }
    else {
        if (calibrate) {
            if (process.env.productName === 'mnplus') {
                // eslint-disable-next-line no-param-reassign
                calibrateData.defaults.ddrVoltageMnp.scale = ddrVoltage.voltage / ret.value;
            }
            else {
                // eslint-disable-next-line no-param-reassign
                calibrateData.defaults.ddrVoltageMnp.scale = ddrVoltage.voltage / ret.value;
            }

            if (process.env.productName === 'mnplus') {
                // eslint-disable-next-line no-param-reassign
                calibrateData.defaults.ddrVoltageMnp = ddrVoltage;
            }
            else {
                // eslint-disable-next-line no-param-reassign
                calibrateData.defaults.ddrVoltageM1 = ddrVoltage;
            }
            logger.info(`calibrating TP=${ddrVoltage.name} scale to value=${ddrVoltage.scale}`);
            await calibrateData.saveConfigFile();
            // eslint-disable-next-line no-continue
            return true;
        }
        logger.info(`Passed TP=${ddrVoltage.name} test, Voltage = ${(ret.value * ddrVoltage.scale).toFixed(2)}V, Expected = ${ddrVoltage.voltage.toFixed(2)}V Tolerance = ${(error * 100).toFixed(1)}%`);
    }
    return true;
}

async function cellBatTest(logger, db, calibrate, calibrateData) {
    if (process.env.skipBatteryTest === 'true') {
        return true;
    }
    let minVoltage = calibrateData.defaults.coinCellBattery.minVoltageNew;
    if (process.env.cellBatTol === 'used') {
        minVoltage = calibrateData.defaults.coinCellBattery.minVoltageAged;
    }

    const ret = await testBoardLink.sendCommand(`getiopin ${testBoardLink.findPinIdByName(calibrateData.coinCellBattery.name)}`);
    if (!ret.status) {
        db.updateErrorCode(process.env.serial, errorCodes.codes[calibrateData.defaults.coinCellBattery.name].errorCode, 'T');
        throw new Error(`Test Board control command failed on pinName=${calibrateData.defaults.coinCellBattery.name}, ${ret.error}`);
    }
    if (calibrate) {
        if (!process.env.cellBatVoltage) {
            throw new Error('Failed: You MUST specify voltage on coin cell battery at this time with -v voltage flag');
        }
        // eslint-disable-next-line no-param-reassign
        calibrateData.defaults.coinCellBattery.scale = process.env.cellBatVoltage / ret.value;
        // eslint-disable-next-line no-param-reassign
        calibrateData.defaults.coinCellBattery = calibrateData.coinCellBattery;
        logger.info(`calibrating coin cell battery scale to value=${calibrateData.defaults.coinCellBattery.scale}`);
        await calibrateData.saveConfigFile();
        // eslint-disable-next-line no-continue
        return true;
    }

    if (ret.value * calibrateData.coinCellBattery.scale < minVoltage) {
        db.updateErrorCode(process.env.serial, errorCodes.codes[calibrateData.coinCellBattery.name].errorCode, 'E');
        throw new Error(`Failed: Coin cell battery voltage is not in the range. actual: ${ret.value}V, req: ${minVoltage}V`);
    }
    else {
        logger.info(`Passed coin cell battery test, Actual = ${(ret.value * calibrateData.coinCellBattery.scale).toFixed(1)}V, Expected Minimum: ${minVoltage}V`);
    }
    return true;
}

async function strikeBoostReg(tolerance, logger, db, calibrate, calibrateData) {
    // Test strike regulators
    let retValue = true;
    let command;
    let ret;
    /* eslint-disable-next-line no-restricted-syntax */
    for (const testPoint of calibrateData.strikeReg) {
        logger.info(`Enabling ${testPoint.funcName}`);


        command = mnpHwIo.getCommand('write', testPoint.functnameAux, 1, logger); // enable Cap charging command
        // eslint-disable-next-line no-await-in-loop
        ret = await targetICTLink.sendCommand(command);
        // eslint-disable-next-line no-await-in-loop
        await delay(1000); // Wait for 1 second
        command = mnpHwIo.getCommand('write', testPoint.funcName, 1, logger); // relay kicker
        // eslint-disable-next-line no-await-in-loop
        ret = await targetICTLink.sendCommand(command);
        if (!ret.status) {
            logger.error(`Target Board control command failed on pin ${testPoint.funcName}, ${ret.error}`);
            retValue = false;
            db.updateErrorCode(process.env.serial, errorCodes.codes[testPoint.name].errorCode, 'T');
        }
        // eslint-disable-next-line no-await-in-loop
        ret = await testBoardLink.sendCommand(`getiopin ${testBoardLink.findPinIdByName(testPoint.name)}`);
        if (!ret.status) {
            command = mnpHwIo.getCommand('write', testPoint.functnameAux, 0, logger); // enable Cap charging command
            // eslint-disable-next-line no-await-in-loop
            await targetICTLink.sendCommand(command);
            db.updateErrorCode(process.env.serial, errorCodes.codes[testPoint.name].errorCode, 'T');
            throw new Error(`Test Board control command failed on pinName=${testPoint.name}, ${ret.error}`);
        }
        if (!testPoint.tolerance) testPoint.tolerance = tolerance;
        const error = Math.abs((ret.value * testPoint.scale - testPoint.voltage) / (testPoint.voltage));
        if (error > testPoint.tolerance) {
            if (calibrate) {
                logger.error(`Voltage is out of tolerance and cannot be calibrated. Check A/D HW, TP=${testPoint.name}, value=${(ret.value * testPoint.scale).toFixed(2)}, reqValue=${testPoint.voltage.toFixed(2)} Error = ${(error * 100).toFixed(1)}%`);
                retValue = false;
                // eslint-disable-next-line no-continue
                continue;
            }
            else {
                db.updateErrorCode(process.env.serial, errorCodes.codes[testPoint.name].errorCode, 'E');
                logger.error(`Failed: Voltage is out of tolerance, TP=${testPoint.name}, value=${(ret.value * testPoint.scale).toFixed(2)}, reqValue=${testPoint.voltage.toFixed(2)} Error = ${(error * 100).toFixed(1)}%`);
                retValue = false;
            }
        }
        else {
            if (calibrate) {
                testPoint.scale = testPoint.voltage / ret.value;
                // eslint-disable-next-line no-param-reassign
                calibrateData.defaults.strikeReg = calibrateData.strikeReg;
                logger.info(`calibrating TP=${testPoint.name} scale to value=${testPoint.scale}`);
                // eslint-disable-next-line no-await-in-loop
                await calibrateData.saveConfigFile();
                // eslint-disable-next-line no-continue
                continue;
            }
            logger.info(`Passed TP=${testPoint.name} test, Voltage = ${(ret.value * testPoint.scale).toFixed(2)}V, Expected = ${testPoint.voltage.toFixed(2)}V Tolerance = ${(error * 100).toFixed(1)}%`);
        }
        command = mnpHwIo.getCommand('write', testPoint.functnameAux, 0, logger); // enable Cap charging command
    }
    return retValue;
}


async function test(tolerance, logger, db, calibrate, calibrateData) {
    let retValue = true;
    // eslint-disable-next-line no-restricted-syntax
    if (process.env.productName === 'mnplus') await testBoardLink.poeOn(true);
    /* eslint-disable-next-line no-restricted-syntax */
    for (const testPoint of testPoints) {
        if (process.env.cellBatTol === 'new') {
            testPoints.voltage = 3.3;
        }
        else {
            testPoints.voltage = 2.9;
        }
        // eslint-disable-next-line no-await-in-loop
        const ret = await testBoardLink.sendCommand(`getiopin ${testBoardLink.findPinIdByName(testPoint.name)}`);
        if (!ret.status) {
            db.updateErrorCode(process.env.serial, errorCodes.codes[testPoint.name].errorCode, 'T');
            throw new Error(`Test Board control command failed on pinName=${testPoint.name}, ${ret.error}`);
        }
        if (!testPoint.tolerance) testPoint.tolerance = tolerance;
        const error = Math.abs((ret.value * testPoint.scale - testPoint.voltage) / (testPoint.voltage));

        if (error > testPoint.tolerance) {
            if (calibrate) {
                logger.error(`Voltage is out of tolerance and cannot be calibrated. Check A/D HW, TP=${testPoint.name}, value=${(ret.value * testPoint.scale).toFixed(2)}, reqValue=${testPoint.voltage.toFixed(2)} Error = ${(error * 100).toFixed(1)}%`);
                retValue = false;
                // eslint-disable-next-line no-continue
                continue;
            }
            else {
                db.updateErrorCode(process.env.serial, errorCodes.codes[testPoint.name].errorCode, 'E');
                logger.error(`Failed: Voltage is out of tolerance, TP=${testPoint.name}, value=${(ret.value * testPoint.scale).toFixed(2)}, reqValue=${testPoint.voltage.toFixed(2)} Error = ${(error * 100).toFixed(1)}%`);
                retValue = false;
            }
        }
        else {
            if (calibrate) {
                testPoint.scale = testPoint.voltage / ret.value;
                if (process.env.productName === 'mnplus') {
                    // eslint-disable-next-line no-param-reassign
                    calibrateData.defaults.testPointsMnp = testPoints;
                }
                else {
                    // eslint-disable-next-line no-param-reassign
                    calibrateData.defaults.testPointsM1 = testPoints;
                }
                logger.info(`calibrating TP=${testPoint.name} scale to value=${testPoint.scale}`);
                // eslint-disable-next-line no-await-in-loop
                await calibrateData.saveConfigFile();

                // eslint-disable-next-line no-continue
                continue;
            }
            else {
                logger.info(`Passed TP=${testPoint.name} test, Voltage = ${(ret.value * testPoint.scale).toFixed(2)}V, Expected = ${testPoint.voltage.toFixed(2)}V Tolerance = ${(error * 100).toFixed(1)}%`);
            }
        }
    }
    if (process.env.productName === 'mnplus') await testBoardLink.poeOn(false);
    return retValue;
}

module.exports = {
    test,
    testDDRVoltage,
    cellBatTest,
    checkLeverState,
    init,
    strikeBoostReg
};

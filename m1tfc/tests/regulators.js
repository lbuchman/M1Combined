'use strict';

const testBoardLink = require('../src/testBoardLink');
const errorCodes = require('../bin/errorCodes');
const mnpHwIo = require('../tests/mnpHW');
const targetICTLink = require('../src/m1ICTLink');

const ddrVoltageMnp = { name: 'TP304', voltage: 1.35 };
const ddrVoltageM1 = { name: 'TP31', voltage: 1.35 };

const leverLockVoltage = { name: 'LeverSensor', voltage: 0 };
const testPointsM1 = [
    { name: 'TP025', voltage: 5, scale: 1 },
    { name: 'TP33', voltage: 2.8, scale: 1 },
    { name: 'TP35', voltage: 3.3, scale: 1 },
    { name: 'TP34', voltage: 3.3, scale: 1 },
    { name: 'TP36', voltage: 1.2, scale: 1 },
    { name: 'J5.13', voltage: 11.7, scale: 1 },
    { name: 'J5.5', voltage: 6.0, scale: 1 },
    { name: 'J5.7', voltage: 6.0, scale: 1 },
    { name: 'J5.8', voltage: 6.0, scale: 1 }
];

const testPointsMnp = [
    { name: 'TP204', voltage: 5.0, scale: 1 },
    { name: 'TP308', voltage: 2.8, scale: 1 },
    { name: 'TP303', voltage: 1.2, scale: 1 },
    { name: 'TP305', voltage: 3.3, scale: 1 },
    { name: 'TP306', voltage: 3.3, scale: 1 },
    { name: 'TP401', voltage: 5.0, scale: 0.98 },
    { name: 'TP2301', voltage: 12.8, scale: 1 },
    { name: 'TP202', voltage: 12.0, scale: 0.995 },
    { name: 'J2101.1', voltage: 11.85, scale: 2.78 },
    { name: 'J2001.1', voltage: 11.85, scale: 2.78 }
];

const strikeReg = [
    { name: 'SW1601.6', funcName: 'STRIKE1_KICKER_EN', voltage: 28.3, scale: 5.42 },
    { name: 'SW1602.6', funcName: 'STRIKE2_KICKER_EN', voltage: 28.3, scale: 5.42 }
];


let testPoints;
let ddrVoltage;

const coinCellBattery = { name: 'BatCellBat', minVoltage: 3.1, maxVoltage: 3.7 };


function init() {
    if (process.env.productName === 'mnplus') {
        testPoints = testPointsMnp;
        ddrVoltage = ddrVoltageMnp;
        return;
    }
    testPoints = testPointsM1;
    ddrVoltage = ddrVoltageM1;
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

async function testDDRVoltage(tolerance, logger, db) {
    const ret = await testBoardLink.sendCommand(`getiopin ${testBoardLink.findPinIdByName(ddrVoltage.name)}`);
    if (!ret.status) {
        db.updateErrorCode(process.env.serial, errorCodes.codes[ddrVoltage.name].errorCode, 'T');
        throw new Error(`Test Board control command failed on pinName=${ddrVoltage.name}, ${ret.error}`);
    }
    if (((Math.abs(ret.value - ddrVoltage.voltage)) / (ddrVoltage.voltage)) > tolerance) {
        db.updateErrorCode(process.env.serial, errorCodes.codes[ddrVoltage.name].errorCode, 'E');
        throw new Error(`Failed: Voltage is out of tolerance, TP=${ddrVoltage.name}, value=${ret.value.toFixed(2)}, reqValue=${ddrVoltage.voltage.toFixed(2)}`);
    }
    else {
        logger.info(`Passed TP=${ddrVoltage.name} test, Voltage = ${ret.value.toFixed(2)}V, Expected = ${ddrVoltage.voltage.toFixed(2)}V`);
    }
    return true;
}

async function cellBatTest(logger, db) {
    if (process.env.skipBatteryTest) {
        return true;
    }
    if (process.env.cellBatTol === 'used') {
        coinCellBattery.minVoltage = 2.75;
    }
    if (process.env.coinCellDebug) {
        coinCellBattery.minVoltage = 2.9;
    }

    logger.info(`Minimum Coin cell battery voltage expected ${coinCellBattery.minVoltage}V`);
    const ret = await testBoardLink.sendCommand(`getiopin ${testBoardLink.findPinIdByName(coinCellBattery.name)}`);
    if (!ret.status) {
        db.updateErrorCode(process.env.serial, errorCodes.codes[coinCellBattery.name].errorCode, 'T');
        throw new Error(`Test Board control command failed on pinName=${coinCellBattery.name}, ${ret.error}`);
    }
    coinCellBattery.tolerance = 0.05;
    if ((ret.value < coinCellBattery.minVoltage) || (ret.value > coinCellBattery.maxVoltage)) {
        db.updateErrorCode(process.env.serial, errorCodes.codes[coinCellBattery.name].errorCode, 'E');
        throw new Error(`Failed: Coin cell battery voltage is not in the range. actual: ${ret.value}V, req: ${coinCellBattery.minVoltage}V - 3.3V`);
    }
    else {
        logger.info(`Passed coin cell battery test, Actual = ${ret.value}V, Expected: ${coinCellBattery.minVoltage}V - 3.3V`);
    }
    return true;
}

async function strikeBoostReg(tolerance, logger, db) {
    // Test strike regulators
    let retValue = true;
    let command;
    let ret;
    /* eslint-disable-next-line no-restricted-syntax */
    for (const testPoint of strikeReg) {
        logger.info(`Enabling ${testPoint.funcName}`);
        command = mnpHwIo.getCommand('write', testPoint.funcName, 1, logger);
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
            db.updateErrorCode(process.env.serial, errorCodes.codes[testPoint.name].errorCode, 'T');
            throw new Error(`Test Board control command failed on pinName=${testPoint.name}, ${ret.error}`);
        }
        if (!testPoint.tolerance) testPoint.tolerance = tolerance;
        if (((Math.abs(ret.value * testPoint.scale - testPoint.voltage)) / (testPoint.voltage)) > testPoint.tolerance) {
            db.updateErrorCode(process.env.serial, errorCodes.codes[testPoint.name].errorCode, 'E');
            logger.error(`Failed: Voltage is out of tolerance, TP=${testPoint.name}, value=${(ret.value * testPoint.scale).toFixed(2)}, reqValue=${testPoint.voltage.toFixed(2)}`);
            retValue = false;
        }
        else {
            logger.info(`Passed TP=${testPoint.name} test, Voltage = ${(ret.value * testPoint.scale).toFixed(2)}V, Expected = ${testPoint.voltage.toFixed(2)}V`);
        }
    }
    return retValue;
}


async function test(tolerance, logger, db) {
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
        if (((Math.abs(ret.value * testPoint.scale - testPoint.voltage)) / (testPoint.voltage)) > testPoint.tolerance) {
            db.updateErrorCode(process.env.serial, errorCodes.codes[testPoint.name].errorCode, 'E');
            logger.error(`Failed: Voltage is out of tolerance, TP=${testPoint.name}, value=${(ret.value * testPoint.scale).toFixed(2)}, reqValue=${testPoint.voltage.toFixed(2)}`);
            retValue = false;
        }
        else {
            logger.info(`Passed TP=${testPoint.name} test, Voltage = ${(ret.value * testPoint.scale).toFixed(2)}V, Expected = ${testPoint.voltage.toFixed(2)}V`);
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

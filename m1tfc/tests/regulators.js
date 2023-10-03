'use strict';

const testBoardLink = require('../src/testBoardLink');
const errorCodes = require('../bin/errorcodes');

const ddrVoltage = { name: 'TP31', voltage: 1.35 };

const testPoints = [
    { name: 'TP025', voltage: 5 },
    { name: 'TP33', voltage: 2.8 },
    { name: 'TP35', voltage: 3.3 },
    { name: 'TP34', voltage: 3.3 },
    { name: 'TP36', voltage: 1.2 },
    { name: 'J5.13', voltage: 11.7 },
    { name: 'J5.8', voltage: 6.0 }
];

const coinCellBattery = { name: 'BatCellBat', minVoltage: 3.1, maxVoltage: 3.7 };

async function testDDRVoltage(tolerance, logger) {
    const ret = await testBoardLink.sendCommand(`getiopin ${testBoardLink.findPinIdByName(ddrVoltage.name)}`);
    if (!ret.status) {
        throw new Error(`Test Board control command failed on pinName=${ddrVoltage.name}, ${ret.error}`);
    }
    if (((Math.abs(ret.value - ddrVoltage.voltage)) / (ddrVoltage.voltage)) > tolerance) {
        throw new Error(`Failed: Voltage is out of tolerance, TP=${ddrVoltage.name}, value=${ret.value}, reqValue=${ddrVoltage.voltage}`);
    }
    else {
        logger.info(`Passed TP=${ddrVoltage.name} test, Voltage = ${ret.value}V, Expected = ${ddrVoltage.voltage}V`);
    }
    return true;
}


async function cellBatTest(logger) {
    if (process.env.cellBatTol === 'used') {
        coinCellBattery.minVoltage = 2.75;
    }
    if (process.env.coinCellDebug) {
        coinCellBattery.minVoltage = 2.9;
    }

    logger.info(`Minimum Coin cell battery voltage expected ${coinCellBattery.minVoltage}V`);
    const ret = await testBoardLink.sendCommand(`getiopin ${testBoardLink.findPinIdByName(coinCellBattery.name)}`);
    if (!ret.status) {
        throw new Error(`Test Board control command failed on pinName=${coinCellBattery.name}, ${ret.error}`);
    }
    coinCellBattery.tolerance = 0.05;
    if ((ret.value < coinCellBattery.minVoltage) || (ret.value > coinCellBattery.maxVoltage)) {
        throw new Error(`Failed: Coin cell battery voltage is not in the range. actual: ${ret.value}V, req: ${coinCellBattery.minVoltage}V - 3.3V`);
    }
    else {
        logger.info(`Passed coin cell battery test, Actual = ${ret.value}V, Expected: ${coinCellBattery.minVoltage}V - 3.3V`);
    }
    return true;
}

async function test(tolerance, logger) {
    // eslint-disable-next-line no-restricted-syntax
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
            throw new Error(`Test Board control command failed on pinName=${testPoint.name}, ${ret.error}`);
        }
        if (!testPoint.tolerance) testPoint.tolerance = tolerance;
        if (((Math.abs(ret.value - testPoint.voltage)) / (testPoint.voltage)) > testPoint.tolerance) {
            throw new Error(`Failed: Voltage is out of tolerance, TP=${testPoint.name}, value=${ret.value}, reqValue=${testPoint.voltage}`);
        }
        else {
            logger.info(`Passed TP=${testPoint.name} test, Voltage = ${ret.value}V, Expected = ${testPoint.voltage}V`);
        }
    }
    return true;
}

module.exports = {
    test,
    testDDRVoltage,
    cellBatTest
};

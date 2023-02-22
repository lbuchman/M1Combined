
'use strict';

/* eslint-disable no-await-in-loop */

const testBoardLink = require('../src/testBoardLink');
const targetICTLink = require('../src/m1ICTLink');
const delay = require('delay');
const exitCodes = require('../src/exitCodes');

const tamperSensor = {
    port: 'k',
    pin: 2,
    pinNameOnTestBoard: 'TamperPin',
    activated: 1,
    deactivated: 0
};


async function getSensorState(fromNumber, expectedValue, logger) {
    await delay(500);
    let ret = await targetICTLink.sendCommand(`getgpio ${tamperSensor.port} ${tamperSensor.pin}`);
    if (!ret.status) {
        throw new Error(`Target Board control command <getgpio> failed on pin ${tamperSensor.port}.${tamperSensor.pin} ${ret.error}`);
    }

    if (ret.value === expectedValue) {
        return ret;
    }
    await delay(100);
    const nextNumber = fromNumber - 1;
    if (nextNumber > 0) {
        ret = await getSensorState(nextNumber, expectedValue, logger);
        if (ret.value !== expectedValue) {
            return ret;
        }
    }
    return ret;
}

async function activatetamper() {
    const ret = await testBoardLink.sendCommand(`setiopin ${testBoardLink.findPinIdByName(tamperSensor.pinNameOnTestBoard)} 0`);
    if (!ret.status) {
        throw new Error(`Test Board control command failed on pinName=${tamperSensor.pinNameOnTestBoard}, ${ret.error}`);
    }
}

async function deactivatetamper() {
    const ret = await testBoardLink.sendCommand(`setiopin ${testBoardLink.findPinIdByName(tamperSensor.pinNameOnTestBoard)} 1`);
    if (!ret.status) {
        throw new Error(`Test Board control command failed on pinName=${tamperSensor.pinNameOnTestBoard}, ${ret.error}`);
    }
}

async function initSensor() {
    const ret = await targetICTLink.sendCommand(`confgpio ${tamperSensor.port} ${tamperSensor.pin} input none`);
    if (!ret.status) {
        throw new Error(`Target Board control command <confgpio> failed on pin ${tamperSensor.port}.${tamperSensor.pin} ${ret.error}`);
    }
}

async function test(logger) {
    try {
        await initSensor(logger);
        let sensorState;
        await deactivatetamper();
        await delay(100);
        sensorState = await getSensorState(10, tamperSensor.deactivated, logger);
        if (await sensorState.value !== tamperSensor.deactivated) {
            logger.error(`tamper sensor test failed, invalid sensor state detected, expected state  ${tamperSensor.deactivated}`);
            return false;
        }
        await activatetamper();
        await delay(100);
        sensorState = await getSensorState(10, tamperSensor.activated, logger);
        if (await sensorState.value !== tamperSensor.activated) {
            logger.error(`tamper sensor test failed, invalid sensor state detected, expected state  ${tamperSensor.activated}`);
            process.exit(exitCodes.tamperSensorTestFailed);
            return false;
        }
        logger.info('Passed Tamper test');
    }
    catch (err) {
        logger.error('Failed Led test');
        logger.error(err);
        // logger.debug(err.stack);
        return false;
    }

    return true;
}

module.exports = {
    test
};

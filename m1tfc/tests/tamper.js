'use strict';

/* eslint-disable no-await-in-loop */

const testBoardLink = require('../src/testBoardLink');
const targetICTLink = require('../src/m1ICTLink');
const delay = require('delay');
const errorCodes = require('../bin/errorCodes');
const runtimeContext = require('../utils/runtimeContext');
const GPIOHelper = require('../utils/gpioHelper');

const tamperSensor = {
    port: 'k',
    pin: 2,
    pinNameOnTestBoard: 'TamperPin',
    activated: 1,
    deactivated: 0
};

let gpioHelper;

async function getSensorState(fromNumber, expectedValue, logger, db) {
    await delay(500);
    const ret = await gpioHelper.readLevel(
        tamperSensor.port,
        tamperSensor.pin,
        tamperSensor.pinNameOnTestBoard
    );
    if (ret === false) {
        throw new Error(`Failed to read GPIO ${tamperSensor.port}.${tamperSensor.pin}`);
    }

    if (ret === expectedValue) {
        return { value: ret, status: true };
    }

    await delay(100);
    const nextNumber = fromNumber - 1;
    if (nextNumber > 0) {
        const nextRet = await getSensorState(nextNumber, expectedValue, logger, db);
        if (nextRet.value !== expectedValue) {
            db.updateErrorCode(
                runtimeContext.getRuntime().serial,
                errorCodes.codes[tamperSensor.pinNameOnTestBoard].errorCode,
                'E'
            );
            return nextRet;
        }
        return nextRet;
    }

    return { value: ret, status: false };
}

async function activatetamper() {
    const ret = await testBoardLink.sendCommand(
        `setiopin ${testBoardLink.findPinIdByName(tamperSensor.pinNameOnTestBoard)} 0`
    );
    if (!ret.status) {
        throw new Error(
            `Test Board control command failed on ${tamperSensor.pinNameOnTestBoard}: ${ret.error}`
        );
    }
}

async function deactivatetamper() {
    const ret = await testBoardLink.sendCommand(
        `setiopin ${testBoardLink.findPinIdByName(tamperSensor.pinNameOnTestBoard)} 1`
    );
    if (!ret.status) {
        throw new Error(
            `Test Board control command failed on ${tamperSensor.pinNameOnTestBoard}: ${ret.error}`
        );
    }
}

async function initSensor() {
    return await gpioHelper.configureMultipleInput([tamperSensor]);
}

async function test(logger, db) {
    gpioHelper = new GPIOHelper(targetICTLink, logger, db);

    try {
        if (!(await initSensor())) {
            return false;
        }

        // Test deactivated state
        await deactivatetamper();
        await delay(100);
        let sensorState = await getSensorState(10, tamperSensor.deactivated, logger, db);
        if (sensorState.value !== tamperSensor.deactivated) {
            logger.error(
                `Tamper sensor test failed: expected deactivated (${tamperSensor.deactivated}), got ${sensorState.value}`
            );
            return false;
        }

        // Test activated state
        await activatetamper();
        await delay(100);
        sensorState = await getSensorState(10, tamperSensor.activated, logger, db);
        if (sensorState.value !== tamperSensor.activated) {
            logger.error(
                `Tamper sensor test failed: expected activated (${tamperSensor.activated}), got ${sensorState.value}`
            );
            return false;
        }

        logger.info('Passed Tamper test');
        return true;
    } catch (err) {
        logger.error(`Tamper test failed: ${err.message}`);
        return false;
    }
}

module.exports = {
    test
};

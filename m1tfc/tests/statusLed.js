'use strict';

const lodash = require('lodash');
const testBoardLink = require('../src/testBoardLink');
const targetICTLink = require('../src/m1ICTLink');
const errorCodes = require('../bin/errorCodes');

const statusLed = {
    ledBlue: { port: 'b', pin: 1, pinNameOnTestBoard: 'J8.2', onState: 1, offState: 0, maxVoltage: -2.6, minVoltage: -3.3 },
    ledRed: { port: 'b', pin: 11, pinNameOnTestBoard: 'J8.1', onState: 1, offState: 0, minVoltage: 1.6, maxVoltage: 2.2 }
};


async function setLedActive(ledOn, ledOff) {
    let ret;
    ret = await targetICTLink.sendCommand(`setgpio ${ledOn.port} ${ledOn.pin} ${ledOn.onState}`);
    if (!ret.status) {
        throw new Error(`Target Board control command <setgpio> failed on pin ${ledOn.port}.${ledOn.pin} ${ret.error}`);
    }

    ret = await targetICTLink.sendCommand(`setgpio ${ledOff.port} ${ledOff.pin} ${ledOff.offState}`);
    if (!ret.status) {
        throw new Error(`Target Board control command <setgpio> failed on pin ${ledOff.port}.${ledOff.pin} ${ret.error}`);
    }
}

async function getLedVoltages() {
    let ret;
    ret = await testBoardLink.sendCommand(`getiopin ${testBoardLink.findPinIdByName(statusLed.ledBlue.pinNameOnTestBoard)}`);
    if (!ret.status) {
        throw new Error(`Test Board control command failed on pinName=${statusLed.ledBlue.pinNameOnTestBoard}, ${ret.error}`);
    }
    const value1 = ret.value;

    ret = await testBoardLink.sendCommand(`getiopin ${testBoardLink.findPinIdByName(statusLed.ledRed.pinNameOnTestBoard)}`);
    if (!ret.status) {
        throw new Error(`Test Board control command failed on pinName=${statusLed.ledBlue.pinNameOnTestBoard}, ${ret.error}`);
    }
    const value2 = ret.value;

    return value1 - value2;
}

async function initLed() {
    let ret;
    ret = await targetICTLink.sendCommand(`confgpio ${statusLed.ledBlue.port} ${statusLed.ledBlue.pin} output none`);
    if (!ret.status) {
        throw new Error(`Target Board control command <confgpio> failed on pin ${statusLed.ledBlue.port}.${statusLed.ledBlue.pin} ${ret.error}`);
    }

    ret = await targetICTLink.sendCommand(`confgpio ${statusLed.ledRed.port} ${statusLed.ledRed.pin} output none`);
    if (!ret.status) {
        throw new Error(`Target Board control command <confgpio> failed on pin ${statusLed.ledRed.port}.${statusLed.ledRed.pin} ${ret.error}`);
    }
}

async function test(logger, db) {
    let ret = true;
    try {
        await initLed(logger);
        await setLedActive(statusLed.ledBlue, statusLed.ledRed, logger);
        let ledVoltage = await getLedVoltages(logger);

        if (!lodash.inRange(ledVoltage, statusLed.ledBlue.minVoltage, statusLed.ledBlue.maxVoltage)) {
            logger.error(`Led test failed, expected voltage range ${statusLed.ledBlue.minVoltage} - ${statusLed.ledBlue.maxVoltage}, actual ${ledVoltage}`);
            db.updateErrorCode(process.env.serial, errorCodes.codes[statusLed.ledBlue.pinNameOnTestBoard].errorCode, 'E');
            ret = false;
        }

        setLedActive(statusLed.ledRed, statusLed.ledBlue, logger);
        ledVoltage = await getLedVoltages(logger);
        if (!lodash.inRange(ledVoltage, statusLed.ledRed.minVoltage, statusLed.ledRed.maxVoltage)) {
            logger.error(`Led test failed, expected voltage range ${statusLed.ledBlue.minVoltage} - ${statusLed.ledBlue.maxVoltage}, actual ${ledVoltage}`);
            db.updateErrorCode(process.env.serial, errorCodes.codes[statusLed.ledRed.pinNameOnTestBoard].errorCode, 'E');
            ret = false;
        }

        if (ret) logger.info('Passed Led test');
        else logger.error('Led test failed');
    }
    catch (err) {
        logger.error('Failed Led test');
        logger.error(err);
        db.updateErrorCode(process.env.serial, errorCodes.codes[statusLed.ledBlue.pinNameOnTestBoard].errorCode, 'T');
        // logger.debug(err.stack);
        return false;
    }

    return true;
}

module.exports = {
    test
};

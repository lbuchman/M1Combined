'use strict';

const lodash = require('lodash');
const testBoardLink = require('../src/testBoardLink');
const targetICTLink = require('../src/m1ICTLink');
const errorCodes = require('../bin/errorCodes');
const runtimeContext = require('../utils/runtimeContext');
const GPIOHelper = require('../utils/gpioHelper');
const CommandHelper = require('../utils/commandHelper');

const statusLed = {
    ledBlue: {
        port: 'b',
        pin: 1,
        pinNameOnTestBoard: 'J8.2',
        onState: 1,
        offState: 0,
        maxVoltage: -2.6,
        minVoltage: -3.3
    },
    ledRed: {
        port: 'b',
        pin: 11,
        pinNameOnTestBoard: 'J8.1',
        onState: 1,
        offState: 0,
        minVoltage: 1.6,
        maxVoltage: 2.2
    }
};

let gpioHelper;
let cmdHelper;

async function setLedActive(ledOn, ledOff) {
    if (
        !(await gpioHelper.setLevel(ledOn.port, ledOn.pin, ledOn.onState, ledOn.pinNameOnTestBoard))
    ) {
        return false;
    }
    return await gpioHelper.setLevel(
        ledOff.port,
        ledOff.pin,
        ledOff.offState,
        ledOff.pinNameOnTestBoard
    );
}

async function getLedVoltages() {
    const blueRet = await cmdHelper.execute(
        () =>
            testBoardLink.sendCommand(
                `getiopin ${testBoardLink.findPinIdByName(statusLed.ledBlue.pinNameOnTestBoard)}`
            ),
        `Read blue LED voltage (${statusLed.ledBlue.pinNameOnTestBoard})`,
        statusLed.ledBlue.pinNameOnTestBoard,
        'T'
    );
    if (!blueRet) {
        return false;
    }

    const redRet = await cmdHelper.execute(
        () =>
            testBoardLink.sendCommand(
                `getiopin ${testBoardLink.findPinIdByName(statusLed.ledRed.pinNameOnTestBoard)}`
            ),
        `Read red LED voltage (${statusLed.ledRed.pinNameOnTestBoard})`,
        statusLed.ledRed.pinNameOnTestBoard,
        'T'
    );
    if (!redRet) {
        return false;
    }

    return blueRet.value - redRet.value;
}

async function initLed() {
    if (
        !(await gpioHelper.configureOutput(
            statusLed.ledBlue.port,
            statusLed.ledBlue.pin,
            statusLed.ledBlue.pinNameOnTestBoard
        ))
    ) {
        return false;
    }
    return await gpioHelper.configureOutput(
        statusLed.ledRed.port,
        statusLed.ledRed.pin,
        statusLed.ledRed.pinNameOnTestBoard
    );
}

async function test(logger, db) {
    gpioHelper = new GPIOHelper(targetICTLink, logger, db);
    cmdHelper = new CommandHelper(logger, db);

    try {
        if (!(await initLed())) {
            return false;
        }

        // Test blue LED
        if (!(await setLedActive(statusLed.ledBlue, statusLed.ledRed))) {
            return false;
        }
        let ledVoltage = await getLedVoltages();
        if (ledVoltage === false) {
            return false;
        }

        if (
            !lodash.inRange(ledVoltage, statusLed.ledBlue.minVoltage, statusLed.ledBlue.maxVoltage)
        ) {
            logger.error(
                `Blue LED test failed, expected ${statusLed.ledBlue.minVoltage}-${statusLed.ledBlue.maxVoltage}V, got ${ledVoltage}V`
            );
            db.updateErrorCode(
                runtimeContext.getRuntime().serial,
                errorCodes.codes[statusLed.ledBlue.pinNameOnTestBoard].errorCode,
                'E'
            );
            return false;
        }

        // Test red LED
        if (!(await setLedActive(statusLed.ledRed, statusLed.ledBlue))) {
            return false;
        }
        ledVoltage = await getLedVoltages();
        if (ledVoltage === false) {
            return false;
        }

        if (!lodash.inRange(ledVoltage, statusLed.ledRed.minVoltage, statusLed.ledRed.maxVoltage)) {
            logger.error(
                `Red LED test failed, expected ${statusLed.ledRed.minVoltage}-${statusLed.ledRed.maxVoltage}V, got ${ledVoltage}V`
            );
            db.updateErrorCode(
                runtimeContext.getRuntime().serial,
                errorCodes.codes[statusLed.ledRed.pinNameOnTestBoard].errorCode,
                'E'
            );
            return false;
        }

        logger.info('Passed LED test');
        return true;
    } catch (err) {
        logger.error(`LED test exception: ${err.message}`);
        db.updateErrorCode(
            runtimeContext.getRuntime().serial,
            errorCodes.codes[statusLed.ledBlue.pinNameOnTestBoard].errorCode,
            'T'
        );
        return false;
    }
}

module.exports = {
    test
};

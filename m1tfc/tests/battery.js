'use strict';

/* eslint-disable no-await-in-loop */

const testBoardLink = require('../src/testBoardLink');
const targetICTLink = require('../src/m1ICTLink');
const delay = require('delay');
const errorCodes = require('../bin/errorCodes');
const runtimeContext = require('../utils/runtimeContext');
const GPIOHelper = require('../utils/gpioHelper');

const chargerIntervalCheck = 5; /* 5 sec */
const chargerMinVoltage = 12.6;

const batteryStateStatusPins = [
    { name: 'nBATT_PRESENT', port: 'g', pin: 7, pinNameOnTestBoard: 'none' },
    { name: 'nDCIN_PWR', port: 'e', pin: 9, pinNameOnTestBoard: 'none' }
];

let gpioHelper;

async function getChargingVoltage(timeout) {
    if (timeout - new Date() / 1000 > 0) {
        const batChargeVoltage = await testBoardLink.sendCommand(
            `getiopin ${testBoardLink.findPinIdByName('batChargeVAD')}`
        );
        if (batChargeVoltage.status) {
            if (batChargeVoltage.value > chargerMinVoltage) {
                return batChargeVoltage.value;
            }
        }
        await delay(500);
        return getChargingVoltage(timeout);
    }

    throw Error('Battery charging circuit test failed');
}

async function readBatteryPin(pin) {
    const ret = await gpioHelper.readLevel(pin.port, pin.pin, pin.pinNameOnTestBoard);
    if (ret === false) {
        throw new Error(`Failed to read battery pin ${pin.port}.${pin.pin}`);
    }
    return ret;
}

async function configureBatteryPins() {
    for (const pin of batteryStateStatusPins) {
        const ret = await targetICTLink.sendCommand(`confgpio ${pin.port} ${pin.pin} input none`);
        if (!ret.status) {
            throw new Error(`GPIO config failed on pin ${pin.port}.${pin.pin}: ${ret.error}`);
        }
    }
}

async function test(logger, db) {
    gpioHelper = new GPIOHelper(targetICTLink, logger, db);

    try {
        logger.info('Testing battery switch circuit');
        await configureBatteryPins();
        await testBoardLink.targetPower(true);
        await testBoardLink.batteryLoadOn(true);
        await testBoardLink.batteryOn(false);
        await delay(500);

        let battPresent = await readBatteryPin(batteryStateStatusPins[0]);
        let dcinPresent = await readBatteryPin(batteryStateStatusPins[1]);
        if (battPresent !== 1) {
            throw new Error('nBATT_PRESENT expected 0, got 1');
        }
        if (dcinPresent !== 0) {
            throw new Error('nDCIN_PWR expected 0, got 1');
        }

        await testBoardLink.targetPower(true);
        await testBoardLink.batteryLoadOn(false);
        await delay(500);
        battPresent = await readBatteryPin(batteryStateStatusPins[0]);
        dcinPresent = await readBatteryPin(batteryStateStatusPins[1]);
        if (battPresent !== 1) {
            throw new Error('nBATT_PRESENT expected 0, got 1');
        }
        if (dcinPresent !== 0) {
            throw new Error('nDCIN_PWR expected 0, got 1');
        }

        await testBoardLink.batteryOn(true);
        await delay(5000);
        await testBoardLink.targetPower(false);
        await testBoardLink.batteryLoadOn(true);

        logger.info('Testing main 12V power loss');
        await delay(500);
        try {
            battPresent = await readBatteryPin(batteryStateStatusPins[0]);
        } catch (err) {
            throw new Error('Power off battery test failed');
        }
        logger.info('Power loss test passed');
        dcinPresent = await readBatteryPin(batteryStateStatusPins[1]);
        if (battPresent !== 1) {
            throw new Error('nBATT_PRESENT expected 0, got 1');
        }
        if (dcinPresent !== 1) {
            throw new Error('nDCIN_PWR expected 0, got 1');
        }

        logger.info('Testing battery charging circuit...');
        await testBoardLink.targetPower(true);
        await delay(500);
        battPresent = await readBatteryPin(batteryStateStatusPins[0]);
        dcinPresent = await readBatteryPin(batteryStateStatusPins[1]);
        if (battPresent !== 1) {
            throw new Error('nBATT_PRESENT expected 1, got 0');
        }
        if (dcinPresent !== 0) {
            throw new Error('nDCIN_PWR expected 0, got 1');
        }

        const batVoltage = await testBoardLink.sendCommand(
            `getiopin ${testBoardLink.findPinIdByName('bat12VAD')}`
        );
        if (!batVoltage.status) {
            throw Error('Failed to read battery voltage');
        }

        const chargingVoltage = await getChargingVoltage(new Date() / 1000 + chargerIntervalCheck);
        logger.info(`Battery test passed. Charging voltage = ${chargingVoltage}V`);
    } catch (err) {
        db.updateErrorCode(
            runtimeContext.getRuntime().serial,
            errorCodes.codes['BACHR'].errorCode,
            'E'
        );
        throw new Error(err.message);
    } finally {
        await testBoardLink.batteryLoadOn(false);
    }

    return true;
}

module.exports = {
    test
};

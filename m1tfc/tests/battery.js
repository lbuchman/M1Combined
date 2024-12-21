'use strict';

const testBoardLink = require('../src/testBoardLink');
const targetICTLink = require('../src/m1ICTLink');
const delay = require('delay');
const errorCodes = require('../bin/errorCodes');

const chargerIntervalCheck = 5; /* 5 sec */
const chargerMinVoltage = 12.6;

const batteryStateStatusPins = [
    { name: 'nBATT_PRESENT', port: 'g', pin: 7, pinNameOnTestBoard: 'none' },
    { name: 'nDCIN_PWR', port: 'e', pin: 9, pinNameOnTestBoard: 'none' }
];

async function getChargingVoltage(timeout) {
    if (timeout - new Date() / 1000 > 0) {
        const batChargeVoltage = await testBoardLink.sendCommand(`getiopin ${testBoardLink.findPinIdByName('batChargeVAD')}`);
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

async function test(logger, db) {
    try {
        logger.info('Testing battery switch circuit');
        await targetICTLink.sendCommand(`confgpio ${batteryStateStatusPins[0].port} ${batteryStateStatusPins[0].pin} input none`);
        await targetICTLink.sendCommand(`confgpio ${batteryStateStatusPins[1].port} ${batteryStateStatusPins[1].pin} input none`);
        await testBoardLink.targetPower(true);
        await testBoardLink.batteryLoadOn(true);
        await testBoardLink.batteryOn(true);
        await delay(500);
        let battPresent = await targetICTLink.sendCommand(`getgpio ${batteryStateStatusPins[0].port} ${batteryStateStatusPins[0].pin}`);
        let dcinPresent = await await targetICTLink.sendCommand(`getgpio ${batteryStateStatusPins[1].port} ${batteryStateStatusPins[1].pin}`);
        if (battPresent.value !== 1) throw new Error('nBATT_PRESENT status bit is 1, expected 0');
        if (dcinPresent.value !== 0) throw new Error('nDCIN_PWR status bit is 1, expected 0');

        await testBoardLink.targetPower(true);
        await testBoardLink.batteryLoadOn(false);
        await delay(500);
        battPresent = await targetICTLink.sendCommand(`getgpio ${batteryStateStatusPins[0].port} ${batteryStateStatusPins[0].pin}`);
        dcinPresent = await await targetICTLink.sendCommand(`getgpio ${batteryStateStatusPins[1].port} ${batteryStateStatusPins[1].pin}`);
        if (battPresent.value !== 1) throw new Error('nBATT_PRESENT status bit is 1, expected 0');
        if (dcinPresent.value !== 0) throw new Error('nDCIN_PWR status bit is 1, expected 0');
        await testBoardLink.targetPower(false);
        await testBoardLink.batteryLoadOn(true);
        logger.info('Testing main 12V power loss');
        await delay(500);
        try {
            battPresent = await targetICTLink.sendCommand(`getgpio ${batteryStateStatusPins[0].port} ${batteryStateStatusPins[0].pin}`);
        }
        catch (err) {
            throw new Error('Power off, Battery On test failed');
        }
        logger.info('Power loss test passed');
        dcinPresent = await await targetICTLink.sendCommand(`getgpio ${batteryStateStatusPins[1].port} ${batteryStateStatusPins[1].pin}`);
        if (battPresent.value !== 1) throw new Error('nBATT_PRESENT status bit is 1, expected 0');
        if (dcinPresent.value !== 1) throw new Error('nDCIN_PWR status bit is 1, expected 0');
        logger.info('Testing battery charging circuit ...');
        await testBoardLink.targetPower(true);
        battPresent = await targetICTLink.sendCommand(`getgpio ${batteryStateStatusPins[0].port} ${batteryStateStatusPins[0].pin}`);
        dcinPresent = await await targetICTLink.sendCommand(`getgpio ${batteryStateStatusPins[1].port} ${batteryStateStatusPins[1].pin}`);
        if (battPresent.value !== 1) throw new Error('nBATT_PRESENT status bit is 1, expected 0');
        if (dcinPresent.value !== 1) throw new Error('nDCIN_PWR status bit is 1, expected 0');
        battPresent = await targetICTLink.sendCommand(`getgpio ${batteryStateStatusPins[0].port} ${batteryStateStatusPins[0].pin}`);
        dcinPresent = await await targetICTLink.sendCommand(`getgpio ${batteryStateStatusPins[1].port} ${batteryStateStatusPins[1].pin}`);
        const batVoltage = await testBoardLink.sendCommand(`getiopin ${testBoardLink.findPinIdByName('bat12VAD')}`);
        if (!batVoltage.status) {
            throw Error('Target Board control command <getiopin bat12VAD> failed');
        }
        const chargingVoltage = await getChargingVoltage(new Date() / 1000 + chargerIntervalCheck);
        logger.info(`Battery test passed. charging Voltage = ${chargingVoltage}V`);
    }
    catch (err) {
        /* eslint-disable dot-notation */
        db.updateErrorCode(process.env.serial, errorCodes.codes['BACHR'].errorCode, 'E');
        throw new Error(err.message);
    }
    finally {
        await testBoardLink.batteryLoadOn(false);
    }

    return true;
}

module.exports = {
    test
};

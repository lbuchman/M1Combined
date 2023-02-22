'use strict';

const testBoardLink = require('../src/testBoardLink');
const targetICTLink = require('../src/m1ICTLink');
const delay = require('delay');

const chargerIntervalCheck = 5; /* 5 sec */
const chargerMinVoltage = 12.6;

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

async function test(logger) {
    try {
        logger.info('Testing power to battery switch circuit ...');
        await testBoardLink.targetPower(false);
        await testBoardLink.batteryLoadOn(true);
        await delay(500);
        await targetICTLink.sendCommand('getfwrev');
        logger.info('Testing battery charging circuit ...');
        await testBoardLink.targetPower(true);
        const batVoltage = await testBoardLink.sendCommand(`getiopin ${testBoardLink.findPinIdByName('bat12VAD')}`);
        if (!batVoltage.status) {
            throw Error('Target Board control command <getiopin bat12VAD> failed');
        }
        const chargingVoltage = await getChargingVoltage(new Date() / 1000 + chargerIntervalCheck);
        logger.info(`Battery test passed. charging Voltage = ${chargingVoltage}V`);
    }
    catch (err) {
        throw new Error('Battery test failed');
    }
    finally {
        await testBoardLink.batteryLoadOn(false);
    }

    return true;
}

module.exports = {
    test
};

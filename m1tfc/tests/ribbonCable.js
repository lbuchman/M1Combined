'use strict';

const utils = require('../utils/utils');

const targetICTLink = require('../src/m1ICTLink');
const testBoardLink = require('../src/testBoardLink');


const ribbonCableSelectPins = [
    { name: 'aioS0', port: 'd', pin: 5, pinNameOnTestBoard: 'J5.18' },
    { name: 'aioS1', port: 'c', pin: 6, pinNameOnTestBoard: 'J5.15' },
    { name: 'aioS2', port: 'd', pin: 4, pinNameOnTestBoard: 'J5.16' },
    { name: 'aioSel7', port: 'c', pin: 10, pinNameOnTestBoard: 'J5.19' },
    { name: 'aioNrst', port: 'k', pin: 0, pinNameOnTestBoard: 'J5.17' }
];

const ribbonCableI2CPinsSlave = [
    { name: 'sda', port: 'a', pin: 12, pinNameOnTestBoard: 'J5.6' },
    { name: 'scl', port: 'a', pin: 11, pinNameOnTestBoard: 'J5.3' }
];

const ribbonCableI2CPinsMaster = [
    { name: 'sda', port: 'g', pin: 15, pinNameOnTestBoard: 'J5.6' },
    { name: 'scl', port: 'd', pin: 7, pinNameOnTestBoard: 'J5.3' }
];

async function runRibbonCableTestStaticVoltages(tolerance, logger) {
    let retValue;
    let freturn = true;
    const testBoardIoDef = testBoardLink.getIoDef();
    try {
        // eslint-disable-next-line no-plusplus
        for (let count = 0; count < testBoardIoDef.length; count++) { // canot use forEach or map because callbacks will bring in concurrency
            retValue = true;
            // eslint-disable-next-line no-continue
            if (testBoardIoDef[count].group !== testBoardLink.getGroupdDefinition().RibbonCableGroupStatic) continue;
            // eslint-disable-next-line no-await-in-loop
            const ret = await testBoardLink.sendCommand(`getiopin ${testBoardIoDef[count].pinId}`);
            if (!ret.status) {
                logger.error(`Test Board control command failed on pinName=${ribbonCableSelectPins[count].pinNameOnTestBoard}, ${ret.error}`);
                retValue = false;
                freturn = false;
            }

            let zeroValueFix = 0;
            if (!testBoardIoDef[count].reqValue) zeroValueFix = 5;
            if (!retValue || ((Math.abs(ret.value - testBoardIoDef[count].reqValue)) / (testBoardIoDef[count].reqValue + zeroValueFix)) > tolerance) {
                logger.error(`Failed: Voltage is out of tolerance, pinName=${testBoardIoDef[count].pinName}, value=${ret.value}, reqValue=${testBoardIoDef[count].reqValue}`);
                retValue = false;
                freturn = false;
            }
            else {
                logger.info(`Passed pinName=${testBoardIoDef[count].pinName}, actual = ${ret.value}V, expected = ${testBoardIoDef[count].reqValue}V `);
            }
        }
        if (!freturn) {
            return false;
        }
        return true;
    }
    catch (err) {
        logger.error(err);
        // logger.debug(err.stack);
        return false;
    }
}
async function runRibbonCableTestAddressSelectResetPins(settoLevel, floatpins, logger) {
    let freturn = true;
    let ret;
    let count = 0;
    logger.info(`Testing ribbon cable address select lines for value - ${settoLevel}V`);
    try {
        // eslint-disable-next-line no-plusplus
        for (count = 0; count < ribbonCableSelectPins.length; count++) { // canot use forEach or map because callbacks will bring in concurrency
            let retValue = true;
            if (!floatpins) {
                // eslint-disable-next-line no-await-in-loop
                ret = await targetICTLink.sendCommand(`confgpio ${ribbonCableSelectPins[count].port} ${ribbonCableSelectPins[count].pin} output none`);
                if (!ret.status) {
                    logger.error(`Target Board control command <confgpio> failed on pin ${ribbonCableSelectPins[count].port}.${ribbonCableSelectPins[count].pin}, ${ret.error}`);
                    retValue = false;
                    freturn = false;
                }

                // eslint-disable-next-line no-await-in-loop
                ret = await targetICTLink.sendCommand(`setgpio ${ribbonCableSelectPins[count].port} ${ribbonCableSelectPins[count].pin} ${settoLevel}`);
                if (!ret.status) {
                    logger.error(`Target Board control command <setgpio> failed on pin ${ribbonCableSelectPins[count].port}.${ribbonCableSelectPins[count].pin}, ${ret.error}`);
                    retValue = false;
                    freturn = false;
                }
            }
            // eslint-disable-next-line no-await-in-loop
            ret = await testBoardLink.sendCommand(`getiopin ${testBoardLink.findPinIdByName(ribbonCableSelectPins[count].pinNameOnTestBoard)}`);
            if (!ret.status) {
                logger.error(`Test Board control command failed on pinName=${ribbonCableSelectPins[count].pinNameOnTestBoard}, ${ret.error}`);
                retValue = false;
            }

            if (!retValue || !utils.testLogicalValue(ret.value, 3.3, settoLevel)) {
                logger.error(`Failed: Incorrect voltage level on Pin=${ribbonCableSelectPins[count].pinNameOnTestBoard}, Value=${ret.value} 1ogValue=${settoLevel}`);
                retValue = false;
                freturn = false;
            }
            else {
                logger.info(`Passed pinName=${ribbonCableSelectPins[count].pinNameOnTestBoard}, actual = ${ret.value}`);
            }
        }

        if (!freturn) {
            return false;
        }
    }
    catch (err) {
        logger.error(err);
        // logger.debug(err.stack);
        return false;
    }
    return true;
}

async function testI2Cpins(pins, logger, settoLevel) {
    let retValue = true;
    let ret;
    let count = 0;
    try {
        // eslint-disable-next-line no-plusplus
        for (count = 0; count < pins.length; count++) { // canot use forEach or map because callbacks will bring in concurrency
            // eslint-disable-next-line no-await-in-loop
            ret = await testBoardLink.sendCommand(`setiopin ${testBoardLink.findPinIdByName(pins[count].pinNameOnTestBoard)} ${settoLevel}`);
            if (!ret.status) {
                logger.error(`Test Board Command failed on pinName=${pins[count].pinNameOnTestBoard}, ${ret.error}`);
                retValue = false;
            }

            // eslint-disable-next-line no-await-in-loop
            ret = await targetICTLink.sendCommand(`confgpio ${pins[count].port} ${pins[count].pin} input none`);
            if (!ret.status) {
                retValue = false;
                logger.error(`Target Board control command failed on ${pins[count].name}`);
            }

            // eslint-disable-next-line no-await-in-loop
            ret = await targetICTLink.sendCommand(`getgpio ${pins[count].port} ${pins[count].pin}`);
            if (!ret.status) {
                retValue = false;
                logger.error(`Target Board Command failed on ${pins[count].name}`);
            }

            if (ret.value !== settoLevel) {
                logger.error(`Test Failed: Incorrect voltage level on Pin=${pins[count].pinNameOnTestBoard}, expected ${settoLevel}, actual=${ret.value}`);
                retValue = false;
            }
            else {
                logger.debug(`Passed pinName=${pins[count].pinNameOnTestBoard}, actual = ${ret.value}, ${settoLevel} `);
            }
        }

        if (!retValue) {
            return false;
        }
    }
    catch (err) {
        logger.error(err);
        // logger.debug(err.stack);
        return false;
    }
    return true;
}

async function testRs422(logger) {
    try {
        const ret = await targetICTLink.sendCommand('testrs422');
        if (!ret.status) {
            logger.error(`Failed Ribbon Rs422 echo test error: ${ret.error}`);
            return false;
        }
        logger.info('Passed Ribbon RS422 echo test ');
    }
    catch (err) {
        logger.error('Failed Ribbon RS422 echo test');
        logger.error(err);
        // logger.debug(err.stack);
        return false;
    }
    return true;
}

async function runRibbonCableTest(tolerance, logger) {
    let retValue = true;
    const i2cTestCases = [0, 1, 0, 1];
    // eslint-disable-next-line no-restricted-syntax
    for (const testCase of i2cTestCases) {
        // eslint-disable-next-line no-await-in-loop, no-loop-func
        if (!await testI2Cpins(ribbonCableI2CPinsSlave, logger, testCase)) retValue = false;
    }
    logger.info('Passed I2C Slave test');
    // eslint-disable-next-line no-restricted-syntax
    for (const testCase of i2cTestCases) {
        // eslint-disable-next-line no-await-in-loop, no-loop-func
        if (!await testI2Cpins(ribbonCableI2CPinsMaster, logger, testCase)) retValue = false;
    }
    logger.info('Passed I2C Master test');
    if (!await runRibbonCableTestAddressSelectResetPins(1, false, logger)) retValue = false;
    if (!await runRibbonCableTestAddressSelectResetPins(0, false, logger)) retValue = false;
    if (!await runRibbonCableTestStaticVoltages(tolerance, logger)) retValue = false;
    if (!await testRs422(logger).catch(() => { retValue = false; })) retValue = false;
    if (!retValue) {
        return false;
    }

    logger.info('Passed Ribbon cable test');
    return true;
}

module.exports = {
    runRibbonCableTest
};

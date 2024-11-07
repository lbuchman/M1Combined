'use strict';

const utils = require('../utils/utils');
const errorCodes = require('../bin/errorCodes');

const targetICTLink = require('../src/m1ICTLink');
const testBoardLink = require('../src/testBoardLink');

let db;

const ribbonCableSelectPins = [
    { name: 'aioS0', port: 'd', pin: 5, pinNameOnTestBoard: 'J5.18' },
    { name: 'aioS1', port: 'c', pin: 6, pinNameOnTestBoard: 'J5.15' },
    { name: 'aioS2', port: 'd', pin: 4, pinNameOnTestBoard: 'J5.16' },
    { name: 'aioSel7', port: 'c', pin: 10, pinNameOnTestBoard: 'J5.19' },
    { name: 'aioNrst', port: 'k', pin: 0, pinNameOnTestBoard: 'J5.17' }
];


const ribbonCableA2DPins = [
    { name: 'TP1801', voltage: 2.97 },
    { name: 'TP1802', voltage: 2.97 },
    { name: 'TP1901', voltage: 2.97 },
    { name: 'TP1902', voltage: 2.97 },
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
                logger.error(`Test Board control command failed on pinName=${testBoardIoDef[count].pinName}, ${ret.error}`);
                db.updateErrorCode(process.env.serial, errorCodes.codes[testBoardIoDef[count].pinName].errorCode, 'T');
                retValue = false;
                freturn = false;
            }

            let zeroValueFix = 0;
            if (!testBoardIoDef[count].reqValue) zeroValueFix = 5;
            if (!retValue || ((Math.abs(ret.value - testBoardIoDef[count].reqValue)) / (testBoardIoDef[count].reqValue + zeroValueFix)) > tolerance) {
                logger.error(`Failed: Voltage is out of tolerance, pinName=${testBoardIoDef[count].pinName}, value=${ret.value}, reqValue=${testBoardIoDef[count].reqValue}`);
                retValue = false;
                freturn = false;
                db.updateErrorCode(process.env.serial, errorCodes.codes[testBoardIoDef[count].pinName].errorCode, 'E');
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

async function testA2DVoltages(tolerance, logger) {
    let retValue = true;
    // eslint-disable-next-line no-restricted-syntax
    for (const testPoint of ribbonCableA2DPins) {
        // eslint-disable-next-line no-await-in-loop
        const ret = await testBoardLink.sendCommand(`getiopin ${testBoardLink.findPinIdByName(testPoint.name)}`);
        if (!ret.status) {
            db.updateErrorCode(process.env.serial, errorCodes.codes[testPoint.name].errorCode, 'T');
            throw new Error(`Test Board control command failed on pinName=${testPoint.name}, ${ret.error}`);
        }
        if (!testPoint.tolerance) testPoint.tolerance = tolerance;
        if (((Math.abs(ret.value - testPoint.voltage)) / (testPoint.voltage)) > testPoint.tolerance) {
            db.updateErrorCode(process.env.serial, errorCodes.codes[testPoint.name].errorCode, 'E');
            logger.error(`Failed: Voltage is out of tolerance, TP=${testPoint.name}, value=${ret.value}, reqValue=${testPoint.voltage}`);
            retValue = false;
        }
        else {
            logger.info(`Passed TP=${testPoint.name} test, Voltage = ${ret.value}V, Expected = ${testPoint.voltage}V`);
        }
    }
    return retValue;
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
                    db.updateErrorCode(process.env.serial, errorCodes.codes[ribbonCableSelectPins[count].pinNameOnTestBoard].errorCode, 'T');
                }

                // eslint-disable-next-line no-await-in-loop
                ret = await targetICTLink.sendCommand(`setgpio ${ribbonCableSelectPins[count].port} ${ribbonCableSelectPins[count].pin} ${settoLevel}`);
                if (!ret.status) {
                    logger.error(`Target Board control command <setgpio> failed on pin ${ribbonCableSelectPins[count].port}.${ribbonCableSelectPins[count].pin}, ${ret.error}`);
                    retValue = false;
                    freturn = false;
                    db.updateErrorCode(process.env.serial, errorCodes.codes[ribbonCableSelectPins[count].pinNameOnTestBoard].errorCode, 'E');
                }
            }
            // eslint-disable-next-line no-await-in-loop
            ret = await testBoardLink.sendCommand(`getiopin ${testBoardLink.findPinIdByName(ribbonCableSelectPins[count].pinNameOnTestBoard)}`);
            if (!ret.status) {
                logger.error(`Test Board control command failed on pinName=${ribbonCableSelectPins[count].pinNameOnTestBoard}, ${ret.error}`);
                retValue = false;
                db.updateErrorCode(process.env.serial, errorCodes.codes[ribbonCableSelectPins[count].pinNameOnTestBoard].errorCode, 'T');
            }

            if (!retValue || !utils.testLogicalValue(ret.value, 3.3, settoLevel)) {
                logger.error(`Failed: Incorrect voltage level on Pin=${ribbonCableSelectPins[count].pinNameOnTestBoard}, Value=${ret.value} 1ogValue=${settoLevel}`);
                retValue = false;
                freturn = false;
                db.updateErrorCode(process.env.serial, errorCodes.codes[ribbonCableSelectPins[count].pinNameOnTestBoard].errorCode, 'E');
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
            ret = await targetICTLink.sendCommand(`confgpio ${pins[count].port} ${pins[count].pin} output none`);
            if (!ret.status) {
                retValue = false;
                db.updateErrorCode(process.env.serial, errorCodes.codes[pins[count].pinNameOnTestBoard].errorCode, 'T');
                logger.error(`Target Board control command failed on ${pins[count].name}`);
                return false;
            }

            // eslint-disable-next-line no-await-in-loop
            ret = await targetICTLink.sendCommand(`setgpio ${pins[count].port} ${pins[count].pin} ${settoLevel}`);
            if (!ret.status) {
                retValue = false;
                logger.error(`Target Board Command failed on ${pins[count].name}`);
                db.updateErrorCode(process.env.serial, errorCodes.codes[pins[count].pinNameOnTestBoard].errorCode, 'T');
                return false;
            }

            // eslint-disable-next-line no-await-in-loop
            ret = await testBoardLink.sendCommand(`getiopin ${testBoardLink.findPinIdByName(pins[count].pinNameOnTestBoard)}`);
            if (!ret.status) {
                logger.error(`Test Board Command failed on pinName=${pins[count].pinNameOnTestBoard}, ${ret.error}`);
                db.updateErrorCode(process.env.serial, errorCodes.codes[pins[count].pinNameOnTestBoard].errorCode, 'T');
                retValue = false;
                return false;
            }
            if (ret.value === undefined) {
                db.updateErrorCode(process.env.serial, errorCodes.codes[pins[count].pinNameOnTestBoard].errorCode, 'E');
                return false;
            }
            if (ret.value > 3) ret.value = 1;
            if (ret.value !== settoLevel) {
                logger.error(`Test Failed: Incorrect voltage level on Pin=${pins[count].pinNameOnTestBoard}, expected ${settoLevel}, actual=${ret.value}`);
                db.updateErrorCode(process.env.serial, errorCodes.codes[pins[count].pinNameOnTestBoard].errorCode, 'E');
                retValue = false;
                return false;
            }
            logger.debug(`Passed pinName=${pins[count].pinNameOnTestBoard}, actual = ${ret.value}, ${settoLevel} `);
        }

        if (!retValue) {
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

async function testRs422(logger) {
    try {
        const ret = await targetICTLink.sendCommand('testrs422');
        if (!ret.status) {
            logger.error(`Failed Ribbon Rs422 echo test error: ${ret.error}`);
            throw new Error('Ribbon RS422 echo test failed');
        }
        logger.info('Passed Ribbon RS422 echo test ');
    }
    catch (err) {
        logger.error('Failed Ribbon RS422 echo test');
        /* eslint-disable dot-notation */
        db.updateErrorCode(process.env.serial, errorCodes.codes['RS422'].errorCode, 'E');
        // logger.error(err);
        // logger.debug(err.stack);
        return false;
    }
    return true;
}

async function runRibbonCableTestMnp(tolerance, logger, db_) {
    db = db_;
    let retValue = true;

    if (!await testA2DVoltages(tolerance, logger)) retValue = false;

    // if (!await runRibbonCableTestAddressSelectResetPins(1, false, logger)) retValue = false;
    // if (!await runRibbonCableTestAddressSelectResetPins(0, false, logger)) retValue = false;


    return retValue;
}

async function runRibbonCableTestM1(tolerance, logger, db_) {
    logger.info('Testing Ribbon cable pins ...');
    db = db_;
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

async function runRibbonCableTest(tolerance, logger, db_) {
    if (process.env.productName === 'mnplus') return runRibbonCableTestMnp(tolerance, logger, db_);
    if (process.env.productName === 'm1') return runRibbonCableTestM1(tolerance, logger, db_);
    return true;
}

module.exports = {
    runRibbonCableTest
};

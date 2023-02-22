'use strict';


const targetICTLink = require('../src/m1ICTLink');

async function testDDRDatabus(logger) {
    try {
        const ret = await targetICTLink.sendCommand('ddrdatbus');
        if (!ret.status) {
            logger.error(`Failed DDR3 databus test ${ret.error}`);
            return false;
        }
        logger.info('Passed DDR3 data bus test');
    }
    catch (err) {
        logger.error('Failed DDR3 databus test');
        logger.error(err);
        // logger.debug(err.stack);
        return false;
    }
    return true;
}

async function testDDRAddrbus(logger) {
    try {
        const ret = await targetICTLink.sendCommand('ddradrbus');
        if (!ret.status) {
            logger.error(`Failed DDR3 address test ${ret.error}`);
            return false;
        }
        logger.info('Passed DDR3 address bus test');
    }
    catch (err) {
        logger.error('Failed DDR3 adress bus test');
        logger.error(err);
        // logger.debug(err.stack);
        return false;
    }
    return true;
}

async function testDDRtest(ddrblocks, logger) {
    try {
        const memSize = parseInt(ddrblocks, 10) * 1024 * 1024;
        logger.info(`Testing DDR3 memsize 0x${memSize.toString(16)}, test may take a while`);
        const ret = await targetICTLink.sendCommand(`ddrtest 0 ${memSize.toString(16)}`, 65000);
        if (!ret.status) {
            logger.error(`Failed DDR3 test ${ret.error}`);
            return false;
        }
        logger.info('Passed DDR3 test');
    }
    catch (err) {
        logger.error('Failed DDR3 test');
        logger.error(err);
        // logger.debug(err.stack);
        return false;
    }
    return true;
}

async function testDDR3Test(ddrblocks, logger) {
    let ret = true;
    if (!await testDDRDatabus(logger)) ret = false;
    if (!await testDDRAddrbus(logger)) ret = false;
    if (!await testDDRtest(ddrblocks, logger)) ret = false;
    return ret;
}

module.exports = {
    testDDR3Test
};

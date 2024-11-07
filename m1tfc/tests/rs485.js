'use strict';

const targetICTLink = require('../src/m1ICTLink');
const errorCodes = require('../bin/errorCodes');

async function testRs485(logger, db) {
    try {
        const ret = await targetICTLink.sendCommand('testrs485');
        if (!ret.status) {
            logger.error(`Failed RS485 echo test ${ret.error}`);
            throw new Error('RS485 echo test failed');
        }
        logger.info('Passed RS485 test');
    }
    catch (err) {
        logger.error('Failed RS485 echo test');
        // logger.error(err);
        // logger.debug(err.stack);
        /* eslint-disable dot-notation */
        db.updateErrorCode(process.env.serial, errorCodes.codes['RS485'].errorCode, 'E');
        return false;
    }
    return true;
}

module.exports = {
    testRs485
};

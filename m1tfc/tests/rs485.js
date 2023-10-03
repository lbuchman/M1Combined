'use strict';


const targetICTLink = require('../src/m1ICTLink');

async function testRs485(logger, db) {
    try {
        const ret = await targetICTLink.sendCommand('testrs485');
        if (!ret.status) {
            logger.error(`Failed RS485 echo test ${ret.error}`);
            logger.error('RS485 echo test failed');
            return false;
        }
        logger.info('Passed RS485 test');
    }
    catch (err) {
        logger.error('Failed RS485 echo test');
        logger.error(err);
        // logger.debug(err.stack);
        return false;
    }
    return true;
}

module.exports = {
    testRs485
};

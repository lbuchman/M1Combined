'use strict';

const targetICTLink = require('../src/m1ICTLink');
const CommandHelper = require('../utils/commandHelper');

async function testRs485(logger, db) {
    const cmdHelper = new CommandHelper(logger, db);

    return await cmdHelper.executeTest(
        () => targetICTLink.sendCommand('testrs485'),
        'RS485 echo test',
        'RS485'
    );
}

module.exports = {
    testRs485
};

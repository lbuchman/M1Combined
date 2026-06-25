'use strict';

const targetICTLink = require('../src/m1ICTLink');
const CommandHelper = require('../utils/commandHelper');

let cmdHelper;

async function testDDRDatabus() {
    return await cmdHelper.executeTest(
        () => targetICTLink.sendCommand('ddrdatbus'),
        'DDR3 databus test',
        'DDR3Bus'
    );
}

async function testDDRAddrbus() {
    return await cmdHelper.executeTest(
        () => targetICTLink.sendCommand('ddradrbus'),
        'DDR3 address bus test',
        'DDR3ABus'
    );
}

async function testDDRtest(ddrblocks, logger) {
    const memSize = parseInt(ddrblocks, 10) * 1024 * 1024;
    logger.info(`Testing DDR3 memsize 0x${memSize.toString(16)}, test may take a while`);

    return await cmdHelper.executeTest(
        () => targetICTLink.sendCommand(`ddrtest 0 ${memSize.toString(16)}`, 65000),
        'DDR3 memory test',
        'DDR3'
    );
}

async function testDDR3Test(ddrblocks, logger, db) {
    cmdHelper = new CommandHelper(logger, db);

    cmdHelper.logger.info('Starting DDR3 tests');

    let passed = true;

    if (!await testDDRDatabus()) {
        passed = false;
    }

    if (!await testDDRAddrbus()) {
        passed = false;
    }

    if (!await testDDRtest(ddrblocks, logger)) {
        passed = false;
    }

    return passed;
}

module.exports = {
    testDDR3Test
};

'use strict';

const testBoardLink = require('../src/testBoardLink');

const pinNameOnTestBoard = 'M1Boot1';

async function activateDFU() {
    const ret = await testBoardLink.sendCommand(`setiopin ${testBoardLink.findPinIdByName(pinNameOnTestBoard)} 1`);
    if (!ret.status) {
        throw new Error(`Test Board control command failed on pinName=${pinNameOnTestBoard}, ${ret.error}`);
    }
}

async function deActivateDFU() {
    const ret = await testBoardLink.sendCommand(`setiopin ${testBoardLink.findPinIdByName(pinNameOnTestBoard)} 0`);
    if (!ret.status) {
        throw new Error(`Test Board control command failed on pinName=${pinNameOnTestBoard}, ${ret.error}`);
    }
}

module.exports = {
    activateDFU,
    deActivateDFU
};

'use strict';

const testBoardLink = require('../src/testBoardLink');

async function buzzerBeepSuccess() {
    const ret = await testBoardLink.sendCommand('buzzerbeep 0');
    if (!ret.status) {
        throw new Error(`Test Board buzzer failed ${ret.error}`);
    }
}

async function buzzerBeepFailed() {
    const ret = await testBoardLink.sendCommand('buzzerbeep 1');
    if (!ret.status) {
        throw new Error(`Test Board buzzer failed ${ret.error}`);
    }
}

module.exports = {
    buzzerBeepSuccess,
    buzzerBeepFailed
};

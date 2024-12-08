'use strict';

const MercuryBoard = require('../utils/mercuryBoard');

module.exports = class MnpTests {
    constructor(log) {
        this.logger = log;
        this.mnpReader1 = [
            // Reader 1
            { name: 'rd1rd0', pinType: 'OUTPUT', cmdWrite: 'rd1d0', pinN: 9, mnpPinName: 'WGD1_D0_3V3' },
            { name: 'rd1rd1', pinType: 'OUTPUT', cmdWrite: 'rd1d1', pinN: 10, mnpPinName: 'WGD1_D1_3V3' },
            { name: 'rd1rled', pinType: 'INPUT', cmdRead: 'Rd1Rled', pinN: 5, mnpPinName: 'RD1_RLED' },
            { name: 'rd1gled', pinType: 'INPUT', cmdRead: 'Rd1Gled', pinN: 11, mnpPinName: 'RD1_GLED' },
            { name: 'rd1bz', pinType: 'INPUT', cmdRead: 'rd1bz', pinN: 40, mnpPinName: 'WGD1_BPR' },
            // Reader 2
            { name: 'rd2rd0', pinType: 'OUTPUT', cmdWrite: 'rd2d0', pinN: 32, mnpPinName: 'WGD2_D0_3V3' },
            { name: 'rd2rd1', pinType: 'OUTPUT', cmdWrite: 'rd2d1', pinN: 26, mnpPinName: 'WGD2_D1_3V3' },
            { name: 'Rd2Rled', pinType: 'INPUT', cmdRead: 'Rd2Rled', pinN: 12, mnpPinName: 'RD2_RLED' },
            { name: 'Rd2Gled', pinType: 'INPUT', cmdRead: 'Rd2Gled', pinN: 30, mnpPinName: 'RD2_GLED' },
            { name: 'Rd2bz', pinType: 'INPUT', cmdRead: 'rd2bz', pinN: 39, mnpPinName: 'WGD2_BPR' },
            // Relay 1
            { name: 'ry1', pinType: 'INPUT', cmdRead: 'ry1', pinN: 23, mnpPinName: 'RLY1_EN' },
            // Relay 2
            { name: 'ry1', pinType: 'INPUT', cmdRead: 'ry2', pinN: 3, mnpPinName: 'RLY1_EN' },
            // Relay 3
            { name: 'ry1', pinType: 'INPUT', cmdRead: 'ry3', pinN: 33, mnpPinName: 'RLY1_EN' },
            // Relay 4
            { name: 'ry1', pinType: 'INPUT', cmdRead: 'ry4', pinN: 37, mnpPinName: 'RLY1_EN' }
        ];
    }

    async testme() {
        return this.mercuryBoard.sendCommand('rd2d0', 1);
    }

    async begin() {
        this.mercuryBoard = new MercuryBoard(this.logger);
        await this.mercuryBoard.begin();
    }

    async run() {
        return this.testme();
    }
};

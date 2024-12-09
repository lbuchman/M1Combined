'use strict';

const MercuryBoard = require('../utils/mercuryBoard');
const mnpHwIo = require('../tests/mnpHW');
const targetICTLink = require('../src/m1ICTLink');
const errorCodes = require('../bin/errorCodes');

module.exports = class MnpTests {
    constructor(db, log) {
        this.logger = log;
        this.db = db;
        this.mnpIo = [
            // Reader 1
            { name: 'rd1rd0', pinType: 'OUTPUT', cmdWrite: 'rd1d0', pinN: 9, mnpPinName: 'WGD1_D0_3V3', inverted: 1 },
            { name: 'rd1rd1', pinType: 'OUTPUT', cmdWrite: 'rd1d1', pinN: 10, mnpPinName: 'WGD1_D1_3V3', inverted: 1 },
            { name: 'rd1rled', pinType: 'INPUT', cmdRead: 'rd1rled', pinN: 5, mnpPinName: 'RD1_RLED', inverted: 0 },
            { name: 'rd1gled', pinType: 'INPUT', cmdRead: 'rd1gled', pinN: 11, mnpPinName: 'RD1_GLED', inverted: 0 },
            { name: 'rd1bz', pinType: 'INPUT', cmdRead: 'rd1bz', pinN: 40, mnpPinName: 'WGD1_BPR', inverted: 0 },
            // Reader 2
            { name: 'rd2rd0', pinType: 'OUTPUT', cmdWrite: 'rd2d0', pinN: 32, mnpPinName: 'WGD2_D0_3V3', inverted: 1 },
            { name: 'rd2rd1', pinType: 'OUTPUT', cmdWrite: 'rd2d1', pinN: 26, mnpPinName: 'WGD2_D1_3V3', inverted: 1 },
            { name: 'Rd2Rled', pinType: 'INPUT', cmdRead: 'rd2rled', pinN: 12, mnpPinName: 'RD2_RLED', inverted: 0 },
            { name: 'Rd2Gled', pinType: 'INPUT', cmdRead: 'rd2gled', pinN: 30, mnpPinName: 'RD2_GLED', inverted: 0 },
            { name: 'Rd2bz', pinType: 'INPUT', cmdRead: 'rd2bz', pinN: 39, mnpPinName: 'WGD2_BPR', inverted: 0 },
            // Relay 1
            { name: 'ry1', pinType: 'INPUT', cmdRead: 'ry1', pinN: 23, mnpPinName: 'RLY1_EN', inverted: 1 },
            // Relay 2
            { name: 'ry2', pinType: 'INPUT', cmdRead: 'ry2', pinN: 3, mnpPinName: 'RLY1_EN', inverted: 1 },
            // Relay 3
            // { name: 'ry3', pinType: 'INPUT', cmdRead: 'ry3', pinN: 33, mnpPinName: 'RLY1_EN', inverted: true },
            // Relay 4
            // { name: 'ry4', pinType: 'INPUT', cmdRead: 'ry4', pinN: 37, mnpPinName: 'RLY1_EN', inverted: true }
        ];
    }


    async begin() {
        this.mercuryBoard = new MercuryBoard(this.logger);
        await this.mercuryBoard.begin();
    }

    async testOutputLogicalState(thisIo, thisMnpIo, value, inverted) {
        await this.mercuryBoard.sendCommand(thisIo.cmdWrite, value);
        const command = mnpHwIo.getCommand('read', thisMnpIo.name, value, this.logger);
        const ret = await targetICTLink.sendCommand(command);
        // eslint-disable-next-line no-bitwise
        if (ret.value !== (value ^ inverted)) {
            this.db.updateErrorCode(process.env.serial, errorCodes.codes[thisIo.mnpPinName].errorCode, 'T');
            this.logger.error(`Failed ${thisIo.mnpPinName} test.`);
            return false;
        }
        return true;
    }


    async testInputLogicalState(thisIo, thisMnpIo, value, inverted) {
        const command = mnpHwIo.getCommand('write', thisMnpIo.name, value, this.logger);
        await targetICTLink.sendCommand(command);
        const ret = await this.mercuryBoard.sendCommand(thisIo.cmdRead, null);

        // eslint-disable-next-line no-bitwise
        if (ret.value !== (value ^ inverted)) {
            this.db.updateErrorCode(process.env.serial, errorCodes.codes[thisIo.mnpPinName].errorCode, 'T');
            this.logger.error(`Failed ${thisIo.mnpPinName} test.`);
            return false;
        }
        return true;
    }

    async run() {
        let ret = true;
        /* eslint-disable-next-line no-restricted-syntax */
        for (const thisIo of this.mnpIo) {
            const mnpPin = mnpHwIo.getIoMap().filter((item) => {
                return item.name === thisIo.mnpPinName;
            });
            // Perform asynchronous operations with 'element'
            if (mnpPin.length === 0) throw new Error(`Failed ${thisIo.mnpPinName} test. Cannot find pin name ${thisIo.mnpPinName} in MNP board io map`);
            const thisMnpIo = mnpPin[0];

            let testPassed = true;
            if (thisIo.pinType === 'OUTPUT') {
                // eslint-disable-next-line no-await-in-loop
                if (!await this.testOutputLogicalState(thisIo, thisMnpIo, 1, thisIo.inverted)) {
                    ret = false;
                    testPassed = false;
                }
                // eslint-disable-next-line no-await-in-loop
                if (!await this.testOutputLogicalState(thisIo, thisMnpIo, 0, thisIo.inverted)) {
                    ret = false;
                    testPassed = false;
                }
                if (testPassed) this.logger.info(`Passed ${thisMnpIo.name} test`);
            }
            else {
                // eslint-disable-next-line no-await-in-loop
                if (!await this.testInputLogicalState(thisIo, thisMnpIo, 1, thisIo.inverted)) {
                    ret = false;
                    testPassed = false;
                }
                // eslint-disable-next-line no-await-in-loop
                if (!await this.testInputLogicalState(thisIo, thisMnpIo, 0, thisIo.inverted)) {
                    ret = false;
                    testPassed = false;
                }
                if (testPassed) this.logger.info(`Passed ${thisMnpIo.name} test`);
            }
        }
        return ret;
    }
};

'use strict';

const MercurlBoard = require('./mercuryBoard');
const CommandHelper = require('./commandHelper');
const mnpHwIo = require('../tests/mnpHW');
const targetICTLink = require('../src/m1ICTLink');

class MercuryBoardHelper {
    constructor(logger, db) {
        this.logger = logger;
        this.db = db;
        this.commandHelper = new CommandHelper(logger, db);
        this.mercurlBoard = new MercurlBoard(logger);
    }

    async begin() {
        await this.mercurlBoard.begin();
    }

    async testOutputLogicalState(thisIo, thisMnpIo, value, inverted) {
        return await this.commandHelper.executeTest(
            async () => {
                await this.mercurlBoard.sendCommand(thisIo.cmdWrite, value);
                const command = mnpHwIo.getCommand('read', thisMnpIo.name, value, this.logger);
                const ret = await targetICTLink.sendCommand(command);

                // eslint-disable-next-line no-bitwise
                if (ret.value !== (value ^ inverted)) {
                    return { status: false, error: 'Value mismatch' };
                }
                return { status: true };
            },
            `${thisIo.mnpPinName} test`,
            thisIo.mnpPinName
        );
    }

    async testInputLogicalState(thisIo, thisMnpIo, value, inverted) {
        return await this.commandHelper.executeTest(
            async () => {
                const command = mnpHwIo.getCommand('write', thisMnpIo.name, value, this.logger);
                await targetICTLink.sendCommand(command);
                const ret = await this.mercurlBoard.sendCommand(thisIo.cmdRead, null);

                // eslint-disable-next-line no-bitwise
                if (ret.value !== (value ^ inverted)) {
                    return { status: false, error: 'Value mismatch' };
                }
                return { status: true };
            },
            `${thisIo.mnpPinName} test`,
            thisIo.mnpPinName
        );
    }
}

module.exports = MercuryBoardHelper;

'use strict';

const utils = require('../../utils/utils');

module.exports = class MacProvisioningService {
    constructor(logger) {
        this.logger = logger;
    }

    // eslint-disable-next-line class-methods-use-this
    async writeRandomOsdpKey(programmer, otpTransport) {
        const osdpKey = [60, 61, 62, 63];
        // eslint-disable-next-line no-restricted-syntax
        for (const address of osdpKey) {
            const value = `0x${utils.genRanHex(8)}`;
            // eslint-disable-next-line no-await-in-loop
            await otpTransport.writeLockedWord(programmer, address, value);
        }
    }

    // eslint-disable-next-line class-methods-use-this
    async readCurrentMacData(programmer, otpTransport) {
        const word57 = await otpTransport.readWord(programmer, utils.otp57);
        const word58 = await otpTransport.readWord(programmer, utils.otp58);
        const otp57Value = utils.getWordData(word57, utils.otp57);
        const otp58Value = utils.getWordData(word58, utils.otp58);
        const isBlank = (otp57Value === '0x00000000') || (otp58Value === '0x00000000');

        return {
            word57,
            word58,
            cpuSerial: utils.getCPUSerial(word57),
            isBlank,
            mac: isBlank ? null : utils.otpToMac(otp57Value, otp58Value)
        };
    }

    // eslint-disable-next-line class-methods-use-this
    async programAndVerifyMac(programmer, otpTransport, mac) {
        const progData = utils.macToOtp(mac);
        await otpTransport.writeLockedWord(programmer, utils.otp57, progData.oTp57);
        await otpTransport.writeLockedWord(programmer, utils.otp58, progData.oTp58);

        const rbword57 = await otpTransport.readWord(programmer, utils.otp57);
        const rbword58 = await otpTransport.readWord(programmer, utils.otp58);
        const isTheSame = utils.isMacTheSame(rbword57, rbword58, mac.toUpperCase());

        return {
            isTheSame,
            rbword57,
            rbword58
        };
    }
};

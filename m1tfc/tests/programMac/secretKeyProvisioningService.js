'use strict';

const { randomInt } = require('crypto');
const utils = require('../../utils/utils');

module.exports = class SecretKeyProvisioningService {
    constructor(logger) {
        this.logger = logger;
    }

    // eslint-disable-next-line class-methods-use-this
    createSecretKey() {
        return {
            word0: randomInt(0x100000000),
            word1: randomInt(0x100000000),
            word2: randomInt(0x100000000),
            word3: randomInt(0x100000000)
        };
    }

    // eslint-disable-next-line class-methods-use-this
    async readBackSecretKey(programmer, otpTransport) {
        const word60Tmp = await otpTransport.readWord(programmer, utils.otp60);
        const word61Tmp = await otpTransport.readWord(programmer, utils.otp61);
        const word62Tmp = await otpTransport.readWord(programmer, utils.otp62);
        const word63Tmp = await otpTransport.readWord(programmer, utils.otp63);

        return {
            word0: utils.getWordData(word60Tmp, utils.otp60),
            word1: utils.getWordData(word61Tmp, utils.otp61),
            word2: utils.getWordData(word62Tmp, utils.otp62),
            word3: utils.getWordData(word63Tmp, utils.otp63)
        };
    }

    // eslint-disable-next-line class-methods-use-this
    isBlank(readBackSecretKey) {
        return (
            readBackSecretKey.word3 === '0x00000000' &&
            readBackSecretKey.word2 === '0x00000000' &&
            readBackSecretKey.word1 === '0x00000000' &&
            readBackSecretKey.word0 === '0x00000000'
        );
    }

    // eslint-disable-next-line class-methods-use-this
    async programAndVerifySecretKey(programmer, otpTransport, secretKey) {
        await otpTransport.writeLockedWord(
            programmer,
            utils.otp60,
            `0x${secretKey.word0.toString(16)}`
        );
        await otpTransport.writeLockedWord(
            programmer,
            utils.otp61,
            `0x${secretKey.word1.toString(16)}`
        );
        await otpTransport.writeLockedWord(
            programmer,
            utils.otp62,
            `0x${secretKey.word2.toString(16)}`
        );
        await otpTransport.writeLockedWord(
            programmer,
            utils.otp63,
            `0x${secretKey.word3.toString(16)}`
        );

        const word0 = await otpTransport.readWord(programmer, utils.otp60);
        const word1 = await otpTransport.readWord(programmer, utils.otp61);
        const word2 = await otpTransport.readWord(programmer, utils.otp62);
        const word3 = await otpTransport.readWord(programmer, utils.otp63);

        const isVerified =
            utils.getWordData(word3, utils.otp63).toLowerCase() ===
                `0x${secretKey.word3.toString(16)}` &&
            utils.getWordData(word2, utils.otp62).toLowerCase() ===
                `0x${secretKey.word2.toString(16)}` &&
            utils.getWordData(word1, utils.otp61).toLowerCase() ===
                `0x${secretKey.word1.toString(16)}` &&
            utils.getWordData(word0, utils.otp60).toLowerCase() ===
                `0x${secretKey.word0.toString(16)}`;

        return isVerified;
    }
};

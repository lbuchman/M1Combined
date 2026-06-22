'use strict';

const path = require('path');
const delay = require('delay');
const common = require('../../tests/common');
const os = require('../../utils/os');

const fwDir = `${path.resolve(__dirname)}/../../fw`;
const minimalTsv = `${fwDir}/flashlayout_st-ls2m1-image-core/trusted/minimal.tsv`;

module.exports = class OtpTransport {
    constructor(logger) {
        this.logger = logger;
    }

    async prepareSession(programmer) {
        await common.initializeTestFixture(null, null, false, null, null, null, this.logger, null, null);
        this.logger.debug('Wait for DFU ...');
        await common.waitDFU(programmer, this.logger);
        this.logger.debug('Programming TSV file ...');
        await os.executeShellCommand(`${programmer}  -c port=usb1 -d ${minimalTsv}`, this.logger, false, false, 1024 * 200, fwDir);
        await delay(100);
    }

    async readWord(programmer, address) {
        return os.executeShellCommand(`${programmer}  -c port=usb1  -otp displ word=${address}`, this.logger, false);
    }

    async writeLockedWord(programmer, address, value) {
        return os.executeShellCommand(`${programmer}  -c port=usb1 -c port=USB1 -y -otp write lock word=${address} value=${value}`, this.logger, false);
    }
};

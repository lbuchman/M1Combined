'use strict';

const common = require('../tests/common');
const testBoardLink = require('../src/testBoardLink');
const delay = require('delay');
const os = require('../utils/os');
const utils = require('../utils/utils');
const exitCodes = require('../src/exitCodes');
const sqliteDriver = require('../utils/sqliteDriver');
const tsv = require('../utils/tsv');

const log = console;
module.exports = class ProgramMac {
    constructor(config, serial, log_) {
        this.logger = log_;
        this.tsv = config.layoutFilePath;
        this.serial = serial;
        this.config = config;
    }

    /**
      * @public
      *
      * @param {object} log
      */
    async init(teensyDev, teensyDevBaudrate) {
        await testBoardLink.initSerial(teensyDev, teensyDevBaudrate, this.logger);
    }

    /**
      * @public
      *
      * @param
      */
    async run(programmer, config) {
        try {
            const fwDir = process.env.fwDir;
            const db = sqliteDriver.initialize(this.logger);
            db.updateSerial(this.serial);
            await common.initializeTestFixture(null, false, null, null, this.logger);
            this.logger.debug('Programming TSV file ...');
            const minimalTsv = tsv.makeMinimalTsv(this.tsv);
            await common.waitDFU(programmer, this.logger);
            await os.executeShellCommand(`${programmer}  -c port=usb1 -d ${minimalTsv}`, this.logger, false, false, 1024 * 200, fwDir);
            await delay(100);

            this.logger.debug('Reading current OTP values ...');
            const word57 = await os.executeShellCommand(`${programmer}  -c port=usb1  -otp displ word=${utils.otp57}`, this.logger, false);
            const word58 = await os.executeShellCommand(`${programmer}  -c port=usb1  -otp displ word=${utils.otp58}`, this.logger, false);
            const cpuSerial = utils.getCPUSerial(word57);
            if (!this.config.testNewMac) db.updateCPUSerial(this.serial, cpuSerial);
            if (!this.config.testNewMac && (utils.getWordData(word57, utils.otp57) !== '0x00000000') && (utils.getWordData(word58, utils.otp58) !== '0x00000000')) {
                const otpToMac = utils.otpToMac(utils.getWordData(word57, utils.otp57), utils.getWordData(word58, utils.otp58));
                this.logger.warn(`OTP is not blank MAC Address is: ${otpToMac}`);
                const dbRecord = db.getRecordFromMac(otpToMac);
                if (dbRecord.length) {
                    const entry = dbRecord[0];
                    if (entry.vendorSerial !== this.serial) throw Error('Board MAC is recorder under a different vendorSerial');
                }
                this.mac = otpToMac;
                await common.testEndSuccess();
                await delay(300);
                db.updateUid(this.serial, otpToMac);
                const exitCode = exitCodes.normalExit;
                return ({ exitCode, mac: this.mac });
            }

            const mac = utils.getNextMac(db.getLastUsedMac());
            if (!utils.isString(mac)) throw new Error('DA Error, cannot get next MAC');
            const progData = utils.macToOtp(mac);
            this.logger.info('Programing OTP values ...');
            await os.executeShellCommand(`${programmer}  -c port=usb1 -c port=USB1 -y -otp write lock word=${utils.otp57} value=${progData.oTp57}`, this.logger, false);
            await os.executeShellCommand(`${programmer}  -c port=usb1 -c port=USB1 -y -otp write lock word=${utils.otp58} value=${progData.oTp58}`, this.logger, false);
            this.logger.debug('Verifying OTP values ...');
            const rbword57 = await os.executeShellCommand(`${programmer}  -c port=usb1 -c port=USB1 -otp displ word=${utils.otp57}`, this.logger, false);
            const rbword58 = await os.executeShellCommand(`${programmer}  -c port=usb1 -c port=USB1 -otp displ word=${utils.otp58}`, this.logger, false);

            if (!utils.isMacTheSame(rbword57, rbword58, mac.toUpperCase())) {
                this.logger.error(`OTP compare failed!!! programmed ${mac.toUpperCase()}, read back ${utils.otpToMac(utils.getWordData(rbword57, utils.otp57), utils.getWordData(rbword58, utils.otp58))}`);
                await common.testFailed();
                const throwError = new Error('OTP (MAC) compare failed');
                throwError.level = exitCodes.programMacFailed;
                throw throwError;
            }
            this.logger.info(`MAC: ${mac} programmed`);
            this.logger.debug('adding MAC to DB');
            db.updateUid(this.serial, mac);
            db.updateLastUsedMac(mac);
            this.logger.info('MAC programming is done and verified!!!');
            this.logger.info(`MAC Address is: ${mac}`);
            this.mac = mac;
            await common.testEndSuccess();
            const exitCode = exitCodes.normalExit;
            return ({ exitCode, mac: this.mac });
        }
        catch (err) {
            // if (err.stack) this.logger.debug(err.stack);
            await common.testFailed();
            const throwError = new Error(err.message);
            throwError.level = exitCodes.programMacFailed;
            throw throwError;
        }
    }

  /**
  * @public
  *
  * @param
  */
    async getMac(programmer, readOnly = false) {
        const fwDir = process.env.fwDir;
        await common.initializeTestFixture(null, false, null, null, this.logger);
        this.logger.debug('Programming TSV file ...');
        const minimalTsv = tsv.makeMinimalTsv(this.tsv);
        await common.waitDFU(programmer, this.logger);
        await os.executeShellCommand(`${programmer}  -c port=usb1 -d ${minimalTsv}`, this.logger, false, false, 1024 * 200, fwDir);
        await delay(100);

        this.logger.debug('Reading current OTP values ...');
        const word57 = await os.executeShellCommand(`${programmer}  -c port=usb1  -otp displ word=${utils.otp57}`, this.logger, false);
        const word58 = await os.executeShellCommand(`${programmer}  -c port=usb1  -otp displ word=${utils.otp58}`, this.logger, false);
        if ((utils.getWordData(word57, utils.otp57) !== '0x00000000') || (utils.getWordData(word58, utils.otp58) !== '0x00000000')) {
            const otpToMac = utils.otpToMac(utils.getWordData(word57, utils.otp57), utils.getWordData(word58, utils.otp58));
            this.mac = otpToMac;
            if (readOnly) log.info(this.mac);
            await common.testEndSuccess();
            await delay(300);
            const exitCode = exitCodes.normalExit;
            return ({ exitCode, mac: this.mac });
        }
        if (readOnly) log.info('00:00:00:00:00:00');
        await common.testEndSuccess();
        const exitCode = exitCodes.normalExit;
        return ({ exitCode, mac: this.mac });
    }
};

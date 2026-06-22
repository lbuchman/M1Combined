'use strict';

const common = require('../tests/common');
const testBoardLink = require('../src/testBoardLink');
const delay = require('delay');
const utils = require('../utils/utils');
const exitCodes = require('../src/exitCodes');
const sqliteDriver = require('../utils/sqliteDriver');
const OtpTransport = require('../tests/programMac/otpTransport');
const MacProvisioningService = require('../tests/programMac/macProvisioningService');
const SecretKeyProvisioningService = require('../tests/programMac/secretKeyProvisioningService');

// const log = console;
module.exports = class ProgramMac {
    constructor(config, serial, log_) {
        this.logger = log_;
        this.tsv = `${config.mtfDir}/${config.fwDir}/${config.layoutFilePath}`;
        this.serial = serial;
        this.config = config;
        this.otpTransport = new OtpTransport(this.logger);
        this.macProvisioning = new MacProvisioningService(this.logger);
        this.secretKeyProvisioning = new SecretKeyProvisioningService(this.logger);
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
    async run(programmer) {
        try {
            const db = sqliteDriver.initialize(this.logger);
            db.updateSerial(this.serial);
            await this.otpTransport.prepareSession(programmer);

            this.logger.debug('Reading current OTP values ...');
            const currentMacData = await this.macProvisioning.readCurrentMacData(programmer, this.otpTransport);
            await this.macProvisioning.writeRandomOsdpKey(programmer, this.otpTransport);

            const cpuSerial = currentMacData.cpuSerial;
            if (!this.config.testNewMac) db.updateCPUSerial(this.serial, cpuSerial);
            if (!this.config.testNewMac && !currentMacData.isBlank) {
                const otpToMac = currentMacData.mac;
                this.logger.info(`OTP is not blank MAC Address is: ${otpToMac}`);
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
            await this.otpTransport.writeLockedWord(programmer, utils.otp57, progData.oTp57);
            await this.otpTransport.writeLockedWord(programmer, utils.otp58, progData.oTp58);
            this.logger.debug('Verifying OTP values ...');
            const verification = await this.macProvisioning.programAndVerifyMac(programmer, this.otpTransport, mac);
            const rbword57 = verification.rbword57;
            const rbword58 = verification.rbword58;

            if (!verification.isTheSame) {
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
    async getMac(programmer) {
        await this.otpTransport.prepareSession(programmer);

        this.logger.debug('Reading current OTP values ...');
        const currentMacData = await this.macProvisioning.readCurrentMacData(programmer, this.otpTransport);
        if (!currentMacData.isBlank) {
            const otpToMac = currentMacData.mac;
            this.mac = otpToMac;
            await common.testEndSuccess();
            await delay(300);
            const exitCode = exitCodes.normalExit;
            return ({ exitCode, mac: this.mac });
        }
        await common.testEndSuccess();
        const exitCode = exitCodes.normalExit;
        return ({ exitCode, mac: this.mac });
    }

    /**
 * @public
 *
 * @param
 */
    async getSecreteKey(programmer) {
        await this.otpTransport.prepareSession(programmer);

        this.logger.debug('Reading current OTP values ...');
        const readBackSecretKey = await this.secretKeyProvisioning.readBackSecretKey(programmer, this.otpTransport);

        if (!this.secretKeyProvisioning.isBlank(readBackSecretKey)) {
            const otpToMac = utils.otpToMac(readBackSecretKey.word2, readBackSecretKey.word3);
            this.mac = otpToMac;
            await common.testEndSuccess();
            await delay(300);
            const exitCode = exitCodes.normalExit;
            return ({ exitCode, mac: this.mac });
        }
        await common.testEndSuccess();
        const exitCode = exitCodes.normalExit;
        return ({ exitCode, mac: this.mac });
    }

    /**
  * @public
  *
  * @param
  */
    async runProgSecret(programmer) {
        try {
            const secretKey = this.secretKeyProvisioning.createSecretKey();
            const db = sqliteDriver.initialize(this.logger);
            const exitCode = exitCodes.normalExit;
            db.updateSerial(this.serial);
            await this.otpTransport.prepareSession(programmer);
            this.logger.debug('Reading current OTP values for Secrete Key...');
            const readBackSecretKey = await this.secretKeyProvisioning.readBackSecretKey(programmer, this.otpTransport);
            this.logger.info(`Read OTP Secret Key ${readBackSecretKey.word3}.${readBackSecretKey.word2}.${readBackSecretKey.word1}.${readBackSecretKey.word0}`);
            if (!this.secretKeyProvisioning.isBlank(readBackSecretKey)) {
                this.logger.info('Secret Key OTP is already programmed ');
                await common.testEndSuccess();
                await delay(300);
                return ({ exitCode, readBackSecretKey });
            }

            this.logger.info(`Programing Secret Key OTP values ${secretKey.word3}.${secretKey.word2}.${secretKey.word1}.${secretKey.word0}`);
            const isVerified = await this.secretKeyProvisioning.programAndVerifySecretKey(programmer, this.otpTransport, secretKey);
            this.logger.debug('Verifying Secret Key OTP values ...');
            if (!isVerified) {
                this.logger.error('Secretekey in OTP compare failed!!!)');
                await common.testFailed();
                const throwError = new Error('OTP (SecretKey) compare failed');
                throwError.level = exitCodes.programMacFailed;
                throw throwError;
            }
            this.logger.info('Secret Key: is programmed');

            await common.testEndSuccess();
            return ({ exitCode, secretKey });
        }
        catch (err) {
            // if (err.stack) this.logger.debug(err.stack);
            await common.testFailed();
            const throwError = new Error(err.message);
            throwError.level = exitCodes.programMacFailed;
            throw throwError;
        }
    }

    getRandomInt() {
        const maxInt4Bytes = 4294967295;
        this.secretKey = Math.floor(Math.random() * maxInt4Bytes);
        return this.secretKey;
    }
};

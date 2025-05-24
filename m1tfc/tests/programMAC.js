'use strict';

const common = require('../tests/common');
const testBoardLink = require('../src/testBoardLink');
const delay = require('delay');
const path = require('path');
const os = require('../utils/os');
const utils = require('../utils/utils');
const exitCodes = require('../src/exitCodes');
const sqliteDriver = require('../utils/sqliteDriver');

// const log = console;
module.exports = class ProgramMac {
    constructor(config, serial, log_) {
        this.logger = log_;
        this.tsv = `${config.mtfDir}/${config.fwDir}/${config.layoutFilePath}`;
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
    async run(programmer) {
        try {
            const db = sqliteDriver.initialize(this.logger);
            db.updateSerial(this.serial);
            await common.initializeTestFixture(null, false, null, null, this.logger);
            this.logger.debug('Wait for DFU ...');
            await common.waitDFU(programmer, this.logger);
            this.logger.debug('Programming TSV file ...');
            const fwDir = `${path.resolve(__dirname)}/../fw`;
            const minimalTsv = `${fwDir}/flashlayout_st-ls2m1-image-core/trusted/minimal.tsv`;
            await os.executeShellCommand(`${programmer}  -c port=usb1 -d ${minimalTsv}`, this.logger, false, false, 1024 * 200, fwDir);
            await delay(100);

            this.logger.debug('Reading current OTP values ...');
            const word57 = await os.executeShellCommand(`${programmer}  -c port=usb1  -otp displ word=${utils.otp57}`, this.logger, false);
            const word58 = await os.executeShellCommand(`${programmer}  -c port=usb1  -otp displ word=${utils.otp58}`, this.logger, false);
            const cpuSerial = utils.getCPUSerial(word57);
            if (!this.config.testNewMac) db.updateCPUSerial(this.serial, cpuSerial);
            if (!this.config.testNewMac && (utils.getWordData(word57, utils.otp57) !== '0x00000000') && (utils.getWordData(word58, utils.otp58) !== '0x00000000')) {
                const otpToMac = utils.otpToMac(utils.getWordData(word57, utils.otp57), utils.getWordData(word58, utils.otp58));
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
    async getMac(programmer) {
        await common.initializeTestFixture(null, false, null, null, this.logger);
        this.logger.debug('Programming TSV file ...');
        await common.waitDFU(programmer, this.logger);
        const fwDir = `${path.resolve(__dirname)}/../fw`;
        const minimalTsv = `${fwDir}/flashlayout_st-ls2m1-image-core/trusted/minimal.tsv`;
        await os.executeShellCommand(`${programmer}  -c port=usb1 -d ${minimalTsv}`, this.logger, false, false, 1024 * 200, fwDir);
        await delay(100);

        this.logger.debug('Reading current OTP values ...');
        const word57 = await os.executeShellCommand(`${programmer}  -c port=usb1  -otp displ word=${utils.otp57}`, this.logger, false);
        const word58 = await os.executeShellCommand(`${programmer}  -c port=usb1  -otp displ word=${utils.otp58}`, this.logger, false);
        if ((utils.getWordData(word57, utils.otp57) !== '0x00000000') || (utils.getWordData(word58, utils.otp58) !== '0x00000000')) {
            const otpToMac = utils.otpToMac(utils.getWordData(word57, utils.otp57), utils.getWordData(word58, utils.otp58));
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
        await common.initializeTestFixture(null, false, null, null, this.logger);
        this.logger.debug('Programming TSV file ...');
        await common.waitDFU(programmer, this.logger);
        const fwDir = `${path.resolve(__dirname)}/../fw`;
        const minimalTsv = `${fwDir}/flashlayout_st-ls2m1-image-core/trusted/minimal.tsv`;
        await os.executeShellCommand(`${programmer}  -c port=usb1 -d ${minimalTsv}`, this.logger, false, false, 1024 * 200, fwDir);
        await delay(100);

        this.logger.debug('Reading current OTP values ...');
        const word60 = await os.executeShellCommand(`${programmer}  -c port=usb1  -otp displ word=${utils.otp60}`, this.logger, false);
        const word61 = await os.executeShellCommand(`${programmer}  -c port=usb1  -otp displ word=${utils.otp61}`, this.logger, false);
        const word62 = await os.executeShellCommand(`${programmer}  -c port=usb1  -otp displ word=${utils.otp62}`, this.logger, false);
        const word63 = await os.executeShellCommand(`${programmer}  -c port=usb1  -otp displ word=${utils.ot63}`, this.logger, false);

        if ((utils.getWordData(word60, utils.otp60) !== '0x00000000') || (utils.getWordData(word61, utils.otp61) !== '0x00000000') || (utils.getWordData(word62, utils.otp62) !== '0x00000000') || (utils.getWordData(word63, utils.otp63) !== '0x00000000')) {
            const otpToMac = utils.otpToMac(utils.getWordData(word62, utils.otp62), utils.getWordData(word63, utils.otp63));
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
            const secretKey = { word0: this.getRandomInt(), word1: this.getRandomInt(), word2: this.getRandomInt(), word3: this.getRandomInt() };
            const db = sqliteDriver.initialize(this.logger);
            const exitCode = exitCodes.normalExit;
            db.updateSerial(this.serial);
            await common.initializeTestFixture(null, false, null, null, this.logger);
            this.logger.debug('Wait for DFU ...');
            await common.waitDFU(programmer, this.logger);
            this.logger.debug('Programming TSV file ...');
            const fwDir = `${path.resolve(__dirname)}/../fw`;
            const minimalTsv = `${fwDir}/flashlayout_st-ls2m1-image-core/trusted/minimal.tsv`;
            await os.executeShellCommand(`${programmer}  -c port=usb1 -d ${minimalTsv}`, this.logger, false, false, 1024 * 200, fwDir);
            await delay(100);
            this.logger.debug('Reading current OTP values for Secrete Key...');
            const word60Tmp = await os.executeShellCommand(`${programmer}  -c port=usb1  -otp displ word=${utils.otp60}`, this.logger, false);
            const word61Tmp = await os.executeShellCommand(`${programmer}  -c port=usb1  -otp displ word=${utils.otp61}`, this.logger, false);
            const word62Tmp = await os.executeShellCommand(`${programmer}  -c port=usb1  -otp displ word=${utils.otp62}`, this.logger, false);
            const word63Tmp = await os.executeShellCommand(`${programmer}  -c port=usb1  -otp displ word=${utils.otp63}`, this.logger, false);
            const readBackSecretKey = {};
            readBackSecretKey.word0 = utils.getWordData(word60Tmp, utils.otp60);
            readBackSecretKey.word1 = utils.getWordData(word61Tmp, utils.otp61);
            readBackSecretKey.word2 = utils.getWordData(word62Tmp, utils.otp62);
            readBackSecretKey.word3 = utils.getWordData(word63Tmp, utils.otp63);
            this.logger.info(`Read OTP Secret Key ${readBackSecretKey.word3}.${readBackSecretKey.word2}.${readBackSecretKey.word1}.${readBackSecretKey.word0}`);
            if (readBackSecretKey.word3 === '0x00000000' && readBackSecretKey.word2 === '0x00000000' && readBackSecretKey.word1 === '0x00000000' && readBackSecretKey.word0 === '0x00000000') {
                await common.testEndSuccess();
                await delay(300);
                return ({ exitCode, readBackSecretKey });
            }

            this.logger.info(`Programing Secret Key OTP values ${secretKey.word3}.${secretKey.word2}.${secretKey.word1}.${secretKey.word0}`);
            await os.executeShellCommand(`${programmer}  -c port=usb1 -c port=USB1 -y -otp write lock word=${utils.otp60} value=${secretKey.word0}`, this.logger, false);
            await os.executeShellCommand(`${programmer}  -c port=usb1 -c port=USB1 -y -otp write lock word=${utils.otp61} value=${secretKey.word1}`, this.logger, false);
            await os.executeShellCommand(`${programmer}  -c port=usb1 -c port=USB1 -y -otp write lock word=${utils.otp62} value=${secretKey.word2}`, this.logger, false);
            await os.executeShellCommand(`${programmer}  -c port=usb1 -c port=USB1 -y -otp write lock word=${utils.otp63} value=${secretKey.word3}`, this.logger, false);
            this.logger.debug('Verifying Secret Key OTP values ...');
            const word0 = await os.executeShellCommand(`${programmer}  -c port=usb1 -c port=USB1 -otp displ word=${utils.otp60}`, this.logger, false);
            const word1 = await os.executeShellCommand(`${programmer}  -c port=usb1 -c port=USB1 -otp displ word=${utils.otp61}`, this.logger, false);
            const word2 = await os.executeShellCommand(`${programmer}  -c port=usb1 -c port=USB1 -otp displ word=${utils.otp62}`, this.logger, false);
            const word3 = await os.executeShellCommand(`${programmer}  -c port=usb1 -c port=USB1 -otp displ word=${utils.otp63}`, this.logger, false);
            if (word3 !== secretKey.word3 || word2 !== secretKey.word2 || word1 !== secretKey.word1 || word0 !== secretKey.word0) {
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

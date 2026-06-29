'use strict';

const lodash = require('lodash');
const errorCodes = require('../bin/errorCodes');
const CommandHelper = require('./commandHelper');
const utils = require('./utils');
const { delay } = require('lodash');

class EepromHelper {
    constructor(targetICTLink, logger, db) {
        this.targetICTLink = targetICTLink;
        this.logger = logger;
        this.db = db;
        this.commandHelper = new CommandHelper(logger, db);
    }

    _getSecret(size) {
        return [...Array(size)].map(() => Math.floor(Math.random() * 16).toString(16)).join('');
    }

    _getSerialN(serial, vendorSite) {
        return `${serial}${vendorSite}`;
    }

    async checkEEPROM() {
        return await this.commandHelper.executeTest(
            () => this.targetICTLink.sendCommand('checkeeeprom'),
            'I2C EEPROM test',
            'EEPROM'
        );
    }

    async getVerifyEEPRomData() {
        const ret = await this.targetICTLink.sendCommand('verifyeepromdata');
        if (!ret.status) {
            this.logger.error(`I2C EEPROM verifyeepromdata cmd failed  ${ret.error}`);
            return false;
        }
        return ret;
    }

    async getEEPRomData() {
        const ret = await this.targetICTLink.sendCommand('verifyeepromdata');
        if (!ret.status) {
            if (ret.serial) {
                ret.status = 'HASH is Incorrect';
                await delay(100);
                throw new Error('HASH is Incorrect');
            }
            throw new Error(`I2C EEPROM verifyeepromdata cmd failed  ${ret.error}`);
        }
        return ret;
    }

    async updateEEPRom(serial, eepromoverwrite, vendorSite) {
        const dbRecord = this.db.getRecord(serial);
        utils.checkDbRecord(dbRecord, false);
        this.db.updateSerial(serial);

        const overwrite = eepromoverwrite ? 1 : 0;
        this.logger.info('Updating I2C EEPROM Data ...');

        const ret = await this.targetICTLink.sendCommand(
            `writeeepromdata SN=${this._getSerialN(serial, vendorSite)} SK=${this._getSecret(32)} ${overwrite}`
        );

        if (!ret.status) {
            if (ret.error === 'EEPROM is not Blank' && !overwrite) {
                this.logger.warn(`I2C EEPROM programming is rejected, ${ret.error}`);
                const eepromData = await this.getVerifyEEPRomData();
                this.logger.info('Verifying Data in I2C EEPROM');

                if (!eepromData || !eepromData.status) {
                    this.logger.error(`The Data: ${JSON.stringify(eepromData).trim()}`);
                    throw new Error('I2C EEPROM Data is Invalid');
                }

                delete eepromData.status;
                this.logger.info('The EEPROM Data is valid');
                const eepromSerial = eepromData.serial.substring(3).slice(0, -2);

                if (eepromSerial !== serial) {
                    this.db.updateErrorCode(
                        serial,
                        errorCodes.codes['SERIAL_MISSMATH'].errorCode,
                        'E'
                    );
                    throw new Error(
                        `serial number ${serial} does not match EEPROM value ${eepromData.serial.substring(3).slice(0, -2)}`
                    );
                }

                this.db.updateEepromData(serial, eepromData.secret, eepromData.serial);
            }
            return;
        }

        delete ret.status;
        const eepromData = await this.getVerifyEEPRomData();

        if (!eepromData || !eepromData.status) {
            this.logger.error(`Data returned: ${JSON.stringify(eepromData).trim()}`);
            throw new Error('I2C EEPROM check integrity failed');
        }

        delete eepromData.status;

        if (!lodash.isEqual(ret, eepromData)) {
            this.logger.error(`Expected: ${JSON.stringify(eepromData).trim()}`);
            this.logger.error(`Readback: ${JSON.stringify(ret).trim()}`);
            throw new Error('I2C EEPROM verify failed');
        }

        this.db.updateEepromData(serial, eepromData.secret, eepromData.serial);
        this.logger.debug(`Programmed: ${JSON.stringify(ret).trim()}`);
        this.logger.info('Passed update I2C EEPROM');
    }
}

module.exports = EepromHelper;

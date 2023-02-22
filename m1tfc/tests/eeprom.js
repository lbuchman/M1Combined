'use strict';

const targetICTLink = require('../src/m1ICTLink');
const lodash = require('lodash');
const sqliteDriver = require('../utils/sqliteDriver');
const { delay } = require('lodash');
const utils = require('../utils/utils');

function getSecret(size) {
    return [...Array(size)].map(() => Math.floor(Math.random() * 16).toString(16)).join('');
}

function getWeek() {
    const date = new Date(new Date().getTime());
    date.setHours(0, 0, 0, 0);
    // Thursday in current week decides the year.
    // eslint-disable-next-line  no-mixed-operators
    date.setDate(date.getDate() + 3 - (date.getDay() + 6) % 7);
    // January 4 is always in week 1.
    const week1 = new Date(date.getFullYear(), 0, 4);
    // Adjust to Thursday in week 1 and count number of weeks from date to week1.
    // eslint-disable-next-line  no-mixed-operators
    return 1 + Math.round(((date.getTime() - week1.getTime()) / 86400000 - 3 + (week1.getDay() + 6) % 7) / 7);
}


async function checkEEPROM(logger) {
    const ret = await targetICTLink.sendCommand('checkeeeprom');
    if (!ret.status) {
        logger.error(`I2C EEPROM Test failed  ${ret.error}`);
        return false;
    }

    logger.info('Passed I2C EEPROM test');
    return true;
}


async function getVerifyEEPRomData(logger) {
    const ret = await targetICTLink.sendCommand('verifyeepromdata');
    if (!ret.status) {
        logger.error(`I2C EEPROM verifyeepromdata cmd failed  ${ret.error}`);
        return false;
    }
    return ret;
}

async function getEEPRomData() {
    const ret = await targetICTLink.sendCommand('verifyeepromdata');
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

async function printEEPRomData(logger, console) {
    const eeprom = await this.getEEPRomData();
    delete eeprom.status;
    console.info(`${JSON.stringify(eeprom).trim()}`);
}

function getSerialN(serial, vendorSite) {
    const today = new Date();
    const year = today.getYear() - 100;
    const week = getWeek();
    return `30${year}${week}${serial.substr(-4)}${vendorSite}`;
}

async function updateEEPRom(serial, eeeproverwrite, vendorSite, logger) {
    const db = sqliteDriver.initialize(logger);
    const dbRecord = db.getRecord(serial);
    utils.checkDbRecord(dbRecord, false);
    db.updateSerial(serial);
    const overwrite = eeeproverwrite ? 1 : 0;
    logger.info('Updating I2C EEPROM Data ...');
    const ret = await targetICTLink.sendCommand(`writeeepromdata SN=${getSerialN(serial, vendorSite)} SK=${getSecret(32)} ${overwrite}`);
    if (!ret.status) {
        if (ret.error === 'EEPROM is not Blank' && !overwrite) {
            logger.warn(`I2C EEPROM programming is rejected, ${ret.error}`);
            const eepromData = await getVerifyEEPRomData(logger);
            logger.info('Verifying Data in I2C EEPROM');
            if (!eepromData.status) {
                logger.error(`The Data: ${JSON.stringify(eepromData).trim()}`);
                throw new Error('I2C EEPROM Data is Invalid');
            }
            delete eepromData.status;
            logger.info(`The Data is OK: ${JSON.stringify(eepromData).trim()}`);
            db.updateEepromData(serial, eepromData.secret, eepromData.serial);
        }

        return;
    }

    delete ret.status;
    const eepromData = await getVerifyEEPRomData(logger);
    if (!eepromData.status) {
        logger.error(`Data returned: ${JSON.stringify(eepromData).trim()}`);
        throw new Error('I2C EEPROM check integrity failed');
    }
    delete eepromData.status;
    if (!lodash.isEqual(ret, eepromData)) {
        logger.error(`Expected: ${JSON.stringify(eepromData).trim()}`);
        logger.error(`Readback: ${JSON.stringify(ret).trim()}`);
        throw new Error('I2C EEPROM verify failed');
    }
    db.updateEepromData(serial, eepromData.secret, eepromData.serial);
    logger.info(`Programmed: ${JSON.stringify(ret).trim()}`);
    logger.info('Passed update I2C EEPROM');
}

module.exports = {
    checkEEPROM,
    printEEPRomData,
    updateEEPRom,
    getEEPRomData
};

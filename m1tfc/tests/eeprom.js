'use strict';

const targetICTLink = require('../src/m1ICTLink');
const EepromHelper = require('../utils/eepromHelper');
const sqliteDriver = require('../utils/sqliteDriver');

async function checkEEPROM(logger, db) {
    const eepromHelper = new EepromHelper(targetICTLink, logger, db);
    return await eepromHelper.checkEEPROM();
}

async function getEEPRomData() {
    // Only requires the link
    const eepromHelper = new EepromHelper(targetICTLink, console, null);
    return await eepromHelper.getEEPRomData();
}

async function updateEEPRom(serial, eepromoverwrite, vendorSite, logger) {
    // Needs db to update serial
    const db = sqliteDriver.initialize(logger);
    const eepromHelper = new EepromHelper(targetICTLink, logger, db);
    await eepromHelper.updateEEPRom(serial, eepromoverwrite, vendorSite);
}

module.exports = {
    checkEEPROM,
    updateEEPRom,
    getEEPRomData
};

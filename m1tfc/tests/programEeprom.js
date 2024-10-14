'use strict';

const testBoardLink = require('../src/testBoardLink');
const common = require('../tests/common');
const buzzer = require('./buzzer');
const eeprom = require('./eeprom');
const exitCodes = require('../src/exitCodes');

module.exports = class IctTestRunner {
    constructor(stm32, log) {
        this.logger = log;
        this.stm32 = stm32;
        this.secret = '';
        this.serial = '';
    }

    /**
      * @public
      *
      * @param {object} log
      */
    async init(teensyDev, teensyDevBaudrate, m1Dev, m1DevBaudrate) {
        await testBoardLink.initSerial(teensyDev, teensyDevBaudrate, this.logger);
        this.m1Dev = m1Dev;
        this.m1DevBaudrate = m1DevBaudrate;
    }

    /**
      * @public
      *
      * @param
      */
    async program(programmer, serial, vendorSite, eeepromoverwrite = false) {
        try {
            this.logger.info('Updating I2C EEPROM ...');
            await common.initializeTestFixture(programmer, true, this.stm32, this.m1Dev, this.logger, false);
            return await eeprom.updateEEPRom(serial, eeepromoverwrite, vendorSite, this.logger);
        }
        catch (err) {
            // if (err.stack) this.logger.debug(err.stack);
            await common.testFailed();
            const error = new Error(err.message);
            await buzzer.buzzerBeepFailed();
            error.level = exitCodes.programEepromFailed;
            throw error;
        }
    }

    /**
      * @public
      *
      * @param
      */
    async print(programmer) {
        try {
            this.logger.debug('reading I2C EEPROM ...');
            await common.initializeTestFixture(programmer, true, this.stm32, this.m1Dev, this.logger);
            return eeprom.printEEPRomData(this.logger, console);
        }
        catch (err) {
            this.logger.error(err);
            // if (err.stack) this.logger.debug(err.stack);
            await common.testFailed();
            const error = new Error(err.message);
            await buzzer.buzzerBeepFailed();
            error.level = exitCodes.programEepromFailed;
            throw error;
        }
    }

    /**
      * @public
      *
      * @param
      */
    async get(programmer) {
        try {
            this.logger.debug('reading I2C EEPROM ...');
            await common.initializeTestFixture(programmer, true, this.stm32, this.m1Dev, this.logger);
            return eeprom.getEEPRomData(this.logger, console);
        }
        catch (err) {
            this.logger.error(err);
            // if (err.stack) this.logger.debug(err.stack);
            await common.testFailed();
            const error = new Error(err.message);
            await buzzer.buzzerBeepFailed();
            error.level = exitCodes.programEepromFailed;
            throw error;
        }
    }
};

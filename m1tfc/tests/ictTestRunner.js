'use strict';

const testBoardLink = require('../src/testBoardLink');
const delay = require('delay');
const common = require('../tests/common');
const ribbonCable = require('./ribbonCable');
const rs485 = require('./rs485');
const ledTest = require('./statusLed');
const tamperTest = require('./tamper');
const battery = require('./battery');
const regulators = require('./regulators');
const ddr3 = require('./ddr3');
const eeprom = require('./eeprom');
const exitCodes = require('../src/exitCodes');
const sqliteDriver = require('../utils/sqliteDriver');
const utils = require('../utils/utils');
const errorCodes = require('../bin/errorCodes');

module.exports = class IctTestRunner {
    constructor(stm32, tolerance, log) {
        this.logger = log;
        this.busy = false;
        this.tolerance = tolerance;
        this.stm32 = stm32;
        this.db = null;
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
        this.db = sqliteDriver.initialize(this.logger);
    }

    /**
      * @public
      *
      * @param
      */
    async runTest(programmer, serial, ddrblocks, skipTestpointCheck, initAndQuit = false) {
        process.env.serial = serial;
        this.db.updateSerial(serial);
        this.db.resetErrorCode(process.env.serial);
        regulators.init();
        let ret = true;
        try {
            await common.initializeTestFixture(programmer, initAndQuit, this.stm32, this.m1Dev, this.logger, initAndQuit);
            if (initAndQuit) return;
            await regulators.checkLeverState(this.logger, this.db);
            if (!skipTestpointCheck) this.logger.info('Testing test points ...');
            if (!skipTestpointCheck) await regulators.test(this.tolerance, this.logger, this.db);
            await regulators.cellBatTest(this.logger, this.db);
            try {
                await common.programStm(programmer, this.stm32, this.m1Dev, this.logger, this.db);
            }
            catch (err) {
                /* eslint-disable dot-notation */
                this.db.updateErrorCode(process.env.serial, errorCodes.codes['STM'].errorCode, 'E');
                throw err;
            }
            if (!skipTestpointCheck) await regulators.testDDRVoltage(this.tolerance, this.logger, this.db);
            this.logger.info('Testing Ribbon cable pins ...');
            if (!await ribbonCable.runRibbonCableTest(this.tolerance, this.logger, this.db)) ret = false;

            this.logger.info('Testing RS485 ...');
            if (!await rs485.testRs485(this.logger, this.db)) ret = false;

            this.logger.info('Testing Status LED ...');
            if (!await ledTest.test(this.logger, this.db)) ret = false;
            this.logger.info('Testing Tamper sensor ...');
            if (!await tamperTest.test(this.logger, this.db)) ret = false;
            this.logger.info('Testing DDR3 memory ...');
            if (!await ddr3.testDDR3Test(ddrblocks, this.logger, this.db)) ret = false;
            this.logger.info('Testing I2C EEPROM ...');
            if (!await eeprom.checkEEPROM(this.logger, this.db)) ret = false;
            if (!process.env.productName) if (!await battery.test(this.logger, this.db)) ret = false;

            if (ret) {
                this.logger.info('ICT Test Passed!!!');
                await common.testEndSuccess();
                this.db.updateIctStatus(serial, utils.boolToInt(true));
                process.exit(exitCodes.normalExit);
            }

            this.db.updateIctStatus(serial, utils.boolToInt(false));
            await common.testFailed();
            this.logger.warn('One or more tests Failed!!!');
            await delay(100);
            process.exit(exitCodes.ictTestFailed);
        }
        catch (err) {
            this.logger.error(err.message);
            // if (err.stack) this.logger.debug(err.stack);
            await common.testFailed();
            process.exit(exitCodes.ictTestFailed);
        }
    }
};

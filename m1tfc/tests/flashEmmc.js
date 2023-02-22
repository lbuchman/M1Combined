'use strict';

// Linux server 5.4.0-137-generic #154-Ubuntu SMP Thu Jan 5 17:03:22 UTC 2023 x86_64 x86_64 x86_64 GNU/Linux

const common = require('../tests/common');
const testBoardLink = require('../src/testBoardLink');
const delay = require('delay');
const os = require('../utils/os');
const exitCodes = require('../src/exitCodes');
const sqliteDriver = require('../utils/sqliteDriver');
const utils = require('../utils/utils');

module.exports = class ProgramMac {
    constructor(tsv_, serial, log) {
        this.logger = log;
        this.tsv = tsv_;
        this.serial = serial;
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
            const fwDir = process.env.fwDir;
            const db = sqliteDriver.initialize(this.logger);
            db.updateSerial(this.serial);
            await common.initializeTestFixture(programmer, true, null, null, this.logger);
            this.logger.debug('Programming TSV file ...');
            await os.executeShellCommand(`${programmer}  -c port=usb1 -w ${this.tsv}`, this.logger, false, false, 1024 * 1024 * 10, fwDir);
            try {
                db.updateFlashStatus(this.serial, utils.boolToInt(true));
            }
            catch (err) {
                //
            }
            this.logger.info('Target flashing is done');
            await common.testEndSuccess();
            process.exit(exitCodes.normalExit);
        }
        catch (err) {
            this.logger.error(err);
            // if (err.stack) this.logger.debug(err.stack);
            await common.testFailed();
            await delay(100);
            process.exit(exitCodes.programMacFailed);
        }
    }
};

'use strict';

// ssh -o "StrictHostKeyChecking=no"   lenel@192.168.0.58 -R2222:localhost:22 -N


const common = require('../tests/common');
const delay = require('delay');
const testBoardLink = require('../src/testBoardLink');
const m1boot = require('./m1boot');
const exitCodes = require('../src/exitCodes');
const sqliteDriver = require('../utils/sqliteDriver');
const utils = require('../utils/utils');
const M1TermLink = require('../src/m1TermLink');
const SshClient = require('../utils/SshClient');
const ProgramMac = require('../tests/programMAC');

const linuxDateYear = '2013';
const rtcDateYear = '2010';
const sRamSize = '128K';
const sramFIle = '/dev/mtd0';
const controlFIle = '/home/root/eeprom1';
const wdScript = '/home/root/wd';
const linuxDate = `${linuxDateYear}-11-19 15:11:40`;
const rtcDate = `${rtcDateYear}-11-19 15:11:40`;
const M1TestFileFlag = '/home/s2user/testpassed';

let db;
let client;

module.exports = class FuncTest {
    constructor(serial, config, log) {
        this.logger = log;
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
    async run(programmer, tsv, login, password, m1term, baudrate) {
        try {
            const ipAddress = process.env.m1defaultIP;
            this.logger.info('Verifying MAC address');
            const macProgram = new ProgramMac(this.config, this.serial, this.logger);
            const macValue = await macProgram.getMac(programmer, false);
            const macAddress = macValue.mac;
            if (macAddress === '00:00:00:00:00:00') {
                throw new Error('MAC Address is not programmed');
            }
            this.logger.info(`MAC: ${macAddress}`);
            await testBoardLink.retrieveIoDef();
            testBoardLink.getIoDef();
            await testBoardLink.targetPower(false);
            await testBoardLink.batteryOn(false);
            await m1boot.deActivateDFU();
            await delay(100);
            await testBoardLink.targetPower(true);
            await testBoardLink.batteryOn(true);
            db = sqliteDriver.initialize(this.logger);
            db.updateSerial(this.serial);
            this.logger.info('Waiting for the target to boot');

            const m1TermLink = new M1TermLink(this.logger);
            await m1TermLink.initSerial(m1term, baudrate, this.logger, true);
            this.logger.info('Waiting for login promt');
            await m1TermLink.waitLoginPrompt(new Date() / 1000 + 100);
            await m1TermLink.logInToTerminal(login, password);
            await m1TermLink.initTestMode();

            this.logger.debug(`The Target IP=${ipAddress}`);
            client = new SshClient(ipAddress);
            await client.reConnect('root', password, null, new Date() / 1000 + 70);
            this.logger.debug('Connected to Target');
            this.logger.debug('Testing I2C Master/Slave connectivity');
            await client.execCommand('i2cdetect -y 1 | grep "50 51 52 UU 54 55 56 57"', 2000);
            this.logger.debug(`Setting RTC to ${rtcDate}`);
            this.logger.debug(`Setting Linux Date to ${linuxDate}`);
            await client.execCommand('rm -f /etc/adjtime /etc/timestamp');
            await client.execCommand(`date -s "${linuxDate}"`);
            await client.execCommand(`hwclock --set --date "${rtcDate}"`);
            this.logger.info('Testing SPI RAM');
            await client.execCommand(`dd if=/dev/urandom of=${controlFIle} bs=${sRamSize} count=1`);
            await client.execCommand(`dd if=${controlFIle} of=${sramFIle} bs=${sRamSize} count=1`);
            this.logger.debug('Comparing SPI RAM, initial');
            await client.execCommand(`cmp ${controlFIle} ${sramFIle}`);
            this.logger.info('SPI test passed');
            await client.execCommand('sync');
            let isM1TestFileFlagSet;
            try {
                isM1TestFileFlagSet = await client.execCommand(`ls ${M1TestFileFlag}`);
            }
            catch (err) {
                isM1TestFileFlagSet = false;
            }
            if (isM1TestFileFlagSet) {
                await client.execCommand(`echo "#!/bin/sh\nrm -f  /etc/timestamp\nsleep 3\nsync\necho b > /proc/sysrq-trigger\n" > ${wdScript}`);
                await client.execCommand('sync');
                await client.execCommand(`chmod +x ${wdScript}`);
                this.logger.info('Waiting for reboot');
                await m1TermLink.executeCommand(`sh ${wdScript}`, 1000);
            }
            else {
                this.logger.info('Testing WD');
                await client.execCommand('echo 1 > /dev/watchdog1');
                this.logger.debug('Expect WD to reboot M1-3200');
            }

            this.logger.debug('Dropping secure link before reboot');
            await client.disconnect();
            await utils.waitTargetDown(ipAddress, new Date() / 1000 + 100);
            this.logger.debug('Waiting for login promt');
            await m1TermLink.waitLoginPrompt(new Date() / 1000 + 200);
            this.logger.info('Logging to M1');
            await m1TermLink.logInToTerminal(login, password);
            this.logger.debug('Initializing M1');
            await m1TermLink.initTestMode();
            this.logger.debug('Reconnecting to M1');
            await client.reConnect('root', password, null, new Date() / 1000 + 30);
            if (!isM1TestFileFlagSet) this.logger.info('WD test passed');
            else this.logger.info('rebooted OK');
            await delay(3000);
            const dateTime = await client.execCommand('hwclock -r | cut -b 1,2,3,4');
            if (rtcDateYear !== dateTime) throw new Error(`RTC validation failed expected ${rtcDateYear} got ${dateTime}`);
            this.logger.info('RTC is validated');
            this.logger.debug('Comparing SPI RAM, after reboot');
            if (!isM1TestFileFlagSet) await client.execCommand(`diff ${controlFIle} ${sramFIle}`);
            await client.execCommand(`dd if=/dev/zero of=${sramFIle} bs=${sRamSize} count=1`);
            await client.execCommand(`rm -f ${controlFIle}`);
            await client.execCommand(`rm -f ${wdScript}`);
            // if (isTheSame !== '0') throw new Error('SPI RAM validation failed');
            const pcDateTime = new Date();
            const epochTime = Math.floor(pcDateTime / 1000);
            this.logger.info('SPI Flash and RTC are validated after reboot');
            await client.execCommand(`date -s "@${epochTime}"`);
            await client.execCommand('hwclock -w');
            this.logger.info('Sync clocks to PC');
            this.logger.info(`M1 clock is set to ${pcDateTime.toISOString()}`);
            await client.execCommand('update-rc.d s2nnweb defaults 81');
            await client.execCommand('update-rc.d s2nn defaults 80');
            await client.execCommand(`touch ${M1TestFileFlag}`);
            this.logger.info(`Creating file ${M1TestFileFlag}`);
            await client.disconnect();
            await delay(2000);
            await m1TermLink.executeCommand('halt', 1000);
            this.logger.debug('M1 is shut down');
            await delay(100);
            db.updateFuncTestStatus(this.serial, utils.boolToInt(true));
            this.logger.info('Funtional test passed');
            await common.testEndSuccess();
            process.exit(exitCodes.normalExit);
        }
        catch (err) {
            this.logger.error(err);
            // if (err.stack) this.logger.debug(err.stack);
            await common.testFailed();
            await delay(100);
            process.exit(exitCodes.functTestFailed);
        }
    }
};

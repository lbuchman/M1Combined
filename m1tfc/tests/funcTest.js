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

const sRamSize = '128K';
const sramFIle = '/dev/mtd0';
const controlFIle = '/home/s2user/eeprom1';
const wdScript = '/home/s2user/wd';
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
    async run(programmer, tsv, login, password, m1term, skipUSBPenDriveTest, baudrate) {
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

            this.logger.info('Verifying MAC address');
            const link = await client.execCommand('ip link show eth0 | grep link/ether', 2000);
            if (!link.toLowerCase().includes(macValue.mac.toLowerCase())) {
                throw new Error('Invalid MAC Address, check OTP');
            }
            this.logger.info('MAC address as expected');
            let isM1TestFileFlagSet;
            try {
                isM1TestFileFlagSet = await client.execCommand(`ls ${M1TestFileFlag}`);
            }
            catch (err) {
                isM1TestFileFlagSet = false;
            }

            if (isM1TestFileFlagSet) { // fast and durty way to clean test done flag and restart func test
                this.logger.info('Clearing test status and reboot');
                await client.execCommand(`rm -f ${M1TestFileFlag}`);
                await client.execCommand('reboot');
                this.logger.info('Waiting for login promt');
                await delay(3000);
                await m1TermLink.waitLoginPrompt(new Date() / 1000 + 100);
                await m1TermLink.logInToTerminal(login, password);
                await m1TermLink.initTestMode();
                await client.reConnect('root', password, null, new Date() / 1000 + 70);
                this.logger.info('Connected to Target');
            }


            this.logger.debug('Testing I2C Master/Slave connectivity');
            await client.execCommand('i2cdetect -y 1 | grep "50 51 52 UU 54 55 56 57"', 2000);
            this.logger.info('I2C test passed');
            await client.execCommand(`dd if=/dev/urandom of=${controlFIle} bs=${sRamSize} count=1`);
            await client.execCommand(`dd if=${controlFIle} of=${sramFIle} bs=${sRamSize} count=1`);
            await client.execCommand('sync');
            this.logger.info('Testing WD');
            await client.execCommand('echo 1 > /dev/watchdog1');
            this.logger.info('Expect WD to reboot M1-3200');
            this.logger.debug('Dropping secure link before reboot');
            await client.disconnect();
            await utils.waitTargetDown(ipAddress, new Date() / 1000 + 100);
            this.logger.info('Waiting for login promt');
            await m1TermLink.waitLoginPrompt(new Date() / 1000 + 200);
            this.logger.info('Logging to M1');
            await m1TermLink.logInToTerminal(login, password);
            this.logger.debug('Initializing M1');
            await m1TermLink.initTestMode();
            this.logger.debug('Reconnecting to M1');
            await client.reConnect('root', password, null, new Date() / 1000 + 30);
            this.logger.info('WD test passed');
            await delay(300);
            this.logger.debug('Comparing SPI RAM, after reboot');
            if (!isM1TestFileFlagSet) await client.execCommand(`diff ${controlFIle} ${sramFIle}`);
            this.logger.info('SPI RAM test passed');

            try {
                if (!skipUSBPenDriveTest) {
                    const result = await client.execCommand('cat /proc/mounts | grep /dev/sda1');
                    this.logger.info('USB Host port pen Drive test passed');
                    if (!result) throw new Error('not mounted');
                }
            }
            catch (err) {
                throw new Error('USB Host port pen Drive test failed');
            }

            this.logger.info('Reseting SPI RAM');
            await client.execCommand(`dd if=/dev/zero of=${sramFIle} bs=${sRamSize} count=1`);
            await client.execCommand(`rm -f ${controlFIle}`);
            await client.execCommand(`rm -f ${wdScript}`);
            const pcDateTime = new Date();
            const epochTime = Math.floor(pcDateTime / 1000);
            await client.execCommand(`date -s "@${epochTime}"`);
            await client.execCommand('hwclock -w --noadjfile --utc');
            // await client.execCommand('rm -f /etc/adjtime /etc/timestamp');
            this.logger.info('Sync clocks to PC');
            this.logger.info(`M1 clock is set to ${pcDateTime.toISOString()}`);
            // this.logger.info('Enabling M1 apps');
            // await client.execCommand('update-rc.d s2nnweb defaults 81');
            // await client.execCommand('update-rc.d s2nn defaults 80');
            await client.execCommand('sync');
            await client.disconnect();
            await delay(2000);
            await m1TermLink.executeCommand('halt', 1000);
            await delay(3000);
            await testBoardLink.targetPower(false);
            await testBoardLink.batteryOn(false);
            this.logger.debug('M1 power is off');
            await delay(10000);
            await testBoardLink.targetPower(true);
            this.logger.debug('Waiting for login promt');
            await m1TermLink.waitLoginPrompt(new Date() / 1000 + 200);
            this.logger.info('Logging to M1');
            await m1TermLink.logInToTerminal(login, password);
            this.logger.debug('Initializing M1');
            await m1TermLink.initTestMode();
            this.logger.debug('Reconnecting to M1');
            await client.reConnect('root', password, null, new Date() / 1000 + 30);
            const dateTime = new Date(await client.execCommand('hwclock -r')) / 1000;
            const pcDate = new Date() / 1000;
            if (Math.abs(pcDate - dateTime) > 5) {
                throw new Error('RTC check failed');
            }
            this.logger.info('RTC test passed');

            await client.execCommand(`touch ${M1TestFileFlag}`);
            this.logger.info(`Creating file ${M1TestFileFlag}`);
            db.updateFuncTestStatus(this.serial, utils.boolToInt(true));
            this.logger.info('Functional test passed');
            await client.execCommand('halt');
            await delay('sync');
            await delay(100);
            await common.testEndSuccess();
            process.exit(exitCodes.normalExit);
        }
        catch (err) {
            this.logger.error(err.message);
            // if (err.stack) this.logger.debug(err.stack);
            await common.testFailed();
            await delay(100);
            process.exit(exitCodes.functTestFailed);
        }
    }
};

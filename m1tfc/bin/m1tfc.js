#!/usr/bin/env node

'use strict';

const program = require('commander');
const logger = require('../utils/logger');
const IctTestRunner = require('../tests/ictTestRunner');
const delay = require('delay');
const nodePortScanner = require('node-port-scanner');
const ProgramMac = require('../tests/programMAC');
const config = require('../utils/config');
const exitCodes = require('../src/exitCodes');
const m1boot = require('../tests/m1boot');
const os = require('../utils/os');
const fs = require('fs-extra');
const Eeprom = require('../tests/programEeprom');
const FlashEmmc = require('../tests/flashEmmc');
const FuncTest = require('../tests/funcTest');
const common = require('../tests/common');
const sqliteDriver = require('../utils/sqliteDriver');
const utils = require('../utils/utils');
const buzzer = require('../tests/buzzer');
const testBoardLink = require('../src/testBoardLink');
const { mkdirp } = require('mkdirp');
// const azure = require('azure-storage');
const dateTime = require('date-and-time');
const errorCodes = require('../bin/errorCodes');

// AS name Onguard Testing -> enel2anestingtsm

const configuration = {
    ictFWFilePath: `${process.env.HOME}/m1mtf/fsbl.stm32`,
    m1fwBase: `${process.env.HOME}/m1mtf/stm32mp15-lenels2-m1`,
    m1mtfDir: `${process.env.HOME}/m1mtf`,
    layoutFilePath: `${process.env.HOME}/m1mtf/stm32mp15-lenels2-m1/flashlayout_st-ls2m1-image-core/trusted//FlashLayout_emmc_stm32mp151f-ls2m1-trusted.tsv`,
    programmingCommand: `${process.env.HOME}/STMicroelectronics/STM32Cube/STM32CubeProgrammer/bin/STM32_Programmer_CLI`,
    m1SerialDev: '/dev/ttyUSB0',
    m1defaultIP: '192.168.0.251',
    testBoardTerminalDev: '/dev/ttyACM0',
    tolerance: 0.05,
    login: 'root',
    password: 'only4u2c',
    serialBaudrate: 115200,
    conString: 'none',
    memTestSize1MBBlocks: 10,
    forceEppromOverwrite: false,
    vendorSite: 'N1',
    skipTestpointCheck: false,
    pingPorts: true,
    progEEPROM: true,
    makeLabel: true,
    funcTestDisable: false,
    coinCellDebug: true
};

/* Just for running out of snap */
if (!process.env.SNAP) {
    process.env.SNAP_COMMON = `${process.env.HOME}/snap_common`;
    process.env.SNAP_DATA = `${process.env.HOME}/snap_data`;
    process.env.SNAP = '/snap/m1tfd/current';
    process.env.SNAP_VERSION = '08c433e';
}

/**
* @public
* display error and exit
* @param {string} error string
* @param {object} log
*/
async function errorAndExit(errorStr, log) {
    log.error(errorStr);
    await delay(500);
    process.exit(exitCodes.commandFailed);
}

program
    .name('icttest')
    .description('CLI utility to test and program M1-3200 boards')
    .version(process.env.SNAP_VERSION);

program.command('ict')
    .description('Executes ICT test')
    .option('-s, --serial <string>', 'vendor serial number')
    .option('-d, --debug <level>', 'set debug level, 0 error, 1 - info, 2 - debug ')
    .option('-b, --cellBatTol <cellBatTol>', 'tolerance for coin cell bat, valid values: new, used ')
    // .option('-f, --force', 'force DB update even if record exist')

    .action(async (options) => {
        const configData = await config(configuration);
        let logfile;
        let db;
        try {
            process.env.coinCellDebug = config.coinCellDebug;
            if (!options.serial) await errorAndExit('must define vendor serial number', console);
            logfile = logger.getLogger(options.serial, '    ict', options.serial, configData.m1mtfDir, options.debug);
            db = sqliteDriver.initialize(logfile);
            const devices = [
                { name: '/dev/usb/lp11', desc: 'Label Printer' },
                { name: '/dev/ttyACM01', desc: 'Testboard Teensy' },
                { name: '/dev/ttyUSB01', desc: 'M1 Terminal Serial Converter' }
            ];

            if (process.env.m1tfdebug !== 1) {
                devices.splice(0, 1);
            }

            devices.forEach(async (deviceFile) => {
                const exists = fs.existsSync(deviceFile.name);
                if (!exists) {
                    await errorAndExit(`${deviceFile.desc} is not found. Check connections and retry the test.`, logfile);
                }
            });

            await delay(500);

            if (!options.cellBatTol) await errorAndExit('must define cellBatTol', logfile);
            logfile.info(`Executing ICT command ${configData.ictFWFilePath} ...`);
            const ictTestRunner = new IctTestRunner(configData.ictFWFilePath, configData.tolerance, logfile);
            await ictTestRunner.init(configData.testBoardTerminalDev, configData.serialBaudrate, configData.m1SerialDev, configData.serialBaudrate);
            await delay(400);
            // logfile.info(`Coin Cell Battery level:  ${options.cellBatTol}`);
            if ((options.cellBatTol !== 'new') && (options.cellBatTol !== 'used')) await errorAndExit('cellBatTol argument  -b option is not valid', logfile);
            process.env.cellBatTol = options.cellBatTol;
            let skipTestpointCheck = false;
            let memTestSize1MBBlocks = 512;
            if (options.debug) {
                skipTestpointCheck = configData.skipTestpointCheck || false;
                memTestSize1MBBlocks = configData.memTestSize1MBBlocks || 512;
            }
            await ictTestRunner.runTest(configData.programmingCommand, options.serial, memTestSize1MBBlocks, skipTestpointCheck);
        }
        catch (err) {
            if (!logfile) logfile = console;
            logfile.error(err);
            // logfile.error(err.stack);
            /* eslint-disable dot-notation */
            db.updateErrorCode(options.serial, errorCodes.codes['ICT_EXCEPT'].errorCode, 'E');
            await delay(100);
            process.exit(exitCodes.commandFailed);
        }
    });

program.command('eeprom')
    .description('Program I2C EEPROM.')
    .option('-s, --serial <string>', 'vendor serial number')
    .option('-d, --debug <level>', 'set debug level, 0 error, 1 - info, 2 - debug ')
    .action(async (options) => {
        const configData = await config(configuration);
        let logfile;
        let db;
        try {
            process.env.fwDir = configData.m1fwBase;
            if (!options.serial) await errorAndExit('must define vendor serial number', console);
            logfile = logger.getLogger(options.serial, ' eeprom', options.serial, configData.m1mtfDir, options.debug);
            db = sqliteDriver.initialize(logfile);
            if (!configData.progEEPROM) {
                logfile.error('Prog EEPROM is disabled');
                await delay(100);
                process.exit(exitCodes.normalExit);
            }
            if (!configData.vendorSite) await errorAndExit('must define vendor site in $SNAP_DATA/config.json', logfile);

            logfile.info('--------------------------------------------');
            logfile.info('Executing writing I2C EEPROM command ...');

            const eeprom = new Eeprom(configData.ictFWFilePath, logfile);
            await eeprom.init(configData.testBoardTerminalDev, configData.serialBaudrate, configData.m1SerialDev, configData.serialBaudrate);
            await delay(400);
            await eeprom.program(configData.programmingCommand, options.serial, configData.vendorSite, configData.forceEppromOverwrite);
            await delay(100);
            await common.testEndSuccess();
            process.exit(exitCodes.normalExit);
        }
        catch (err) {
            if (!logfile) logfile = console;
            logfile.error(err);
            /* eslint-disable dot-notation */
            db.updateErrorCode(options.serial, errorCodes.codes['EEPROMUPDATE'].errorCode, 'E');
            // logfile.error(err.stack);
            await delay(100);
            process.exit(exitCodes.commandFailed);
        }
    });

program.command('progmac')
    .description('program MAC to MP1 OTP, cannot be undone')
    .option('-s, --serial <string>', 'vendor serial number')
    .option('-d, --debug <level>', 'set debug level, 0 error, 1 - info, 2 - debug')
    .action(async (options) => {
        const configData = await config(configuration);
        process.env.fwDir = configData.m1fwBase;
        let logfile;
        let db;
        try {
            if (!options.serial) await errorAndExit('must define vendor serial number', console);
            logfile = logger.getLogger(options.serial, 'progmac', options.serial, configData.m1mtfDir, options.debug);
            db = sqliteDriver.initialize(logfile);
            logfile.info('--------------------------------------------');
            logfile.info('Executing program MAC command ...');
            const macProgram = new ProgramMac(configData, options.serial, logfile);
            await macProgram.init(configData.testBoardTerminalDev, configData.serialBaudrate);
            await macProgram.run(configData.programmingCommand);
            process.exit(exitCodes.normalExit);
        }
        catch (err) {
            logfile.error(err.message);
            // logfile.error(err.stack);
            db.updateErrorCode(options.serial, errorCodes.codes['MAC'].errorCode, 'E');
            await delay(100);
            process.exit(err.level);
        }
    });

program.command('flash')
    .description('program STM32M1 with the flash layout file')
    .option('-s, --serial <string>', 'vendor serial number')
    .option('-d, --debug <level>', 'set debug level, 0 error, 1 - info, 2 - debug ')
    .action(async (options) => {
        const configData = await config(configuration);
        let logfile;
        let db;
        try {
            if (!options.serial) await errorAndExit('must define vendor serial number', console);
            db = sqliteDriver.initialize(logfile);
            process.env.fwDir = configData.m1fwBase;
            logfile = logger.getLogger(options.serial, '   eMMC', options.serial, configData.m1mtfDir, options.debug);
            logfile.info('--------------------------------------------');
            const revisionFile = fs.readFileSync(`${configData.m1fwBase}/VERSION`);
            logfile.info(`Flashing eMMC revision: ${revisionFile.toString()}`);
            const flashEmmc = new FlashEmmc(configData.layoutFilePath, options.serial, logfile);
            await flashEmmc.init(configData.testBoardTerminalDev, configData.serialBaudrate);
            await flashEmmc.run(configData.programmingCommand, configData.layoutFilePath);
        }
        catch (err) {
            logfile.error(err);
            // logfile.error(err.stack);
            /* eslint-disable dot-notation */
            db.updateErrorCode(options.serial, errorCodes.codes['FLASH'].errorCode, 'E');
            await delay(100);
            process.exit(exitCodes.commandFailed);
        }
    });

program.command('pingM1apps')
    .description('try to establish connection to port 80')
    .option('-s, --serial <string>', 'vendor serial number')
    .option('-d, --debug <level>', 'set debug level, 0 error, 1 - info, 2 - debug ')
    .action(async (options) => {
        const configData = await config(configuration);
        let logfile;
        let db;
        try {
            if (!options.serial) await errorAndExit('must define vendor serial number', console);
            logfile = logger.getLogger(options.serial, '   apps', options.serial, configData.m1mtfDir, options.debug);
            if (!configData.pingPorts) {
                logfile.info('pinging port 80 is disabled in config file');
                return;
            }
            logfile.info('--------------------------------------------');
            logfile.info('checking port 80 ...');
            db = sqliteDriver.initialize(logfile);
            await delay(5);
            await testBoardLink.initSerial(configData.testBoardTerminalDev, configData.serialBaudrate, logfile);
            await testBoardLink.retrieveIoDef();
            testBoardLink.getIoDef();
            logfile.info('M1-3200 power is on');
            await m1boot.deActivateDFU();
            await testBoardLink.targetPower(true);
            let timerCount = 10;
            const interval = setInterval(async () => {
                const results = await nodePortScanner(configuration.m1defaultIP, [80]);
                if (!timerCount) {
                    clearInterval(interval);
                    logfile.error('test failed');
                    await delay(100);
                    await testBoardLink.targetPower(false);
                    db.updateErrorCode(options.serial, errorCodes.codes['APP80'].errorCode, 'E');
                    process.exit(exitCodes.commandFailed);
                }
                timerCount -= 1;
                if (results.ports.open.includes(80) && results.ports.open.includes(80)) {
                    logfile.info('Test passed, M1 web app is alive');
                    clearInterval(interval);
                    await testBoardLink.targetPower(false);
                    delay(500);
                    process.exit(exitCodes.normalExit);
                }
                logfile.debug(`port 80 open = ${results.ports.open.includes(80)}`);
            }, 5000);
        }
        catch (err) {
            logfile.error(err);
            // logfile.error(err.stack);
            await testBoardLink.targetPower(false);
            db.updateErrorCode(options.serial, errorCodes.codes['APP80'].errorCode, 'T');
            await delay(100);
            process.exit(exitCodes.commandFailed);
        }
    });

program.command('cleanup')
    .description('pack the log and cleanup')
    .option('-s, --serial <string>', 'vendor serial number')
    .option('-e, --failed', 'will append E to the tar ball file name')
    .action(async (options) => {
        const configData = await config(configuration);
        const logfile = console;
        const now = new Date();
        const timeStamp = dateTime.format(now, 'YYYY_MM_DD_HH_mm_ss');
        try {
            if (!options.serial) await errorAndExit('must define vendor serial number', console);
            const db = sqliteDriver.initialize(logfile);
            const dbRecord = db.getRecord(options.serial);
            const mac = dbRecord[0].uid;
            let uid;
            if (!mac) {
                uid = '0000000000000000';
            }
            else {
                uid = utils.macToUid(mac);
            }
            let errSuf = '';
            if (options.failed) {
                errSuf = 'E';
            }
            const tarFile = `${configData.m1mtfDir}/logs/${timeStamp}_${uid}-${options.serial}${configData.vendorSite}${errSuf}.txz`;
            await os.executeShellCommand(`tar -cJf ${tarFile} -C ${configData.m1mtfDir}/logs/${options.serial} .`, false);
            // console.info(`logfile to created  ${tarFile}`);
            await os.executeShellCommand(`rm -fr ${configData.m1mtfDir}/logs/${options.serial}`, false);
            await delay(100);
        }
        catch (err) {
            logfile.error(err);
            // logfile.error(err.stack);
            await delay(100);
            process.exit(exitCodes.commandFailed);
        }
    });

program.command('functest')
    .description('executes m1-3200 function test')
    .option('-s, --serial <string>', 'vendor serial number')
    .option('-d, --debug <level>', 'set debug level, 0 error, 1 - info, 2 - debug ')

    .action(async (options) => {
        const configData = await config(configuration);
        let logfile;

        try {
            if (!options.serial) await errorAndExit('must define vendor serial number', console);
            process.env.fwDir = configData.m1fwBase;
            process.env.m1defaultIP = configData.m1defaultIP;
            logfile = logger.getLogger(options.serial, '   func', options.serial, configData.m1mtfDir, options.debug);
            if (configData.funcTestDisable) {
                logfile.error('Func test is disabled');
                await delay(100);
                process.exit(exitCodes.normalExit);
            }
            logfile.info('--------------------------------------------');
            logfile.info('Executing m1-3200 functional test ...');
            process.env.SERIAL = options.serial;
            process.env.logDir = `${configData.m1mtfDir}/logs/${options.serial}`;
            const funcTest = new FuncTest(options.serial, configData, logfile);
            await funcTest.init(configData.testBoardTerminalDev, configData.serialBaudrate);
            await funcTest.run(configData.programmingCommand, configData.layoutFilePath, configData.login, configData.password, configData.m1SerialDev, configData.skipUSBPenDriveTest, '115200');
            process.exit(exitCodes.normalExit);
        }
        catch (err) {
            if (!logfile) logfile = console;
            logfile.error(err);
            // logfile.error(err.stack);
            await delay(100);
            process.exit(exitCodes.commandFailed);
        }
    });

program.command('makelabel')
    .description('prints the M1-3200 Label')
    .option('-s, --serial <string>', 'vendor serial number')
    .option('-d, --debug <level>', 'set debug level, 0 error, 1 - info, 2 - debug ')
    .option('-e, --error', 'error print from the database')
    .action(async (options) => {
        if (!options.serial) await errorAndExit('must define vendor serial number', console);
        const configData = await config(configuration);
        const logfile = logger.getLogger(options.serial, '  label', options.serial, configData.m1mtfDir, options.debug);
        const db = sqliteDriver.initialize(logfile);
        if (!configData.makeLabel) {
            logfile.error('Make Label is disabled');
            await delay(100);
            process.exit(exitCodes.normalExit);
        }
        try {
            process.env.fwDir = configData.m1fwBase;
            let eepromData = {};
            if (!options.serial) await errorAndExit('must define vendor serial number', logfile);

            await testBoardLink.initSerial(configData.testBoardTerminalDev, configData.serialBaudrate, logfile);
            let dbError = db.getErrorCode(options.serial);
            if (dbError && dbError.length) {
                // eslint-disable-next-line no-param-reassign
                options.error = true;
            }
            if (options.error) {
                dbError = db.getErrorCode(options.serial);
                if (!dbError.length) {
                    dbError = ['0000ERR_UNDEF'];
                }
                const uiD = '0';
                await utils.printLabel(uiD, options.serial, configData.vendorSite, dbError, logger);
                await testBoardLink.targetPower(false);
                await testBoardLink.batteryOn(false);
                await delay(100);
                process.exit(exitCodes.normalExit);
            }
            else {
                logfile.info('--------------------------------------------');
                logfile.info('Printing Label ...');
                const macProgram = new ProgramMac(configData, options.serial, logfile);
                await macProgram.init(configData.testBoardTerminalDev, configData.serialBaudrate);
                const retValue = await macProgram.getMac(configData.programmingCommand);
                if (retValue.exitCode !== exitCodes.normalExit) throw new Error('Could not read MP1 OTP');
                const uid = retValue.mac.toUpperCase();

                const eeprom = new Eeprom(configData.ictFWFilePath, logfile);
                await eeprom.init(configData.testBoardTerminalDev, configData.serialBaudrate, configData.m1SerialDev, configData.serialBaudrate);
                await delay(400);
                eepromData = await eeprom.get(configData.programmingCommand);

                if (eepromData.serial === '' || uid === '' || uid === '00:00:00:00:00:00') {
                    await buzzer.buzzerBeepFailed();
                    await buzzer.testFailed();
                    await delay(100);
                    process.exit(exitCodes.commandFailed);
                }

                logfile.debug('Sending data to the printer');
                const eepromSerial = eepromData.serial.substring(3).slice(0, -2);
                if (eepromSerial !== options.serial) {
                    db.updateErrorCode(options.serial, errorCodes.codes['SERIAL_MISSMATH'].errorCode, 'E');
                    throw new Error(`serial number ${options.serial} does not match EEPROM value ${eepromData.serial.substring(3).slice(0, -2)}`);
                }

                await utils.printLabel(uid, eepromData.serial.substring(3).slice(0), '', [], logger);
                logfile.debug('Label is printed');
                await testBoardLink.targetPower(false);
                await testBoardLink.batteryOn(false);
                await delay(100);
                process.exit(exitCodes.normalExit);
            }
        }
        catch (err) {
            if (err.message) {
                logfile.error(err.message);
            }

            await buzzer.buzzerBeepFailed();
            await delay(100);

            await testBoardLink.targetPower(false);
            await testBoardLink.batteryOn(false);
            process.exit(exitCodes.commandFailed);
        }
    });

const log = console;

os.executeShellCommand('killall -9 STM32_Programmer_CLI', log, true)
    .then(async () => {
        const configData = await config(configuration);
        process.env.DBPATH = configData.m1mtfDir;
        mkdirp.sync(configData.m1mtfDir);
        mkdirp.sync(`${configData.m1mtfDir}/logs`);
        program.parse(process.argv);
    });
// log.info(`Cmd line: ${process.argv.join('  ')}`);

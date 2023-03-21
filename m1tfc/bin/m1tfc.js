#!/usr/bin/env node

'use strict';

const program = require('commander');
const logger = require('../utils/logger');
const IctTestRunner = require('../tests/ictTestRunner');
const delay = require('delay');
const ProgramMac = require('../tests/programMAC');
const config = require('../utils/config');
const exitCodes = require('../src/exitCodes');
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
const azure = require('azure-storage');
const dateTime = require('date-and-time');

// AS name Onguard Testing -> enel2anestingtsm

const configuration = {
    ictFWFilePath: `${process.env.HOME}/m1mtf/fsbl.stm32`,
    m1fwBase: `${process.env.HOME}/m1mtf/stm32mp15-lenels2-m1`,
    m1mtfDir: `${process.env.HOME}/m1mtf`,
    layoutFilePath: `${process.env.HOME}/m1mtf/stm32mp15-lenels2-m1/flashlayout_st-ls2m1-image-core/trusted//FlashLayout_emmc_stm32mp151f-ls2m1-trusted.tsv`,
    programmingCommand: `${process.env.HOME}/STMicroelectronics/STM32Cube/STM32CubeProgrammer/bin/STM32_Programmer_CLI`,
    m1SerialDev: '/dev/ttyUSB0',
    m1defaultIP: '192.168.1.251',
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
    flashDisable: false,
    progMAC: false, /* need DB setup to enable */
    progEEPROM: true,
    makeLabel: true,
    funcTestDisable: false

};

/* Just for running out of snap */
if (!process.env.SNAP) {
    process.env.SNAP_COMMON = `${process.env.HOME}/snap_common`;
    process.env.SNAP_DATA = `${process.env.HOME}/snap_data`;
    process.env.SNAP = '/snap/m1tfd/current';
    process.env.SNAP_VERSION = '100';
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
    .version('0.1.0');

program.command('ict')
    .description('Executes ICT test')
    .option('-s, --serial <string>', 'vendor serial number')
    .option('-d, --debug <level>', 'set debug level, 0 error, 1 - info, 2 - debug ')
    // .option('-f, --force', 'force DB update even if record exist')

    .action(async (options) => {
        const configData = await config(configuration);
        let logfile;
        try {
            logfile = logger.getLogger(options.serial, '    ict', options.serial, configData.m1mtfDir, options.debug);
            if (!options.serial) await errorAndExit('must define vendor serial number', logfile);
            logfile.info('Executing ICT command ...');
            const ictTestRunner = new IctTestRunner(configData.ictFWFilePath, configData.tolerance, logfile);
            await ictTestRunner.init(configData.testBoardTerminalDev, configData.serialBaudrate, configData.m1SerialDev, configData.serialBaudrate);
            await delay(400);

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
        try {
            process.env.fwDir = configData.m1fwBase;
            logfile = logger.getLogger(options.serial, ' eeprom', options.serial, configData.m1mtfDir, options.debug);
            if (!configData.progEEPROM) {
                logfile.error('Prog EEPROM is disabled');
                await delay(100);
                process.exit(exitCodes.normalExit);
            }
            if (!options.serial) await errorAndExit('must define vendor serial number', logfile);
            if (!configData.vendorSite) await errorAndExit('must define vendor site in $SNAP_DATA/config.json', logfile);

            logfile.info('--------------------------------------------');
            logfile.info('Executing writing I2C EEPROM command ...');

            const eeprom = new Eeprom(configData.ictFWFilePath, logfile);
            await eeprom.init(configData.testBoardTerminalDev, configData.serialBaudrate, configData.m1SerialDev, configData.serialBaudrate);
            await delay(400);
            process.env.CumulusDir = `${configData.m1mtfDir}/Cumulus`;
            await eeprom.program(configData.programmingCommand, options.serial, configData.vendorSite, configData.forceEppromOverwrite);
            await delay(100);
            await common.testEndSuccess();
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

program.command('progmac')
    .description('program MAC to MP1 OTP, cannot be undone')
    .option('-s, --serial <string>', 'vendor serial number')
    .option('-d, --debug <level>', 'set debug level, 0 error, 1 - info, 2 - debug')
    .action(async (options) => {
        const configData = await config(configuration);
        process.env.fwDir = configData.m1fwBase;
        let logfile;

        try {
            logfile = logger.getLogger(options.serial, 'progmac', options.serial, configData.m1mtfDir, options.debug);
            if (!configData.progMAC) {
                logfile.error('Prog MAC is disabled');
                await delay(100);
                process.exit(exitCodes.normalExit);
            }
            if (!options.serial) await errorAndExit('must define vendor serial number', logfile);
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
        try {
            process.env.fwDir = configData.m1fwBase;
            logfile = logger.getLogger(options.serial, '   eMMC', options.serial, configData.m1mtfDir, options.debug);
            if (configData.flashDisable) {
                logfile.error('Flash eMMC is disabled');
                await delay(100);
                process.exit(exitCodes.normalExit);
            }
            if (!options.serial) await errorAndExit('must define vendor serial number', logfile);
            logfile.info('--------------------------------------------');
            logfile.info('Flashing eMMC ...');
            const flashEmmc = new FlashEmmc(configData.layoutFilePath, options.serial, logfile);
            await flashEmmc.init(configData.testBoardTerminalDev, configData.serialBaudrate);
            await flashEmmc.run(configData.programmingCommand, configData.layoutFilePath);
        }
        catch (err) {
            logfile.error(err);
            // logfile.error(err.stack);
            await delay(100);
            process.exit(exitCodes.commandFailed);
        }
    });

program.command('pushtocloud')
    .description('pack the log for sepecified serial and push to the cloudand cleanup')
    .option('-s, --serial <string>', 'vendor serial number')
    .option('-d, --debug <level>', 'set debug level, 0 error, 1 - info, 2 - debug ')
    .action(async (options) => {
        const configData = await config(configuration);
        const now = new Date();
        const timeStamp = dateTime.format(now, 'YYYY_MM_DD_HH_MM_SS');
        let logfile;
        try {
            logfile = console;
            const db = sqliteDriver.initialize(logfile);
            const dbRecord = db.getRecord(options.serial);
            const uid = utils.macToUid(dbRecord[0].uid);
            if (!options.serial) await errorAndExit('must define vendor serial number', logfile);
            logfile.info('Pushing log to Cloud ...');
            if (!fs.existsSync(`${configData.m1mtfDir}/logs/${options.serial}`)) {
                if (!fs.existsSync(`${configData.m1mtfDir}/logs/${uid}-${options.serial}.txz`)) throw new Error('Logs do not exist');
            }
            if (fs.existsSync(`${configData.m1mtfDir}/logs/${options.serial}`)) {
                await os.executeShellCommand(`tar -cJf ${configData.m1mtfDir}/logs/${timeStamp}_${uid}-${options.serial}.txz -C ${configData.m1mtfDir}/logs/${options.serial} .`, logfile, false);
            }

            const logContainer = 'm1-3200-logs';
            const blobSvc = azure.createBlobService(configData.conString);
            await new Promise(async (resolve, reject) => {
                blobSvc.createBlockBlobFromLocalFile(`${logContainer}-${configData.vendorSite}`, `${timeStamp}_${uid}-${options.serial}.txz`, `${configData.m1mtfDir}/logs/${timeStamp}_${uid}-${options.serial}.txz`, async (error) => {
                    if (!error) {
                        resolve();
                        logfile.info(`uploaded file ${timeStamp}_${uid}-${options.serial}.txz`);
                        await delay(100);
                        return;
                    }
                    reject(new Error(error));
                });
            });
        }
        catch (err) {
            if (err === '') logfile.info('Upload failed');
            else logfile.info(err);
            await delay(100);
            process.exit(exitCodes.commandFailed);
        }
    });


program.command('cleanup')
    .description('pack the log and cleanup')
    .option('-s, --serial <string>', 'vendor serial number')
    .option('-d, --debug <level>', 'set debug level, 0 error, 1 - info, 2 - debug ')
    .action(async (options) => {
        const configData = await config(configuration);
        let logfile;
        const now = new Date();
        const timeStamp = dateTime.format(now, 'YYYY_MM_DD_HH_MM_SS');
        try {
            logfile = logger.getLogger(options.serial, '  clean', options.serial, configData.m1mtfDir, options.debug);
            if (!options.serial) await errorAndExit('must define vendor serial number', logfile);
            logfile.info('--------------------------------------------');
            const db = sqliteDriver.initialize(logfile);
            const dbRecord = db.getRecord(options.serial);
            utils.checkDbRecord(dbRecord, true);
            const mac = dbRecord[0].uid;
            const uid = utils.macToUid(mac);
            logfile.info('Cleaning up ...');
            await os.executeShellCommand(`tar -cJf ${configData.m1mtfDir}/logs/${timeStamp}_${uid}-${options.serial}.txz -C ${configData.m1mtfDir}/logs/${options.serial} .`, false);
            // await os.executeShellCommand(`rm -fr ${configData.m1mtfDir}/logs/${options.serial}`, false);
            logfile.info('Done');
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
            process.env.fwDir = configData.m1fwBase;
            process.env.m1defaultIP = configData.m1defaultIP;
            logfile = logger.getLogger(options.serial, '   func', options.serial, configData.m1mtfDir, options.debug);
            if (configData.funcTestDisable) {
                logfile.error('Func test is disabled');
                await delay(100);
                process.exit(exitCodes.normalExit);
            }
            if (!options.serial) await errorAndExit('must define vendor serial number', logfile);
            logfile.info('--------------------------------------------');
            logfile.info('Executing m1-3200 functional test ...');
            process.env.SERIAL = options.serial;
            process.env.logDir = `${configData.m1mtfDir}/logs/${options.serial}`;
            const funcTest = new FuncTest(options.serial, configData, logfile);
            await funcTest.init(configData.testBoardTerminalDev, configData.serialBaudrate);
            await funcTest.run(configData.programmingCommand, configData.layoutFilePath, configData.login, configData.password, configData.m1SerialDev, '115200');
            // await funcTest.run(configData.programmingCommand, configData.layoutFilePath);
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
    .option('-e, --express', 'use database data to print the label')
    .option('-l, --label <string>', 'print label')
    .action(async (options) => {
        const configData = await config(configuration);
        let logfile = console;
        if (!configData.makeLabel) {
            logfile.error('Make Label is disabled');
            await delay(100);
            process.exit(exitCodes.normalExit);
        }
        try {
            if (options.label) {
                const lines = options.label.split(',');
                try {
                    await utils.printCustomLabel(lines, logger);
                }
                catch (err) {
                    //
                }
                process.exit(exitCodes.normalExit);
            }

            process.env.fwDir = configData.m1fwBase;
            let uid;
            let eepromData = {};
            logfile = logger.getLogger(options.serial, '  label', options.serial, configData.m1mtfDir, options.debug);
            if (!options.serial) await errorAndExit('must define vendor serial number', logfile);
            logfile.info('--------------------------------------------');
            logfile.info('Printing Label ...');
            await testBoardLink.initSerial(configData.testBoardTerminalDev, configData.serialBaudrate, logfile);
            if (!options.express) {
                const macProgram = new ProgramMac(configData, options.serial, logfile);
                await macProgram.init(configData.testBoardTerminalDev, configData.serialBaudrate);
                const retValue = await macProgram.getMac(configData.programmingCommand);
                if (retValue.exitCode !== exitCodes.normalExit) throw new Error('Could not read i2c EEPROM');
                uid = retValue.mac;
                const eeprom = new Eeprom(configData.ictFWFilePath, logfile);
                await eeprom.init(configData.testBoardTerminalDev, configData.serialBaudrate, configData.m1SerialDev, configData.serialBaudrate);
                await delay(400);
                eepromData = await eeprom.get(configData.programmingCommand);
            }
            else {
                const db = sqliteDriver.initialize(logfile);
                const dbRecord = db.getRecord(options.serial);
                utils.checkDbRecord(dbRecord, true);
                uid = dbRecord[0].uid;
                eepromData.serial = `SN=${dbRecord[0].boardS2Serial}`;
            }

            if (eepromData.serial === '' || uid === '' || uid === '00:00:00:00:00:00') {
                await buzzer.buzzerBeepFailed();
                await buzzer.testFailed();
                await delay(100);
                process.exit(exitCodes.commandFailed);
            }

            logfile.debug('Sending data to the printer');
            await utils.printLabel(uid, eepromData.serial.substring(3), logfile);
            logfile.debug('Label is printed');
            await buzzer.buzzerBeepSuccess();
            await testBoardLink.targetPower(false);
            await testBoardLink.batteryOn(false);
            await delay(100);
            process.exit(exitCodes.normalExit);
        }
        catch (err) {
            if (!logfile) logfile = console;
            logfile.error(err.message);
            // logfile.debug(err.stack);
            await delay(100);
            await buzzer.buzzerBeepFailed();
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

'use strict';

const delay = require('delay');
const logger = require('../utils/logger');
const config = require('../utils/config');
const sqliteDriver = require('../utils/sqliteDriver');
const IctTestRunner = require('../tests/ictTestRunner');
const testBoardLink = require('../src/testBoardLink');
const exitCodes = require('../src/exitCodes');

const configuration = {
    tfInterface: 'eth1',
    mtfDir: `${process.env.HOME}/m1mtf`,
    ictFWFilePath: 'fsbl.stm32',
    fwDir: 'stm32mp15-lenels2-m1',
    layoutFilePath: 'flashlayout_st-ls2m1-image-core/trusted/FlashLayout_emmc_stm32mp151f-ls2m1-trusted.tsv',
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
    skipRS485test: false,
    skipBatteryTest: false,
    pingPorts: true,
    progEEPROM: true,
    makeLabel: true,
    funcTestDisable: false,
    coinCellDebug: true,
    productName: 'm1-3200'
};

async function exitCommandFailure(err, log = console, exitCode = exitCodes.commandFailed, logMessageOnly = false, delayMs = 100) {
    if (err) {
        if (logMessageOnly && err.message) {
            log.error(err.message);
        } else {
            log.error(err);
        }
    }
    await delay(delayMs);
    process.exit(exitCode);
}

async function errorAndExit(errorStr, log) {
    await exitCommandFailure(errorStr, log, exitCodes.commandFailed, false, 500);
}

async function loadConfigData() {
    const configData = await config.getConfig(configuration);
    process.env.productName = configData.productName;
    return configData;
}

async function ensureSerialOption(options, log = console) {
    if (!options.serial) {
        await errorAndExit('must define vendor serial number', log);
    }
}

function applyFirmwareDir(configData) {
    process.env.fwDir = `${configData.mtfDir}/${configData.fwDir}`;
}

function createCommandLogger(serial, commandTag, configData, debugLevel) {
    return logger.getLogger(serial, commandTag, serial, configData.mtfDir, debugLevel);
}

function initDb(logFile) {
    return sqliteDriver.initialize(logFile);
}

async function createIctRunner(configData, logFile, ictFwFilePath = `${configData.mtfDir}/${configData.ictFWFilePath}`) {
    const ictTestRunner = new IctTestRunner(ictFwFilePath, configData.tolerance, logFile, configData, false);
    await ictTestRunner.init(configData.testBoardTerminalDev, configData.serialBaudrate, configData.m1SerialDev, configData.serialBaudrate);
    await delay(400);
    return ictTestRunner;
}

async function powerDownTarget() {
    await testBoardLink.targetPower(false);
    await testBoardLink.batteryOn(false);
}

module.exports = {
    configuration,
    errorAndExit,
    loadConfigData,
    ensureSerialOption,
    applyFirmwareDir,
    createCommandLogger,
    initDb,
    createIctRunner,
    powerDownTarget,
    exitCommandFailure
};

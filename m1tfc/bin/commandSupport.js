'use strict';

const os = require('os');
const path = require('path');
const delay = require('delay');
const config = require('../utils/config');
const exitCodes = require('../src/exitCodes');
const runtimeContext = require('../utils/runtimeContext');

const homeDir = process.env.HOME || os.homedir();

// Device paths and network configuration
const DEVICE_PATHS = {
    M1_SERIAL: '/dev/ttyUSB0',
    TEST_BOARD_TERMINAL: '/dev/ttyACM0'
};

const NETWORK_CONFIG = {
    TF_INTERFACE: 'eth1',
    M1_DEFAULT_IP: '192.168.0.251'
};

const FIRMWARE_CONFIG = {
    ICT_FW_FILE: 'fsbl.stm32',
    FW_DIR: 'stm32mp15-lenels2-m1',
    LAYOUT_FILE: 'flashlayout_st-ls2m1-image-core/trusted/FlashLayout_emmc_stm32mp151f-ls2m1-trusted.tsv'
};

const SERIAL_CONFIG = {
    LOGIN: 'root',
    PASSWORD: process.env.M1_PASSWORD || 'only4u2c',
    BAUDRATE: 115200
};

const defaultConfiguration = {
    tfInterface: NETWORK_CONFIG.TF_INTERFACE,
    mtfDir: `${homeDir}/m1mtf`,
    ictFWFilePath: FIRMWARE_CONFIG.ICT_FW_FILE,
    fwDir: FIRMWARE_CONFIG.FW_DIR,
    layoutFilePath: FIRMWARE_CONFIG.LAYOUT_FILE,
    programmingCommand: `${homeDir}/STMicroelectronics/STM32Cube/STM32CubeProgrammer/bin/STM32_Programmer_CLI`,
    m1SerialDev: DEVICE_PATHS.M1_SERIAL,
    m1defaultIP: NETWORK_CONFIG.M1_DEFAULT_IP,
    testBoardTerminalDev: DEVICE_PATHS.TEST_BOARD_TERMINAL,
    tolerance: 0.05,
    login: SERIAL_CONFIG.LOGIN,
    password: SERIAL_CONFIG.PASSWORD,
    serialBaudrate: SERIAL_CONFIG.BAUDRATE,
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

function ensureSnapEnv() {
    if (process.env.SNAP) return;
    process.env.SNAP_COMMON = `${homeDir}/snap_common`;
    process.env.SNAP_DATA = `${homeDir}/snap_data`;
    process.env.SNAP = '/snap/m1tfd/current';
    process.env.SNAP_VERSION = process.env.SNAP_VERSION || 'dev';
}

async function errorAndExit(errorStr, log) {
    log.error(errorStr);
    await delay(500);
    process.exit(exitCodes.commandFailed);
}

function applyRuntime(configData, extra = {}) {
    runtimeContext.setRuntime({
        dbPath: configData.mtfDir,
        productName: configData.productName,
        m1defaultIP: configData.m1defaultIP,
        skipBatteryTest: !!configData.skipBatteryTest,
        coinCellDebug: !!configData.coinCellDebug,
        fwDir: path.join(configData.mtfDir, configData.fwDir),
        ...extra
    });
}

async function loadConfig() {
    const configData = await config.getConfig({ ...defaultConfiguration });
    applyRuntime(configData);
    return configData;
}

module.exports = {
    defaultConfiguration,
    ensureSnapEnv,
    errorAndExit,
    applyRuntime,
    loadConfig
};

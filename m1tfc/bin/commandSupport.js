'use strict';

const os = require('os');
const path = require('path');
const delay = require('delay');
const config = require('../utils/config');
const exitCodes = require('../src/exitCodes');
const runtimeContext = require('../utils/runtimeContext');

const homeDir = process.env.HOME || os.homedir();

const defaultConfiguration = {
    tfInterface: 'eth1',
    mtfDir: `${homeDir}/m1mtf`,
    ictFWFilePath: 'fsbl.stm32',
    fwDir: 'stm32mp15-lenels2-m1',
    layoutFilePath: 'flashlayout_st-ls2m1-image-core/trusted/FlashLayout_emmc_stm32mp151f-ls2m1-trusted.tsv',
    programmingCommand: `${homeDir}/STMicroelectronics/STM32Cube/STM32CubeProgrammer/bin/STM32_Programmer_CLI`,
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

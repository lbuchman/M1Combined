'use strict';

const fs = require('fs-extra');
const delay = require('delay');
const si = require('systeminformation');
const logger = require('../../utils/logger');
const IctTestRunner = require('../../tests/ictTestRunner');
const os = require('../../utils/os');
const sqliteDriver = require('../../utils/sqliteDriver');
const targetICTLink = require('../../src/m1ICTLink');
const exitCodes = require('../../src/exitCodes');
const errorCodes = require('../errorCodes');
const { loadConfig, errorAndExit, applyRuntime } = require('../commandSupport');

/**
 * Pre-check hardware connections (network, USB devices, printer)
 * @returns {Promise<boolean>} true if all checks pass, false otherwise
 */
async function preCheckHardware(configData, logfile) {
    const devices = [
        { name: '/dev/ttyACM0', desc: 'Testboard Teensy' },
        { name: '/dev/ttyUSB0', desc: 'M1-3200 Terminal Serial Converter' }
    ];
    
    let allChecksPass = true;

    // Check label printer
    if (configData.makeLabel) {
        const printerStatus = await os.executeShellCommand('lsusb | grep "QL-810W"', logfile);
        if (!printerStatus) {
            allChecksPass = false;
            logfile.error('Label Printer is not detected. Check connections and power and retry the test.');
        }
    }

    // Check network interfaces
    const interfaces = await si.networkInterfaces();
    const tfInterface = configData.tfInterface || 'enp0s31f6';
    
    if (interfaces.find(o => o.iface === tfInterface) === undefined) {
        allChecksPass = false;
        logfile.error(`Internet Ethernet jack (${tfInterface}) is not plugged. Check connection and retry the test.`);
    }

    if (interfaces.find(o => o.ip4 === '192.168.0.100') === undefined) {
        allChecksPass = false;
        logfile.error('M1-3200 Ethernet jack is not plugged. Check connection and retry the test.');
    }

    // Check USB devices
    for (const deviceFile of devices) {
        if (!fs.existsSync(deviceFile.name)) {
            allChecksPass = false;
            logfile.error(`${deviceFile.desc} is not found. Check connections and retry the test.`);
        }
    }

    return allChecksPass;
}

function register(program) {
    program.command('ict')
        .description('Executes ICT test')
        .option('-s, --serial <string>', 'vendor serial number')
        .option('-d, --debug <level>', 'set debug level, 0 error, 1 - info, 2 - debug')
        .option('-b, --cellBatTol <cellBatTol>', 'tolerance for coin cell bat, valid values: new, used')
        .option('-c, --callibrate <callibrate>', 'calibrate A/D and save data into the config file')
        .option('-v, --cellBatVoltage <cellBatVoltage>', 'only for the calibration, measure cell bat voltage and enter it like: -v 3.0145')
        .action(async (options) => {
            const configData = await loadConfig();
            let logfile;
            let db;

            const calibrate = options.callibrate === 'true';
            applyRuntime(configData, {
                serial: options.serial,
                debugLevel: options.debug || '0',
                cellBatTol: options.cellBatTol,
                cellBatVoltage: options.cellBatVoltage ? Number(options.cellBatVoltage) : null,
                logDir: options.serial ? `${configData.mtfDir}/logs/${options.serial}` : null
            });

            try {
                if (!options.serial) await errorAndExit('must define vendor serial number', console);
                logfile = logger.getLogger(options.serial, '    ict', options.serial, configData.mtfDir, options.debug);
                db = sqliteDriver.initialize(logfile);

                // Skip hardware checks only in developer debug mode
                if (options.debug !== '2') {
                    const hardwareOk = await preCheckHardware(configData, logfile);
                    if (!hardwareOk) {
                        await delay(500);
                        process.exit(exitCodes.precheckHWFailed);
                    }
                }

                logfile.info('Starting ICT Test.');
                await targetICTLink.initSerial(configData.m1SerialDev, 115200, logfile);
                if (!options.cellBatTol) await errorAndExit('must define cellBatTol', logfile);
                if ((options.cellBatTol !== 'new') && (options.cellBatTol !== 'used')) await errorAndExit('cellBatTol argument  -b option is not valid', logfile);

                logfile.info(`Executing ICT command ${configData.mtfDir}/${configData.ictFWFilePath} ...`);
                const ictTestRunner = new IctTestRunner(`${configData.mtfDir}/${configData.ictFWFilePath}`, configData.tolerance, logfile, configData);
                await ictTestRunner.init(configData.testBoardTerminalDev, configData.serialBaudrate, configData.m1SerialDev, configData.serialBaudrate);
                await delay(400);

                const skipTestpointCheck = options.debug ? (configData.skipTestpointCheck || false) : false;
                const memTestSize1MBBlocks = options.debug ? (configData.memTestSize1MBBlocks || 512) : 10;
                const ictExitCode = await ictTestRunner.runTest(
                    configData.programmingCommand,
                    options.serial,
                    memTestSize1MBBlocks,
                    skipTestpointCheck,
                    false,
                    calibrate
                );
                process.exit(ictExitCode);
            }
            catch (err) {
                if (!logfile) logfile = console;
                logfile.error(err);
                if (db && options.serial) {
                    db.updateErrorCode(options.serial, errorCodes.codes.ICT_EXCEPT.errorCode, 'E');
                }
                await delay(100);
                process.exit(exitCodes.commandFailed);
            }
        });
}

module.exports = {
    register
};

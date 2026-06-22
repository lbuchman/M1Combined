'use strict';

const delay = require('delay');
const fs = require('fs-extra');
const os = require('../../utils/os');
const si = require('systeminformation');
const targetICTLink = require('../../src/m1ICTLink');
const exitCodes = require('../../src/exitCodes');
const errorCodes = require('../../bin/errorCodes');
const config = require('../../utils/config');
const {
    loadConfigData,
    ensureSerialOption,
    createCommandLogger,
    initDb,
    createIctRunner
} = require('../m1tfcShared');

module.exports = function registerIct(program) {
    program.command('ict')
        .description('Executes ICT test')
        .option('-s, --serial <string>', 'vendor serial number')
        .option('-d, --debug <level>', 'set debug level, 0 error, 1 - info, 2 - debug')
        .option('-b, --cellBatTol <cellBatTol>', 'tolerance for coin cell bat, valid values: new, used')
        .option('-c, --callibrate <callibrate>', 'calibrate A/D and save data into the config file, set -c to board Id from the board label')
        .option('-v, --cellBatVoltage <cellBatVoltage>', 'only for the calibration, measure cell bat voltage and enter it like: -v 3.0145')
        .action(async (options) => {
            const configData = await loadConfigData();
            let logfile;
            let db;
            let startStatusOk = true;
            let isCallibrate = false;
            let exitCode = exitCodes.normalExit;

            process.env.debug = options.debug;
            try {
                await ensureSerialOption(options, console);
                logfile = createCommandLogger(options.serial, '    ict', configData, options.debug);
                db = initDb(logfile);
                const devices = [
                    { name: '/dev/ttyACM0', desc: 'Testboard Teensy' },
                    { name: '/dev/ttyUSB0', desc: 'M1-3200 Terminal Serial Converter' }
                ];

                if (options.cellBatVoltage) {
                    process.env.cellBatVoltage = options.cellBatVoltage;
                }

                if (options.debug !== '2') {
                    if (configData.makeLabel) {
                        const printerStatus = await os.executeShellCommand('lsusb | grep "QL-810W"', logfile);
                        if (!printerStatus) {
                            startStatusOk = false;
                            logfile.error('Label Printer is not detected. Check connections and power and retry the test.');
                        }
                    }

                    const interfaces = await si.networkInterfaces();
                    if (!configData.tfInterface) configData.tfInterface = 'enp0s31f6';
                    if (interfaces.find(o => o.iface === configData.tfInterface) === undefined) {
                        startStatusOk = false;
                        logfile.error('Internet Ethernet jack is not plugged. Check connection and retry the test.');
                    }

                    if (interfaces.find(o => o.ip4 === '192.168.0.100') === undefined) {
                        startStatusOk = false;
                        logfile.error('M1-3200 Ethernet jack is not plugged. Check connection and retry the test.');
                    }
                }

                devices.forEach((deviceFile) => {
                    const exists = fs.existsSync(deviceFile.name);
                    if (!exists) {
                        startStatusOk = false;
                        logfile.error(`${deviceFile.desc} is not found. Check connections and retry the test.`);
                    }
                });

                if (!startStatusOk) {
                    await delay(500);
                    exitCode = exitCodes.precheckHWFailed;
                    return;
                }

                logfile.info('Starting ICT Test.');
                await targetICTLink.initSerial(configData.m1SerialDev, 115200, logfile);
                if (!options.cellBatTol) throw new Error('must define cellBatTol');
                logfile.info(`Executing ICT command ${configData.mtfDir}/${configData.ictFWFilePath} ...`);
                const ictTestRunner = await createIctRunner(configData, logfile);
                if ((options.cellBatTol !== 'new') && (options.cellBatTol !== 'used')) {
                    throw new Error('cellBatTol argument  -b option is not valid');
                }
                process.env.cellBatTol = options.cellBatTol;
                isCallibrate = options.callibrate === 'true';
                let skipTestpointCheck = false;
                let memTestSize1MBBlocks = 10;
                if (options.debug) {
                    skipTestpointCheck = configData.skipTestpointCheck || false;
                    memTestSize1MBBlocks = configData.memTestSize1MBBlocks || 512;
                }
                exitCode = await ictTestRunner.runTest(configData.programmingCommand, options.serial, memTestSize1MBBlocks, skipTestpointCheck, false, isCallibrate);
            }
            catch (err) {
                if (!logfile) logfile = console;
                logfile.error(err);
                if (isCallibrate) {
                    await config.saveConfig(configData);
                }
                if (db) db.updateErrorCode(options.serial, errorCodes.codes.ICT_EXCEPT.errorCode, 'E');
                await delay(100);
                exitCode = exitCodes.commandFailed;
            }
            finally {
                process.exit(exitCode);
            }
        });
};

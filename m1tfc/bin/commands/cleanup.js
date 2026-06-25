'use strict';

const delay = require('delay');
const dateTime = require('date-and-time');
const sqliteDriver = require('../../utils/sqliteDriver');
const utils = require('../../utils/utils');
const os = require('../../utils/os');
const exitCodes = require('../../src/exitCodes');
const { loadConfig, errorAndExit, applyRuntime } = require('../commandSupport');

function register(program) {
    program.command('cleanup')
        .description('pack the log and cleanup')
        .option('-s, --serial <string>', 'vendor serial number')
        .option('-e, --failed', 'will append E to the tar ball file name')
        .action(async(options) => {
            const configData = await loadConfig();
            const logfile = console;
            const now = new Date();
            applyRuntime(configData, { serial: options.serial });
            const timeStamp = dateTime.format(now, 'YYYY_MM_DD_HH_mm_ss');
            try {
                if (!options.serial) {
                    await errorAndExit('must define vendor serial number', console);
                }
                const db = sqliteDriver.initialize(logfile);
                const dbRecord = db.getRecord(options.serial);
                const mac = dbRecord[0].uid;
                const uid = mac ? utils.macToUid(mac) : '0000000000000000';
                const errSuf = options.failed ? 'E' : '';
                const tarFile = `${configData.mtfDir}/logs/${timeStamp}_${uid}-${options.serial}${configData.vendorSite}${errSuf}.txz`;
                await os.executeShellCommand(`tar -cJf ${tarFile} -C ${configData.mtfDir}/logs/${options.serial} .`, logfile, false);
                await os.executeShellCommand(`rm -fr ${configData.mtfDir}/logs/${options.serial}`, logfile, false);
                await delay(100);
            } catch (err) {
                logfile.error(err);
                await delay(100);
                process.exit(exitCodes.commandFailed);
            }
        });
}

module.exports = {
    register
};

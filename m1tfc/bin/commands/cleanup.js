'use strict';

const delay = require('delay');
const dateTime = require('date-and-time');
const exitCodes = require('../../src/exitCodes');
const utils = require('../../utils/utils');
const os = require('../../utils/os');
const { loadConfigData, ensureSerialOption, initDb } = require('../m1tfcShared');

module.exports = function registerCleanup(program) {
    program.command('cleanup')
        .description('pack the log and cleanup')
        .option('-s, --serial <string>', 'vendor serial number')
        .option('-e, --failed', 'will append E to the tar ball file name')
        .action(async (options) => {
            const configData = await loadConfigData();
            const logfile = console;
            const now = new Date();
            const timeStamp = dateTime.format(now, 'YYYY_MM_DD_HH_mm_ss');
            let exitCode = exitCodes.normalExit;
            try {
                await ensureSerialOption(options, console);
                const db = initDb(logfile);
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
                const tarFile = `${configData.mtfDir}/logs/${timeStamp}_${uid}-${options.serial}${configData.vendorSite}${errSuf}.txz`;
                await os.executeShellCommand(`tar -cJf ${tarFile} -C ${configData.mtfDir}/logs/${options.serial} .`, logfile, false);
                await os.executeShellCommand(`rm -fr ${configData.mtfDir}/logs/${options.serial}`, logfile, false);
                await delay(100);
            }
            catch (err) {
                logfile.error(err);
                exitCode = exitCodes.commandFailed;
            }
            finally {
                process.exit(exitCode);
            }
        });
};

#!/usr/bin/env node

'use strict';

const program = require('commander');
const { mkdirp } = require('mkdirp');
const crypto = require('crypto');
const fs = require('fs-extra');
const lodash = require('lodash');
const dateTime = require('date-and-time');
const Base32 = require('base32.js');
const azure = require('azure-storage');
const glob = require('glob');
const path = require('path');
const azureOp = require('../src/azureOp');
const secrets = require('../src/secrets');
const config = require('../src/config');
const os = require('../src/os');
const logger = require('../src/logger');

let logContainer = '-logs';
let secretsContainer = '-secrets';
const firmwareContainer = 'firmware';
process.env.SNAP_DATA = '/var/snap/m1tfd1/current';
const publicKey = path.join(process.env.SNAP_DATA, 'public.key');
const debuglevel = '2';

const UpdateFwTimeStamp = 'UpdateFwTimeStamp.txt';
const UpdateSycretsTimeStamp = 'UpdateSycretsTimeStamp.txt';
const UpdateLogsTimeStamp = 'UpdateLogsTimeStamp.txt';

if (!process.env.VERSION) process.env.VERSION = '123456';

program
    .name('m1cli')
    .description('CLI utility retrieve M1-3200 manufacturing logs')
    .version(process.env.SNAP_VERSION);

program.command('update')
    .description('download and update software and firmware on the test fixture')
    .option('-f, --force', 'force update')
    .action(async (options) => {
        let logfile;
        let dir;
        let blobSvc;
        try {
            const dateNow = os.getDate();
            const str = '/root snap install/d';
            os.executeShellCommand(`sudo sed -i '${str}' /etc/crontab`, logfile);
            const configData = await config({ m1mtfDir: '/home/lenel/m1mtf' });
            dir = configData.m1mtfDir;
            logfile = logger.getLogger('m1cli', 'update', 'm1cli', `${configData.m1mtfDir}/m1cli`, debuglevel);
            logfile.info('Checking for SW & FW update ...');
            blobSvc = azure.createBlobService(configData.conString);
            logfile.info('Downloading manifestFile');
            mkdirp.sync(path.join(dir, 'tmp'));
            await azureOp.downloadFile(blobSvc, firmwareContainer, 'manifestFile.json', path.join(dir, 'tmp', 'manifestFile.json'));
            const newManifestFile = fs.readJSONSync(path.join(dir, 'tmp', 'manifestFile.json'));
            let localManifestFile;
            try {
                if (options.force) throw Error('force');
                localManifestFile = fs.readJSONSync(path.join(dir, 'tmp', 'manifestFileLocal.json'));
            }
            catch (err) {
                localManifestFile = [];
            }
            if (lodash.isEqual(newManifestFile, localManifestFile)) {
                logfile.info('No new firmware to update');
                fs.writeFileSync(`${configData.m1mtfDir}/${UpdateFwTimeStamp}`, dateNow);
                return;
            }
            logfile.info('New FW is available');
            const isSnapUpdate = newManifestFile.find(element => element.filetype === 'snap');
            if (isSnapUpdate) {
                logfile.info('sending kill to apps');
                await os.executeShellCommand('killall -9 gui', logfile, true);
            }
            const fileList = newManifestFile.map((item) => {
                logfile.info(`Downloading ${item.filename}`);
                return azureOp.downloadFile(blobSvc, firmwareContainer, item.filename, path.join(dir, 'tmp', item.filename));
            });
            await Promise.all(fileList);
            logfile.info('Download complete, checking hashes');
            azureOp.checkFilesHash(newManifestFile, path.join(dir, 'tmp'));
            logfile.info('Hashes are fine');
            const promises = newManifestFile.map(async (item) => {
                logfile.info(`updating ${item.filetype}`);
                switch (item.filetype) {
                    case 'snapclient':
                        return os.executeShellCommand(`sudo echo "20  4  * * *   root snap install --classic --dangerous ${path.join(dir, 'tmp', item.filename)}" >> /etc/crontab`, logfile);
                    case 'snap':
                        return os.executeShellCommand(`kill -9 ${os.getFrontendPid()}`, logfile, true)
                            .then(() => {
                                return os.executeShellCommand(`sudo snap install --classic --dangerous ${path.join(dir, 'tmp', item.filename)}`, logfile, false);
                            });
                    case 'stm':
                        return os.executeShellCommand(`cp -f ${path.join(dir, 'tmp', item.filename)} ${dir}`, logfile, false);
                    case 'txz':
                        await os.executeShellCommand(`rm -fr ${path.join(dir, 'stm32mp15')}*`, logfile, false);
                        return os.executeShellCommand(`tar -xf ${path.join(dir, 'tmp', item.filename)} -C ${dir}`, logfile, false);
                    default: throw new Error(`Invalid filetype ${item.filetype}`);
                }
            });
            await Promise.all(promises);
            logfile.info(`${configData.m1mtfDir}/${UpdateFwTimeStamp}`);
            fs.writeFileSync(`${configData.m1mtfDir}/${UpdateFwTimeStamp}`, dateNow);

            await os.executeShellCommand(`cp -f ${path.join(dir, 'tmp', 'manifestFile.json')} ${path.join(dir, 'tmp', 'manifestFileLocal.json')}`, logfile);
            logfile.info('Done');
        }
        catch (err) {
            const logconsole = console;
            if (logfile !== undefined) logfile.error(err);
            else logconsole.error(err);
        }
    });


program.command('synclogs')
    .description('sync logs into Cloud AS')
    .action(async () => {
        let noError = true;
        const dateNow = os.getDate();
        const configData = await config({ m1mtfDir: '/home/lenel/m1mtf' });
        const matches = glob.sync(`${configData.m1mtfDir}/logs/*.txz`, { nonull: false, realpath: true });
        const logfile = logger.getLogger('m1cli', 'synclogs', 'm1cli', `${configData.m1mtfDir}/m1cli`, debuglevel);
        const blobSvc = azure.createBlobService(configData.conString);

        if (!matches.length) {
            logfile.info('No log files to upload');
            fs.writeFileSync(`${configData.m1mtfDir}/${UpdateLogsTimeStamp}`, dateNow);
            return;
        }
        try {
            logContainer = `${configData.productName}-logs-${configData.vendorSite}`;
            logfile.info(`Container: ${logContainer}`);
            await azureOp.syncFiles(blobSvc, `${logContainer.toLowerCase()}`, matches);
        }
        catch (err) {
            logfile.error(err.message);
            return;
        }

        logfile.info('Download complete files uploaded:');
        const syncedLogsDir = `${configData.m1mtfDir}/syncedlogs`;
        mkdirp.sync(syncedLogsDir);
        await os.executeShellCommand(`chown lenel: ${syncedLogsDir}`, logfile);
        matches.forEach((item) => {
            logfile.info(path.basename(item));
            try {
                fs.moveSync(item, path.join(`${syncedLogsDir}`, path.basename(item)));
            }
            catch (err) {
                noError = false;
                logfile.error(err.message);
            }
        });
        if (noError) fs.writeFileSync(`${configData.m1mtfDir}/${UpdateLogsTimeStamp}`, dateNow);
    });

program.command('backupdb')
    .description('backup DB to the cloud')
    .action(async () => {
        // const dateNow = os.getDate();
        const configData = await config({ m1mtfDir: '/home/lenel/m1mtf' });
        const logfile = logger.getLogger('m1cli', 'backupdb', 'm1cli', `${configData.m1mtfDir}/m1cli`, debuglevel);
        const dbFile = path.join(configData.m1mtfDir, 'tf.db');
        const blobSvc = azure.createBlobService(configData.conString);
        blobSvc.createBlockBlobFromLocalFile('backup', `${configData.vendorSite}_${path.basename(dbFile)}`, dbFile, (err) => {
            if (err) {
                logfile.error(err.message);
            }
        });

        logfile.info(`Download complete DB file ${dbFile} is uploaded:`);
    });


function padBase32(str) {
    switch (str.length % 8) {
        case 2:
            return `${str}======`;
        case 4:
            return `${str}====`;
        case 5:
            return `${str}===`;
        case 7:
            return `${str}=`;
        default:
            return str;
    }
}

function fromHexString(hexString) {
    return Uint8Array.from(Buffer.from(hexString, 'hex'));
}

function getEncryptedSecretBase32(buffer) {
    const publicKeyData = fs.readFileSync(publicKey);
    const encryptedBuffer = crypto.publicEncrypt(publicKeyData, buffer);
    const encoder = new Base32.Encoder({ type: 'rfc4648', lc: false, pad: '=' });
    const str = encoder.write(encryptedBuffer).finalize();
    return padBase32(str);
}

program.command('syncsecrets')
    .description('sync M1-3200 secrets into Cloud AS')
    .action(async () => {
        const dateNow = os.getDate();
        const configData = await config({ m1mtfDir: '/home/lbuchman/m1mtf' });
        const logfile = logger.getLogger('m1cli', 'syncsecrets', 'm1cli', `${configData.m1mtfDir}/m1cli`, debuglevel);
        secrets.initialize(configData.m1mtfDir, logfile);
        const now = new Date();
        const filename = `/tmp/${dateTime.format(now, 'YYYY_MM_DD_HH_mm_ss')}.csv`;
        const pattern = dateTime.compile('YYYY-MM-DDTHH:mm:ssZZ');
        const date = dateTime.format(now, pattern);
        try {
            const blobSvc = azure.createBlobService(configData.conString);
            const db = secrets.initialize(configData.m1mtfDir, logfile);
            const records = db.getRecords();
            if (!records.length) {
                logfile.info('No secrets to sync');
                fs.writeFileSync(`${configData.m1mtfDir}/${UpdateSycretsTimeStamp}`, dateNow);
                return;
            }
            let firstLine = true;
            secretsContainer = `${configData.productName}-secrets`;
            logfile.info(`Container: ${secretsContainer}`);
            records.forEach((board) => {
                if (!board.uid || !board.secret || !board.boardS2Serial) return;
                const data = {
                    date,
                    uid: `0000${board.uid.split(':').join('')}`,
                    secret: board.secret,
                    serial: board.boardS2Serial
                };

                const secretBuffer = getEncryptedSecretBase32(fromHexString(data.secret.substring(3)));
                const line = `${data.date}|${data.uid}|${secretBuffer}|${data.serial.substring(3)}`;
                if (firstLine) fs.writeFileSync(filename, `${line}\n`);
                else fs.writeFileSync(filename, `${line}\n`, { flag: 'a+' });
                firstLine = false;
            });
            if (firstLine) {
                logfile.info('No secrets to sync');
                fs.writeFileSync(`${configData.m1mtfDir}/${UpdateSycretsTimeStamp}`, dateNow);
                return;
            }
            await azureOp.syncFiles(blobSvc, secretsContainer, [filename]);
            fs.unlinkSync(filename);

            records.forEach((board) => {
                db.updateRecord(board.vendorSerial);
            });
            fs.writeFileSync(`${configData.m1mtfDir}/${UpdateSycretsTimeStamp}`, dateNow);
            logfile.info('Done');
        }
        catch (err) {
            logfile.error(err.message);
        }
    });

program.parse(process.argv);

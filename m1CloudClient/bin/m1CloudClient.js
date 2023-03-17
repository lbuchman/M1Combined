#!/usr/bin/env node

'use strict';

const program = require('commander');
const fs = require('fs-extra');
const azure = require('azure-storage');
const path = require('path');
const azureOp = require('../src/azureOp');

const conString = 'DefaultEndpointsProtocol=https;AccountName=lenels2production;AccountKey=QYLAhAvw4SowA6HJqBm2CTkR3Q36PWxfFUPDhqZ4yEyK6lJNb02GEwS/Z/kF8+qMEPs6VAzSnHHt+AStrIr3VQ==;EndpointSuffix=core.windows.net';
const logContainer = 'm1-3200-logs';
const secretsContainer = 'm1-3200-secrets';
const firmwareContainer = 'firmware';

program
    .name('m1cli')
    .description('CLI utility retrieve M1-3200 manufacturing logs')
    .version('0.1.0');

program.command('uploadfw')
    .description('upload software and firmware to Cloud')
    .option('-t, --txz <string>', 'full path to M1-3200 firmware compressed tar file .txz')
    .option('-m, --stm <string>', 'full path to M1-3200 stm32 file')
    .option('-s, --snap <string>', 'full path to the test fixture snap file')
    .option('-d, --dir <string>', 'firmware directory')
    .option('-c, --snapclient <string>', 'full path to the test fixture snap client file')
    .action(async (options) => {
        const logfile = console;
        logfile.info('Updating files on the Cloud ...');
        try {
            const blobSvc = azure.createBlobService(conString);
            const now = new Date();
            const manifestFile = [];
            const files = [];
            manifestFile.date = now.toISOString();
            if (options.txz) {
                files.push(options.txz);
                manifestFile.push({ filetype: 'txz', filename: path.basename(options.txz), hash: azureOp.getHash512FromFile(options.txz) });
            }
            if (options.stm) {
                files.push(options.stm);
                manifestFile.push({ filetype: 'stm', filename: path.basename(options.stm), hash: azureOp.getHash512FromFile(options.stm) });
            }
            if (options.snap) {
                files.push(options.snap);
                manifestFile.snap = options.snap;
                manifestFile.snapHash = azureOp.getHash512FromFile(options.snap);
                manifestFile.push({ filetype: 'snap', filename: path.basename(options.snap), hash: azureOp.getHash512FromFile(options.snap) });
            }
            if (options.snapclient) {
                files.push(options.snapclient);
                manifestFile.snapclient = options.snapclient;
                manifestFile.snapHash = azureOp.getHash512FromFile(options.snap);
                manifestFile.push({ filetype: 'snapclient', filename: path.basename(options.snapclient), hash: azureOp.getHash512FromFile(options.snapclient) });
            }
            if (!files.length) {
                logfile.error('no files specified');
                return;
            }
            logfile.info('manifestFile:');
            logfile.info(JSON.stringify(manifestFile, null, 2));
            fs.writeFileSync('/tmp/manifestFile.json', JSON.stringify(manifestFile, null, 2));
            await azureOp.syncFiles(blobSvc, firmwareContainer, files);
            await azureOp.syncFiles(blobSvc, firmwareContainer, ['/tmp/manifestFile.json']);
            logfile.info('Done');
        }
        catch (err) {
            logfile.error(err.message);
        }
    });

program.command('getsecrets')
    .description('get the secrets file from the Cloud')
    .argument('<timeStamp>', 'date prefix, the date format is 2023_02_11_16 and used as prefix, so if date = 2023 then all files from 2023 are returned, if date = 2023_02_11_16 only for this date files are ruturned')
    .action(async (timeStamp) => {
        const logfile = console;
        try {
            const blobSvc = azure.createBlobService(conString);
            const entries = await azureOp.getList(blobSvc, secretsContainer, [], timeStamp, null);
            if (!entries && !entries.length) {
                logfile.info('No files to download');
                return;
            }

            const promises = entries.map((file) => {
                logfile.info(`Downloading file ${file.name}`);
                return azureOp.downloadFile(blobSvc, secretsContainer, file.name);
            });

            await Promise.all(promises);
            logfile.info('Done');
        }
        catch (err) {
            logfile.error(err.message);
        }
    });

program.command('getlog')
    .description('get the log from the Cloud')
    .option('-t, --teststation <string>', 'test station id like s1, s2 etc')
    .argument('<logId>', 'log file name or uid or serial number from the barcode')
    .action(async (str, options) => {
        const logfile = console;
        try {
            const blobSvc = azure.createBlobService(conString);
            const ret = await azureOp.getFileFullname(blobSvc, `${logContainer}-${options.teststation}`, str);
            if (ret.length === 0) {
                logfile.info('no such log is available');
                return;
            }
            const files = ret.filter((element) => {
                return element !== null;
            });

            if (!files.length) {
                logfile.info('no such log is available');
                return;
            }

            await azureOp.downloadFile(blobSvc, `${logContainer}-${options.teststation}`, files[0]);
            logfile.info(`downloaded: ${JSON.stringify(files[0])}`);
        }
        catch (err) {
            logfile.error(err.message);
        }
    });

program.command('listsecrets')
    .description('list secrets avaialble in the Cloud')
    .argument('<date ref>', 'date prefix, the date format is 2023_02_11_16 and used as prefix, so if date = 2023 then all file for 2023 are returned, if date = 2023_02_11_16 only for this date files are ruturned')
    .action(async (date) => {
        const logfile = console;
        if (!date) {
            logfile.error('must specify date, see help');
            return;
        }
        try {
            const blobSvc = azure.createBlobService(conString);
            const entries = await azureOp.getList(blobSvc, secretsContainer, [], date, null);
            entries.forEach((element) => {
                logfile.log(element.name);
            });
        }
        catch (err) {
            logfile.error(err.message);
        }
    });


program.command('listlogs')
    .description('list logs avaialble in the Cloud')
    .option('-t, --teststation <string>', 'test station id like s1, s2 etc')
    .argument('<date ref>', 'date prefix, the date format is 2023_02_11_16 and used as prefix, so if date = 2023 then all file for 2023 are returned, if date = 2023_02_11_16 only for this date files are ruturned')
    .action(async (date, options) => {
        const logfile = console;
        if (!date) {
            logfile.error('must specify date, see help');
            return;
        }
        try {
            const blobSvc = azure.createBlobService(conString);
            const entries = await azureOp.getList(blobSvc, `${logContainer}-${options.teststation}`, [], date, null);
            entries.forEach((element) => {
                logfile.log(element.name);
            });
        }
        catch (err) {
            logfile.error(err.message);
        }
    });

program.parse(process.argv);

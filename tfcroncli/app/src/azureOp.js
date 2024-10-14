'use strict';

/* eslint-disable no-param-reassign */

const crypto = require('crypto');

const path = require('path');
const fs = require('fs-extra');

async function listBlobsSegmentedWithPrefix(blobSvc, container, list, prefix, token) {
    return new Promise((resolve, reject) => {
        blobSvc.listBlobsSegmentedWithPrefix(container, prefix, token, (err, data) => {
            if (err) {
                reject(err);
                return;
            }
            resolve(data);
        });
    });
}

async function getList(blobSvc, container, list, prefix, token) {
    const data = await listBlobsSegmentedWithPrefix(blobSvc, container, list, prefix, token);
    list = list.concat(data.entries);
    if (data.continuationToken) {
        return getList(blobSvc, container, list, prefix, data.continuationToken);
    }
    return list;
}

function syncFiles(blobSvc, container, files) {
    const promises = files.map((item) => {
        return new Promise((resolve, reject) => {
            blobSvc.createBlockBlobFromLocalFile(container, path.basename(item), item, (err) => {
                if (err) {
                    reject(err);
                }
                else {
                     resolve();
                }
            });
        });
    });
    return Promise.all(promises);
}

async function getFileFullname(blobSvc, container, logRequired) {
    if (path.extname(logRequired)) {
        return [logRequired];
    }
    const files = await getList(blobSvc, container, [], '', null);
    if (!files) return [];

    return files.map((file) => {
        if (file.name.includes(logRequired)) return file.name;
        return null;
    });
}

async function downloadFile(blobSvc, container, filename, dest = filename) {
    return new Promise((resolve, reject) => {
        blobSvc.getBlobToStream(container, filename, fs.createWriteStream(dest), (error) => {
            if (error) {
                reject(error.message);
            }
            else resolve();
        });
    });
}

function getHash512FromFile(filename) {
    const hashSum = crypto.createHash('sha512');
    const fileBuffer = fs.readFileSync(filename);
    hashSum.update(fileBuffer);
    return hashSum.digest('hex');
}

function checkFilesHash(manifestFile, dir) {
    manifestFile.forEach((item) => {
        const hashSum = crypto.createHash('sha512');
        const fileBuffer = fs.readFileSync(path.join(dir, item.filename));
        hashSum.update(fileBuffer);
        if (hashSum.digest('hex') !== item.hash) throw new Error(`Hash missmatch, file ${item.filename} hash`);
    });
}

module.exports = {
    syncFiles,
    getFileFullname,
    downloadFile,
    getList,
    getHash512FromFile,
    checkFilesHash
};

'use strict';

const pingus = require('pingus');
const ping = require('ping');
const arpScanner = require('arpscan/promise');
const delay = require('delay');
const fs = require('fs-extra');
const os = require('../utils/os');

const otp57 = 57;
const otp58 = 58;

function isString(x) {
    return Object.prototype.toString.call(x) === '[object String]';
}

function testLogicalValue(value, voltageLevel, logicalValue) {
    if (logicalValue === 0) {
        if (value < voltageLevel * 0.2) return true;
        return false;
    }
    if (logicalValue === 1) {
        if (value > voltageLevel * 0.6) return true;
        return false;
    }

    return false;
}

function removeNoneAscii(str) {
    if ((str === null) || (str === '')) {
        return '';
    }
    const origStr = str.toString();

    return origStr.replace(/[^\x20-\x7E]/g, '');
}

function reverseString(str) {
    const splitMac = str.split('');
    return `${splitMac[10]}${splitMac[11]}:${splitMac[8]}${splitMac[9]}:${splitMac[6]}${splitMac[7]}:${splitMac[4]}${splitMac[5]}:${splitMac[2]}${splitMac[3]}:${splitMac[0]}${splitMac[1]}`;
}

function getWordData(data, address) {
    const lines = data.split('\n');
    const str = lines.find(el => el.includes(`Data${address} `));
    return str.substr(-10);
}

function getCPUSerial(data) {
    const lines = data.split('\n');
    const str = lines.find(el => el.includes('SN          :'));
    if (!str) throw new Error('Cannot parse CPU SN');
    const sn = str.split(':')[1].trim();
    return sn;
}


function isMacTheSame(word57, word58, mac) {
    const hexMac = mac.split(':');
    const address57Word = getWordData(word57, otp57).trim();
    const address58Word = getWordData(word58, otp58).trim();
    const newWord57 = `0x${hexMac[3]}${hexMac[2]}${hexMac[1]}${hexMac[0]}`;
    const newWord58 = `0x0000${hexMac[5]}${hexMac[4]}`;
    if ((address57Word !== newWord57) || (address58Word !== newWord58)) {
        return false;
    }
    return true;
}

function otpToMac(word57, word58) {
    const hexMacReverted = `${word58.substring(6)}${word57.substring(2)}`;
    return reverseString(hexMacReverted);
}

function macToOtp(mac) {
    const hexMac = mac.split(':');
    const oTp57 = parseInt(`${hexMac[3]}${hexMac[2]}${hexMac[1]}${hexMac[0]}`, 16).toString(16);
    const oTp58 = parseInt(`${hexMac[5]}${hexMac[4]}`, 16).toString(16);
    return { oTp57: `0x${oTp57}`, oTp58: `0x${oTp58}` };
}

function getNextMac(MACaddress) {
    const hex = MACaddress.split(':').map(x => parseInt(x, 16));

    function plusOne(p) {
        if (p < 0) return;
        if (hex[p] === 255) { hex[p] = 0; plusOne(p - 1); }
        // eslint-disable-next-line no-plusplus
        else { hex[p]++; }
    }
    plusOne(hex.length - 1);
    const ret = hex.map((x) => {
        if (x < 16) return `0${x.toString(16).slice(-2)}`;
        return `${x.toString(16).slice(-2)}`;
    }).join(':');

    return ret;
}

async function getTargetIp(mac) {
    const ret = await arpScanner({ command: 'arp-scan', interface: 'eth0', sudo: false });
    if (!ret) return null;
    const arpItem = ret.find((element) => {
        if (element.mac.includes(mac.toUpperCase())) {
            return element;
        }
        return null;
    });
    if (!arpItem) return null;
    return arpItem.ip;
}

async function getTargetIpWait(mac, timeout) {
    if (timeout - new Date() / 1000 > 0) {
        const ret = await getTargetIp(mac);
        if (ret) return ret;
        return getTargetIpWait(mac, timeout);
    }

    throw new Error('A: Target is not pingable, down or not flashed?');
}

/**
  * @public
  *
  * @param
  */

async function pingTarget(targetIp) {
    if (targetIp) {
        const host = [targetIp];
        const res = await ping.promise.probe(host, { timeout: 1, extra: ['-c', '1'] });
        if (!res.alive) {
            return null;
        }
        return targetIp;
    }
    throw new Error('B: Target is not pingable, down or not flashed?');
}

async function pingTargetWait(targetIp, timeout /* Sec */) {
    if (timeout - new Date() / 1000 > 0) {
        const ret = await pingTarget(targetIp);
        if (ret) {
            return ret;
        }
        return pingTargetWait(targetIp, timeout);
    }

    throw new Error('C: Target is not pingable, down or not flashed?');
}

/**
  * @public
  *
  * @param
  */

async function waitTargetSshPortUp(targetIp, timeout/* Sec */) {
    if (timeout - new Date() / 1000 > 0) {
        const ret = await pingus.default.tcp({ host: targetIp, port: 22 });
        if (ret && ret.status === 'open') {
            return;
        }
        await delay(100);
        // eslint-disable-next-line consistent-return
        return waitTargetSshPortUp(targetIp, timeout);
    }

    throw new Error('D: Target is not pingable, down or not flashed?');
}

/**
  * @public
  *
  * @param
  */
async function waitTargetDown(targetIp, timeout /* Sec */) {
    if (timeout - new Date() / 1000 > 0) {
        const ret = await pingTarget(targetIp);
        if (!ret) {
            return;
        }
        await delay(100);
        // eslint-disable-next-line consistent-return
        return waitTargetDown(targetIp, timeout);
    }

    throw new Error('Target did not reboot');
}

function boolToInt(value) {
    return value ? 1 : 0;
}

function checkDbRecord(records, full) {
    if (records === undefined || records[0] === undefined) throw new Error('Failed, Invalid DB record');
    const record = records[0];
    if (!record) throw new Error('Failed, Invalid DB record');
    if (!record.vendorSerial) throw new Error('Failed, vendorSerial is not defined.');
    if (!record.ictTestPassed) throw new Error('Failed, Must pass ICT test to program EEPROM');
    if (!record.functionalTestPassed) throw new Error('Failed, Must pass Functional test to program EEPROM');
    // if (!record.flashProgrammed) throw new Error('Failed, Must pass Flash Programming to program EEPROM');
    if (!record.uid) throw new Error('Failed, Must pass MAC Programming to program EEPROM');
    if (full) {
        if (!record.secret) throw new Error('Failed, EEPROM secret status is not defined');
        if (!record.boardS2Serial) throw new Error('Failed, EEPROM programming status is not defined');
        if (!record.dateAndTime) throw new Error('Failed, Missing times stamp in DB');
    }
    return true;
}

function macToUid(mac) {
    return `0000${mac.split(':').join('')}`;
}

async function printLabel(mac, serial, tsId, dbError, logger) {
    const labelPath = '/tmp/label.txt';
    const pngPath = '/tmp/label.png';
    let convertCmd = `convert -size 306x200 xc:white -font "Ubuntu-Mono-Bold" -pointsize 36 -fill black -draw @${labelPath} ${pngPath}`;
    const pritnLabelCmd = `brother_ql print -l 29 ${pngPath}`;

    let labelTxt;

    if (dbError.length) {
        convertCmd = `convert -size 306x${150 + 25 * (dbError.length + 1)} xc:white -font "Ubuntu-Mono-Bold" -pointsize 36 -fill black -draw @${labelPath} ${pngPath}`;
        labelTxt = `text 1,1 "\nFailed\nM1-3200\n${serial}${tsId}\n`;
        dbError.forEach((item) => {
            labelTxt += `${item}\n`;
        });
        labelTxt += '"';
    }
    else {
        labelTxt = `text 1,1 "\nM1-3200\n${serial}${tsId}\n${macToUid(mac)}\n${mac}\n"`;
    }

    await fs.writeFile(labelPath, labelTxt);
    await os.executeShellCommand(convertCmd, logger, false, false);
    try {
        await os.executeShellCommand(pritnLabelCmd, logger, false, false);
    }
    catch (err) {
        throw new Error(`command to print Label failed. Is printer on?: ${pritnLabelCmd}`);
    }
}

module.exports = {
    boolToInt,
    testLogicalValue,
    removeNoneAscii,
    getNextMac,
    macToOtp,
    otpToMac,
    isMacTheSame,
    getWordData,
    otp57,
    otp58,
    pingTargetWait,
    getTargetIpWait,
    checkDbRecord,
    waitTargetSshPortUp,
    waitTargetDown,
    printLabel,
    getCPUSerial,
    macToUid,
    isString
};

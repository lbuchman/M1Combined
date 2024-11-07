'use strict';

const testBoardLink = require('../src/testBoardLink');
const delay = require('delay');
const os = require('../utils/os');
const targetICTLink = require('../src/m1ICTLink');
const m1boot = require('./m1boot');
const buzzer = require('./buzzer');
const sqliteDriver = require('../utils/sqliteDriver');
const path = require('path');
const fs = require('fs-extra');

const progPart = '0x1';
const progopt = '-s 0x1';
const dfuTimeout = 20000;

/** wait till DFU device is available, or timeout in dfuTimeout
  * @public
  *
  * @param
  */
async function waitDFU(programmer, logger) {
    const pullTime = 1000;
    let count = dfuTimeout / pullTime;
    return new Promise(async (resolve, reject) => {
        const timeoutHandle = setInterval(async () => {
            if (count === 0) {
                reject(new Error('timeout waiting for DFU device'));
                clearInterval(timeoutHandle);
                return;
            }
            count -= 1;
            const retValue = await os.executeShellCommand(`${programmer} -l`, logger);
            if ((retValue && retValue.includes('DFU in HS Mode')) || (retValue.includes('DFU in FS Mode'))) {
                resolve(0);
                clearInterval(timeoutHandle);
            }
        }, pullTime);
    });
}

async function getICTFWRev(retries) {
    if (retries > 0) {
        try {
            const ictFwRev = await targetICTLink.sendCommand('getfwrev');
            if (ictFwRev.status) return ictFwRev.fwrev;
        }
        catch (err) {
            await delay(500);
        }
        return getICTFWRev(retries - 1);
    }
    throw new Error('cannot get rev from M1 ict fw');
}

async function programStm(programmer, stm32, m1Dev, logger) {
    logger.debug('Programming UUT ICT FW into SRAM...');
    await waitDFU(programmer, logger);
    await delay(300);
    os.executeShellCommand(`${programmer}  -c port=usb1 -d ${stm32} ${progPart} ${progopt}`, logger, true);
    await delay(1000);
    logger.debug('Programming Done');
    await delay(100); // let M1 start
    await targetICTLink.initSerial(m1Dev, 115200, logger);
    logger.debug('Reading target ICT FW Rev ...');
    const fwRev = await getICTFWRev(4);
    logger.debug(`Target ICT FW Rev - ${fwRev}`);
}

async function initializeTestFixture(programmer, programSTM, stm32, m1Dev, logger, initAndQuit) {
    await testBoardLink.retrieveIoDef();
    testBoardLink.getIoDef();

    const testBoardFwRev = await testBoardLink.sendCommand('getfwrev');
    if (!testBoardFwRev.status) throw new Error('cannot get fw rev from teensy test board');
    logger.debug(`Test Board FW Rev - ${testBoardFwRev.fwrev}`);

    logger.debug('Setting UUT power off');
    await testBoardLink.targetPower(false);
    await testBoardLink.batteryOn(false);
    await testBoardLink.poeOn(false);
    await delay(2000);
    await m1boot.activateDFU();
    await delay(1000);
    await testBoardLink.batteryOn(true);
    await testBoardLink.targetPower(true);
    await delay(2000);

    if (programSTM && programmer) {
        await waitDFU(programmer, logger);
        await delay(3000); // not sure why but prog will fail without delay
        logger.debug('Programming UUT ICT FW into SRAM...');
        os.executeShellCommand(`${programmer}  -c port=usb1 -d ${stm32} ${progPart} ${progopt}`, logger, true);
        await delay(100);
        logger.debug('Programming Done');
        await delay(1000); // let M1 start
        if (m1Dev && !initAndQuit) {
            await targetICTLink.initSerial(m1Dev, 115200, logger);
            const fwRev = await getICTFWRev(4);
            logger.debug(`Target ICT FW Rev - ${fwRev}`);
        }
    }
}

async function testingDoneSuccess() {
    await testBoardLink.retrieveIoDef();
    await buzzer.buzzerBeepSuccess();
}


// eslint-disable-next-line class-methods-use-this
async function testEndSuccess() {
    // await buzzer.buzzerBeepSuccess();
    await testBoardLink.targetPower(false);
    await testBoardLink.batteryOn(false);
}


async function testFailed() {
    try {
        await testBoardLink.targetPower(false);
        await testBoardLink.batteryOn(false);
        await buzzer.buzzerBeepFailed();
    }
    catch (err) {
        //
    }
}

function savelog(serial, logDir, logger) {
    const db = sqliteDriver.initialize(logger);
    const data = fs.readFileSync(path.join(logDir, `${serial}-error.log`));
    db.updatelog(serial, data.toString());
}

async function bootLinux(logger) {
    await testBoardLink.retrieveIoDef();
    testBoardLink.getIoDef();
    logger.debug('Setting UUT power off');
    logger.debug('Setting UUT to programming mode');
    await testBoardLink.targetPower(false);
    await testBoardLink.batteryOn(false);
    delay(200);
    await m1boot.deActivateDFU();
    delay(200);
    await testBoardLink.batteryOn(true);
    await testBoardLink.targetPower(true);
}

module.exports = {
    initializeTestFixture,
    testEndSuccess,
    testFailed,
    bootLinux,
    testingDoneSuccess,
    savelog,
    programStm,
    waitDFU

};

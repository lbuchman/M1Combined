'use strict';

/* eslint-disable no-await-in-loop */
const { SerialPort } = require('serialport');
const utils = require('../utils/utils');
const fs = require('fs-extra');

module.exports = class SerialLink {
    constructor(linkName, linkType, parser) {
        this.busy = false;
        this.parser = parser;
        this.timeoutHandler = null;
        this.linkName = linkName;
        this.linkType = linkType;
    }

    /**
      * @public
      * send command
      * @param {object} log
      */
    async sendCommand(cmd, timeout = 1000) {
        if (this.busy) {
            throw (new Error(`${this.linkName} cmd - ${cmd} faled, serial terminal is busy`));
        }
        if (!this.serialPort) {
            throw (new Error('Serial object is NULL'));
        }
        this.busy = true;
        await this.serialPort.flush();
        this.serialPort.write(`${cmd}\n\r`);
        let timeoutHandle;
        return new Promise(async (resolve, reject) => {
            let jsonRet;
            this.parser.on('data', (data) => {
                this.busy = false;
                clearTimeout(timeoutHandle);
                this.parser.removeAllListeners('data');
                try {
                    jsonRet = JSON.parse(utils.removeNoneAscii(data.toString()));
                    resolve(jsonRet);
                }
                catch (err) {
                    this.parser.removeAllListeners('data');
                    this.logger.debug(err.message);
                    this.logger.debug(data.toString());
                    reject(err);
                }
            });
            timeoutHandle = setTimeout(() => {
                this.busy = false;
                clearTimeout(timeoutHandle);
                this.parser.removeAllListeners('data');
                reject(new Error(`${this.linkType} data in timeout - ${timeout}Msec`));
            }, timeout);
        });
    }

    /**
      * @public
      * send command
      * @param {object} log
      */
    async waitTillReady(timeout) {
        let timeoutHandle;
        return new Promise(async (resolve, reject) => {
            this.parser.on('data', (data) => {
                if (!data.includes('login:')) {
                    return;
                }
                this.parser.removeAllListeners('data');
                resolve();
            });
            timeoutHandle = setTimeout(() => {
                clearTimeout(timeoutHandle);
                this.parser.removeAllListeners('data');
                reject(new Error('Cannot get M1-3200 login promt, the board did not boot?'));
            }, timeout);
        });
    }

    /**
      * @public
      * send command
      * @param {object} log
      */
    async sendData(cmd, extectedData, timeout = 100) {
        this.serialPort.flush();
        this.serialPort.write(cmd);
        if (timeout === 0) return null;
        let timeoutHandle;
        return new Promise(async (resolve, reject) => {
            this.parser.on('data', (data) => {
                if (extectedData) {
                    const cleanData = utils.removeNoneAscii(data);
                    if (!cleanData) return;

                    if (!data.includes(extectedData)) {
                        return;
                    }
                }
                this.busy = false;
                clearTimeout(timeoutHandle);
                this.parser.removeAllListeners('data');
                resolve(data);
            });

            timeoutHandle = setTimeout(() => {
                this.busy = false;
                clearTimeout(timeoutHandle);
                this.parser.removeAllListeners('data');
                reject(new Error('serial data in timeout'));
            }, timeout);
        });
    }

    /**
    * @public
    * init
    * @param {object} log
    */
    async initSerial(devFile, baud, log, dumpdata = false) {
        this.logger = log;
        this.devFile = devFile;
        if (this.serialPort) return;
        this.logger.debug(`Opening ${this.linkName} link serial port: ${devFile}`);
        this.serialPort = new SerialPort({ path: devFile, baudRate: parseInt(baud, 10) });
        this.parser = this.serialPort.pipe(this.parser);

        if (dumpdata) {
            const dumpFile = `${process.env.logDir}/${process.env.SERIAL}_dump.log`;
            fs.writeFileSync(dumpFile, '/n/r');
            this.serialPort.on('data', (data) => {
                fs.writeFileSync(dumpFile, data.toString(), { flag: 'a' });
            });
        }

        await new Promise(async (resolve, reject) => {
            this.serialPort.on('open', () => {
                resolve(0);
            });

            this.serialPort.on('error', async (error) => {
                reject(error);
            });
        });
    }
};

'use strict';

const { SerialPort } = require('serialport');
const delay = require('delay');
const { ReadlineParser } = require('@serialport/parser-readline');
const config = require('../utils/config');


module.exports = class TeensyControl {
    constructor() {
        this.config = config();
        this.busy = false;
        this.timeoutHandler = null;
    }

    /**
      * @public
      * send command
      * @param {object} log
      */
    async sendCommand(cmd, timeout = 1000) {
        if (this.busy) {
            throw (new Error(`cmd - ${cmd} faled, serial terminal is busy`));
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
                    jsonRet = JSON.parse(data.toString());
                }
                catch (err) {
                    this.logger.error(err.message);
                    this.logger.error(data.toString());
                    reject(err);
                }
                resolve(jsonRet);
            });
            timeoutHandle = setTimeout(() => {
                this.busy = false;
                clearTimeout(timeoutHandle);
                this.parser.removeAllListeners('data');
                reject(new Error(`Teensy data in timeout - ${timeout}Msec`));
            }, timeout);
        });
    }

    /**
    * @public
    * init
    * @param {object} log
    */
    async initSerial(log) {
        this.logger = log;
        this.logger.debug(`opening teensy serial port: ${this.config.serialDev}`);
        this.serialPort = new SerialPort({ path: this.config.serialDev, baudRate: parseInt(this.config.serialBaud, 10) });
        this.parser = this.serialPort.pipe(new ReadlineParser({ delimiter: '\n\r' }));
        this.serialPort.on('error', async (error) => {
            log.error(error.message);
            await delay(1000);
            process.exit(-1);
        });
    }
};

'use strict';

const { SerialPort } = require('serialport');
const delay = require('delay');
const { ReadlineParser } = require('@serialport/parser-readline');
const config = require('./config');


module.exports = class TargetSerialTerminal {
    constructor(log) {
        this.config = config();
        this.logger = log;
        this.serialPort = null;
        this.busy = false;
        this.timeoutHandler = null;
    }

    /**
      * @public
      * send command
      * @param {object} log
      */
    async executeCommand(cmd, isLogin = false, timeout = 1000) {
        if (this.busy) {
            throw (new Error(`cmd - ${cmd} faled, serial terminal is busy`));
        }

        let parser;

        if (isLogin) {
            parser = this.parserLogin;
        }
        else {
            parser = this.parser;
        }

        this.busy = true;
        await this.serialPort.flush();
        this.serialPort.write(`${cmd}\r\n`);
        let timeoutHandle;
        return new Promise(async (resolve, reject) => {
            parser.on('data', (data) => {
                this.busy = false;
                clearTimeout(timeoutHandle);
                parser.removeAllListeners('data');
                resolve(data);
            });
            timeoutHandle = setTimeout(() => {
                this.busy = false;
                clearTimeout(timeoutHandle);
                parser.removeAllListeners('data');
                reject(new Error(`Teensy data in timeout - ${timeout}Msec`));
            }, timeout);
        });
    }

    /**
    * @public
    * init
    * @param {object} log
    */
    initSerial() {
        this.logger.debug(`opening terminal serial port: ${this.config.targetTerminalDev}`);
        this.serialPort = new SerialPort({ path: this.config.targetTerminalDev, baudRate: parseInt(this.config.terminalBaud, 10) });
        this.parser = this.serialPort.pipe(new ReadlineParser({ delimiter: this.config.shDelimiter }));
        this.parserLogin = this.serialPort.pipe(new ReadlineParser({ delimiter: 'Password:' }));

        this.serialPort.on('error', async (error) => {
            this.logger.error(error.message);
            await delay(10);
            process.exit(-1);
        });
    }
};

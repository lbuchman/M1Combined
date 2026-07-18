'use strict';

/* eslint-disable no-await-in-loop */
const { ReadlineParser } = require('@serialport/parser-readline');
const delay = require('delay');
const SerialLink = require('./serialLink');
const runtimeContext = require('../utils/runtimeContext');

module.exports = class M1TermLink {
    constructor(logger) {
        this.logger = logger;
        this.timeoutHandler = null;
        this.parser = new ReadlineParser({ delimiter: '\r' });
        const productName = require('../utils/runtimeContext').getRuntime().productName;
        this.serialLink = new SerialLink(`${productName} Term`, 'm1Trm', this.parser);
    }

    /**
     * @public
     *
     * @param {object} log
     */
    async initSerial(devFile, baud, log, dump) {
        await this.serialLink.initSerial(devFile, baud, log, dump);
    }

    /**
     * @public
     * wait login prompt
     * @param {object} log
     */
    async waitLoginPrompt(timeout) {
        if (timeout - new Date() / 1000 > 0) {
            try {
                await this.serialLink.waitTillReady(10000);
                return null;
            } catch (err) {
                this.serialLink.sendData('\n\r', null, 0);
            }
            return this.waitLoginPrompt(timeout);
        }

        const productName = require('../utils/runtimeContext').getRuntime().productName;
        throw new Error(`No login promt, did ${productName} boot?`);
    }

    /**
     * @public
     * wait login prompt
     * @param {object} log
     */
    async logInToTerminal(login, password) {
        await this.serialLink.sendData(`${login}\n\r`, login, 0);
        await delay(500);
        await this.serialLink.sendData(`${password}\n\r`, null, 0);
        await delay(500);
    }

    /**
     * @public
     * wait login prompt
     * @param {object} log
     */
    async initTestMode() {
        const runtime = runtimeContext.getRuntime();
        await this.executeCommand('\n\r', 0);
        await delay(500);
        await this.executeCommand(`ifconfig eth0 ${runtime.m1defaultIP} up`, 0);
        await delay(1000);
        await this.executeCommand('/etc/init.d/sshd stop', 0);
        await delay(1000);
        await this.executeCommand(
            '/usr/sbin/sshd -h /var/run/ssh/ssh_host_rsa_key -o MaxSessions=10 -o UsePAM=no -o MaxAuthTries=10',
            0
        );
        await delay(1000);
    }

    /**
     * @public
     * send command
     * @param {object} log
     */
    async executeCommand(cmd, timeout = 1000) {
        return this.serialLink.sendData(`${cmd}\n\r`, null, timeout);
    }
};

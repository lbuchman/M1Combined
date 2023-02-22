'use strict';

/* eslint-disable no-await-in-loop */
const { ReadlineParser } = require('@serialport/parser-readline');
const SerialLink = require('../src/serialLink');

const groupdDefinition = {
    NotDefinedGroup: 0,
    RibbonCableGroupStatic: 1,
    RibbonCableGroupDynamic: 2,
    PowerGroup: 3,
    TestpointsGroup: 4
};

class TestBoardLink {
    constructor() {
        this.busy = false;
        this.timeoutHandler = null;
        this.linkType = 'teensy4.1';
        this.serialLink = new SerialLink('TestBoard', this.linkType, new ReadlineParser({ delimiter: '\n\r' }));
    }

    /**
      * @public
      */
    // eslint-disable-next-line class-methods-use-this
    getGroupdDefinition() {
        return groupdDefinition;
    }

    /**
      * @public
      * send command
      * @param {object} log
      */
    async sendCommand(cmd, timeout = 1000) {
        return this.serialLink.sendCommand(cmd, timeout);
    }

    /**
    * @public
    * init
    * @param {object} log
    */
    async initSerial(devFile, baud, log) {
        this.logger = log;
        await this.serialLink.initSerial(devFile, baud, log);
    }

    /**
 * @public
 * init
 * @param {object} log
 */
    isPortOpen() {
        return this.serialPort.isPortOpen();
    }

    /**
    * @public
    * read io def from the test board
    * @param {object} log
    */
    async retrieveIoDef() {
        this.logger.debug('Reading Io Def from the Test Board...');
        this.ioDef = await this.sendCommand('iodef');
    }

    /**
    * @public
    * get io def from the test board
    * @param {object} log
    */
    getIoDef() {
        return this.ioDef;
    }

    /**
   * @public
   *
   * @param
   */
    findPinIdByName(pinName) {
        const pin = this.ioDef.find(o => o.pinName === pinName);
        if (pin) return pin.pinId;
        throw new Error(`findPinIdByName() invalid pin Name - ${pinName}`);
    }

    /**
     * @public
     *
     * @param {integer} 1 is on, 0 is off
    */
    // eslint-disable-next-line class-methods-use-this
    async targetPower(onoff) {
        if (onoff) {
            await this.sendCommand('targetpoweron');
        }
        else {
            await this.sendCommand('targetpoweroff');
        }
    }

    /**
     * @public
     *
     * @param {integer} 1 is on, 0 is off
    */
    // eslint-disable-next-line class-methods-use-this
    async batteryOn(onoff) {
        if (onoff) {
            await this.sendCommand('batteryon');
        }
        else {
            await this.sendCommand('batteryoff');
        }
    }

    /**
     * @public
     *
     * @param {integer} 1 is on, 0 is off
    */
    // eslint-disable-next-line class-methods-use-this
    async batteryLoadOn(onoff) {
        if (onoff) {
            await this.sendCommand('batteryloadon');
        }
        else {
            await this.sendCommand('batteryloadoff');
        }
    }
}

module.exports = new TestBoardLink();

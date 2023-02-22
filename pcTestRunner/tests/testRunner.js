'use strict';

// https://www.rebasedata.com/convert-sqlite-to-excel-online
const path = require('path');
const Teensy = require('../teensy/teensy');
const delay = require('delay');
const config = require('../utils/config');
const Report = require('../utils/report');
const TargetTerminal = require('../utils/targetTerminal');

const groups = {
    ribbonCableGroup1: 1,
    ribbonCableGroup2: 2,
    testPointsGroup: 2
};


const ledState = {
    ledOff: 0,
    ledOn: 1,
    ledBlink: 2
};

const switches = {
    switchAutoPressed: 1,
    switchTestPressed: 2,
    switchProgramPressed: 8,
    switchPrintLabelPressed: 4
};

module.exports = class TestRunner {
    constructor(log) {
        this.config = config();
        this.logger = log;
        this.busy = false;
    }

    /**
      * @public
      *
      * @param {integer} 1 is on, 0 is off
      */
    // eslint-disable-next-line class-methods-use-this
    async targetPower(onoff) {
        if (onoff) {
            await this.teensy.sendCommand('targetpoweron');
            const power12VState = await this.checkPower();
            if (power12VState) this.logger.debug('Target power is enabled');
            else throw new Error('Failed to enable target power, abborting');
        }
        else {
            await this.teensy.sendCommand('targetpoweroff');
            // const power12VState = await this.checkPower();
            // if (!power12VState) this.logger.debug('Target power is disabled');
            // else throw new Error('Failed to disable target power, abborting');
        }
    }

    /**
      * @public
      *
      * @param {object} log
      */
    async init(log) {
        const switchedPullInterval = 300;
        this.logger = log;
        this.teensy = new Teensy();
        this.teensy.initSerial(log);
        this.targetTerminal = new TargetTerminal(this.logger);
        this.targetTerminal.init(this.logger);
        this.busy = false;
        let switchesState = 0;
        delay(1000);

        this.switchesMonitor = setInterval(async () => {
            if (this.busy) return;

            try {
                const switchesValue = await this.teensy.sendCommand('getswiches');
                if (switchesValue.value === switchesState) {
                    return;
                }

                switchesState = switchesValue.value;

                if (switchesValue.value) {
                    this.busy = true;
                    this.logger.debug('\n*****************************************************************************');
                }
                await this.targetPower(false);
                this.logger.debug('Target power is disabled');
                switch (switchesValue.value) {
                    case switches.switchAutoPressed: break;
                    case switches.switchPrintLabelPressed: break;
                    case switches.switchProgramPressed: break;
                    case switches.switchTestPressed: {
                        await this.switchTestPressed();
                        break;
                    }
                    default:
                        break;
                }
                this.busy = false;
            }
            catch (err) {
                // if (err.message === 'cmd - getswiches,  busy') return;
                this.logger.error(err.message);
                this.busy = false;
            }
        }, switchedPullInterval);
    }

    /**
      * @public
      *
      * @param
      */
    // eslint-disable-next-line class-methods-use-this
    async testRibbonCableGroup1() {
        const ribbonCableTests = await this.teensy.sendCommand(`testtestpoints ${groups.ribbonCableGroup1}`, 2000);
        const report = new Report(path.join(this.config.logdir, 'testreport.log'), this.logger);
        const result = report.reportRibbonCableGroup1TestResults(ribbonCableTests);
        if (!result) {
            await this.teensy.sendCommand('buzzerbeep 1');
            await this.ledControl(this.findPinIdByName('LED_Testing'), ledState.ledOn);
            await this.ledControl(this.findPinIdByName('LED_Failed'), ledState.ledOn);
            throw new Error('Ribbon cable group1 test failed');
        }
    }


    /**
      * @public
      *
      * @param
      */
    // eslint-disable-next-line class-methods-use-this
    async testTestPointsGroup() {
        const testPointsTests = await this.teensy.sendCommand(`testtestpoints ${groups.testPointsGroup}`, 2000);
        const report = new Report(path.join(this.config.logdir, 'testreport.log'), this.logger);
        const result = report.reportRibbonCableGroup1TestResults(testPointsTests);
        if (!result) {
            await this.teensy.sendCommand('buzzerbeep 1');
            await this.ledControl(this.findPinIdByName('LED_Testing'), ledState.ledOn);
            await this.ledControl(this.findPinIdByName('LED_Failed'), ledState.ledOn);
            throw new Error('Ribbon cable group1 test failed');
        }
    }


    /**
      * @public
      *
      * @param
      */
    // eslint-disable-next-line class-methods-use-this
    async testBattery() {
        const cmd = 'testBattery';
        const ret = await this.teensy.sendCommand(cmd, 2000);
        if (!ret.status) {
            await this.teensy.sendCommand('buzzerbeep 1');
            await this.ledControl(this.findPinIdByName('LED_Testing'), ledState.ledOn);
            await this.ledControl(this.findPinIdByName('LED_Failed'), ledState.ledOn);
            throw new Error(`battery test is failed, error command ${cmd}: ${ret.error}`);
        }
    }

    /**
      * @public
      *
      * @param
      */
    async checkPower() {
        const ret = await this.teensy.sendCommand(`testtestpoint ${this.findPinIdByName('base12VAD')}`, 1000);
        return ret.valueValid;
    }

    /**
      * @public
      *
      * @param
      */
    findPinIdByName(pinName) {
        const pin = this.teensyIo.find(o => o.pinName === pinName);
        return pin.pinId;
    }


    /**
      * @public
      *
      * @param
      */
    async ledControl(led, state) {
        await this.teensy.sendCommand(`ledcontrol ${led} ${state}`);
    }

    /**
      * @public
      *
      * @param
      */
    async allLedsOff() {
        await this.ledControl(this.findPinIdByName('LED_Ready'), ledState.ledOff);
        await this.ledControl(this.findPinIdByName('LED_Prog'), ledState.ledOff);
        await this.ledControl(this.findPinIdByName('LED_Testing'), ledState.ledOff);
        await this.ledControl(this.findPinIdByName('LED_Failed'), ledState.ledOff);
    }

    /**
      * @public
      *
      * @param
      */
    async switchTestPressed() {
        // initialize
        this.teensyIo = await this.teensy.sendCommand('iodef');
        await this.targetPower(true);
        await delay(1000); // delay ????, how long, shall I pull serial terminal
        await this.allLedsOff();
        await this.ledControl(this.findPinIdByName('LED_Testing'), ledState.ledBlink);

        // run tests, throw on error
        await this.testRibbonCableGroup1();
        await this.testBattery();
       // await this.testTestPointsGroup();

        // cleanup
        await this.targetPower(false);
        await this.ledControl(this.findPinIdByName('LED_Ready'), ledState.ledOn);
        await this.ledControl(this.findPinIdByName('LED_Testing'), ledState.ledOn);
        await this.teensy.sendCommand('buzzerbeep 0');
        this.busy = false;
    }
};

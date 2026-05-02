'use strict';

const config = require('../utils/config');

module.exports = class CalibrationData {
    constructor(boardId, logger) {
        this.logger = logger;
        this.boardId = parseInt(boardId, 10);
        this.maxM1TestBoardSupported = 10;

        this.ddrVoltageM1 = { name: 'TP31', voltage: 1.35 };
        this.ddrVoltageMnp = { name: 'TP304', voltage: 1.35 };

        this.coinCellBattery = {
            name: 'BatCellBat',
            minVoltageNew: 3.1,
            minVoltageAged: 2.9,
            scale: 1
        };

        this.strikeReg = [
            { name: 'SW1601.6', funcName: 'STRIKE1_KICKER_EN', functnameAux: 'STRIKE1_KICKER_POWER', voltage: 28.0, scale: 5.6 },
            { name: 'SW1602.6', funcName: 'STRIKE2_KICKER_EN', functnameAux: 'STRIKE2_KICKER_POWER', voltage: 28.0, scale: 5.6 }
        ];

        this.ribbonCableA2DPins = [
            { name: 'TP1801', voltage: 2.70, scale: 1.0325 },
            { name: 'TP1802', voltage: 2.70, scale: 1.0125 },
            { name: 'TP1901', voltage: 2.70, scale: 1.0125 },
            { name: 'TP1902', voltage: 2.70, scale: 1.0125 }
        ];


        this.testPointsM1 = [
            { name: 'TP025', voltage: 5, scale: 1 },
            { name: 'TP33', voltage: 2.8, scale: 1 },
            { name: 'TP35', voltage: 3.3, scale: 1 },
            { name: 'TP34', voltage: 3.3, scale: 1 },
            { name: 'TP36', voltage: 1.2, scale: 1 },
            { name: 'J5.13', voltage: 11.7, scale: 1 },
            { name: 'J5.5', voltage: 6.0, scale: 1 },
            { name: 'J5.7', voltage: 6.0, scale: 1 },
            { name: 'J5.8', voltage: 6.0, scale: 1 }
        ];

        this.testPointsMnp = [
            { name: 'TP204', voltage: 5.0, scale: 1 },
            { name: 'TP308', voltage: 2.8, scale: 1 },
            { name: 'TP303', voltage: 1.2, scale: 1 },
            { name: 'TP305', voltage: 3.3, scale: 1 },
            { name: 'TP306', voltage: 3.3, scale: 1 },
            { name: 'TP401', voltage: 5.0, scale: 0.98 },
            { name: 'TP2301', voltage: 12.8, scale: 1 },
            { name: 'TP202', voltage: 12.0, scale: 0.995 },
            { name: 'J2101.1', voltage: 11.85, scale: 2.79 },
            { name: 'J2001.1', voltage: 11.85, scale: 2.79 }
        ];

        this.defaults = {
            testPointsMnp: this.testPointsMnp,
            testPointsM1: this.testPointsM1,
            ribbonCableA2DPins: this.ribbonCableA2DPins,
            strikeReg: this.strikeReg,
            ddrVoltageM1: this.ddrVoltageM1,
            ddrVoltageMnp: this.ddrVoltageMnp,
            coinCellBattery: this.coinCellBattery
        };
    }

    setBoardId(boardId) {
        this.boardId = boardId;
    }


    async intitConfigFile() {
        this.config = await config.getConfig({});
        if (this.boardId === 255) throw new Error('Invalid board ID, update M1 testboard FW and program an ID');
        if (!this.config.boards) {
            const defData = new Array(20).fill(this.defaults);
            this.config.boards = defData;
        }
        else {
            this.testPointsMnp = this.config.boards[this.boardId].testPointsMnp;
            this.testPointsM1 = this.config.boards[this.boardId].testPointsM1;
            this.ribbonCableA2DPins = this.config.boards[this.boardId].ribbonCableA2DPins;
            this.strikeReg = this.config.boards[this.boardId].strikeReg;
            this.ddrVoltageM1 = this.config.boards[this.boardId].ddrVoltageM1;
            this.ddrVoltageMnp = this.config.boards[this.boardId].ddrVoltageMnp;
            this.coinCellBattery = this.config.boards[this.boardId].coinCellBattery;
        }
    }

    getTestPointsMnp() {
        return this.testPointsMnp;
    }

    getTestPointsM1() {
        return this.testPointsM1;
    }

    getRibbonCableA2DPins() {
        return this.ribbonCableA2DPins;
    }

    getStrikeReg() {
        return this.strikeReg;
    }


    setTestPointsMnp(value) {
        this.testPointsMnp = value;
    }

    setTestPointsM1(value) {
        this.testPointsM1 = value;
    }

    setRibbonCableA2DPins(value) {
        this.ribbonCableA2DPins = value;
    }

    setStrikeReg(value) {
        this.strikeReg = value;
    }


    async saveConfigFile() {
        this.config.boards[this.boardId] = this.defaults;
        await config.saveConfig(this.config);
    }
};

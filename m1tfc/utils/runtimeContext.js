'use strict';

const state = {
    dbPath: null,
    serial: null,
    logDir: null,
    m1defaultIP: '192.168.0.251',
    productName: 'm1-3200',
    debugLevel: '0',
    cellBatTol: null,
    cellBatVoltage: null,
    skipBatteryTest: false,
    coinCellDebug: false,
    fwDir: null
};

function setRuntime(values = {}) {
    Object.assign(state, values);
}

function getRuntime() {
    return state;
}

module.exports = {
    setRuntime,
    getRuntime
};

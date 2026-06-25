'use strict';

const runtimeContext = require('../../utils/runtimeContext');

describe('runtimeContext', () => {
    beforeEach(() => {
        runtimeContext.setRuntime({
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
        });
    });

    test('getRuntime returns default state', () => {
        const state = runtimeContext.getRuntime();
        expect(state.m1defaultIP).toBe('192.168.0.251');
        expect(state.productName).toBe('m1-3200');
        expect(state.skipBatteryTest).toBe(false);
    });

    test('setRuntime merges values into state', () => {
        runtimeContext.setRuntime({ serial: 'ABC123', debugLevel: '2' });
        const state = runtimeContext.getRuntime();
        expect(state.serial).toBe('ABC123');
        expect(state.debugLevel).toBe('2');
        expect(state.productName).toBe('m1-3200'); // unchanged
    });

    test('setRuntime overwrites existing values', () => {
        runtimeContext.setRuntime({ m1defaultIP: '10.0.0.1' });
        const state = runtimeContext.getRuntime();
        expect(state.m1defaultIP).toBe('10.0.0.1');
    });
});

'use strict';

const exitCodes = require('../../src/exitCodes');

describe('exitCodes', () => {
    test('normalExit is 0', () => {
        expect(exitCodes.normalExit).toBe(0);
    });

    test('commandFailed is 3', () => {
        expect(exitCodes.commandFailed).toBe(3);
    });

    test('all codes are unique integers', () => {
        const values = Object.values(exitCodes);
        const unique = new Set(values);
        expect(unique.size).toBe(values.length);
        values.forEach(v => expect(typeof v).toBe('number'));
    });

    test('all expected keys are present', () => {
        const expected = [
            'normalExit',
            'configFileMissing',
            'macMissing',
            'commandFailed',
            'ictTestFailed',
            'testPointTestFailed',
            'programEepromFailed',
            'programMacFailed',
            'tamperSensorTestFailed',
            'configVendorSiteMissing',
            'otpIsNotBlank',
            'eepromIsNotBlank',
            'notAllTestsPassed',
            'functTestFailed',
            'invalidArgument',
            'precheckHWFailed',
            'poeTestFailed'
        ];
        expected.forEach(key => expect(exitCodes).toHaveProperty(key));
    });
});

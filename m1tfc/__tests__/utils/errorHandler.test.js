'use strict';

const { wrapAction } = require('../../utils/errorHandler');
const exitCodes = require('../../src/exitCodes');

describe('errorHandler.wrapAction', () => {
    let mockLog;

    beforeEach(() => {
        mockLog = { error: jest.fn(), debug: jest.fn() };
        jest.spyOn(process, 'exit').mockImplementation(() => {});
    });

    afterEach(() => {
        jest.restoreAllMocks();
    });

    test('calls the wrapped function with provided arguments', async() => {
        const fn = jest.fn().mockResolvedValue('ok');
        const wrapped = wrapAction(fn, mockLog);
        await wrapped('arg1', 'arg2');
        expect(fn).toHaveBeenCalledWith('arg1', 'arg2');
    });

    test('does not call process.exit on success', async() => {
        const fn = jest.fn().mockResolvedValue('ok');
        const wrapped = wrapAction(fn, mockLog);
        await wrapped();
        expect(process.exit).not.toHaveBeenCalled();
    });

    test('logs error and exits on failure', async() => {
        const fn = jest.fn().mockRejectedValue(new Error('something went wrong'));
        const wrapped = wrapAction(fn, mockLog);
        await wrapped();
        expect(mockLog.error).toHaveBeenCalledWith(expect.stringContaining('something went wrong'));
        expect(process.exit).toHaveBeenCalledWith(exitCodes.commandFailed);
    });
});

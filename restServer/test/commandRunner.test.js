'use strict';

const test = require('node:test');
const assert = require('node:assert/strict');
const { EventEmitter } = require('node:events');

const { CommandRunner, getSupportedCommands } = require('../src/commandRunner');

function createMockChild() {
    const child = new EventEmitter();
    child.stdout = new EventEmitter();
    child.stderr = new EventEmitter();
    return child;
}

function nextTick() {
    return new Promise(resolve => setImmediate(resolve));
}

test('getSupportedCommands exposes the CLI surface', () => {
    const commands = getSupportedCommands();

    assert.ok(commands.includes('m1cmd'));
    assert.ok(commands.includes('m1tbcmd'));
    assert.ok(!commands.includes('tbcmd'));
});

test('run rejects unsupported commands', async () => {
    const runner = new CommandRunner({
        spawnImpl: () => {
            throw new Error('spawn should not be called');
        }
    });

    const result = await runner.run('does-not-exist');

    assert.equal(result.status, 'FAILED');
    assert.equal(result.errorCode, 14);
    assert.equal(result.ErrorDescription, 'Unsupported command "does-not-exist"');
    assert.equal(result.commandOutput, null);
});

test('run forwards arguments and merges JSON stdout into the response', async () => {
    const captured = {};
    const child = createMockChild();

    const runner = new CommandRunner({
        baseCommand: 'node',
        baseArgs: ['-e', 'mock'],
        cwd: 'C:/temp',
        env: { TEST_ENV: '1' },
        spawnImpl: (command, args, options) => {
            captured.command = command;
            captured.args = args;
            captured.options = options;
            return child;
        }
    });

    const resultPromise = runner.run('m1cmd', { c: 'hello world', positional: ['alpha', 'beta'], v: true });
    await nextTick();
    child.stdout.emit('data', Buffer.from('noise line\n'));
    child.stdout.emit('data', Buffer.from(JSON.stringify({ status: 'OK', errorCode: 0, ErrorDescription: 'Success', echoedArgs: ['m1cmd', 'alpha', 'beta', '-c', 'hello world', '-v'] })));
    child.emit('close', 0);

    const result = await resultPromise;

    assert.equal(captured.command, 'node');
    assert.deepEqual(captured.args, ['-e', 'mock', 'm1cmd', 'alpha', 'beta', '-c', 'hello world', '-v']);
    assert.deepEqual(captured.options, {
        cwd: 'C:/temp',
        env: { TEST_ENV: '1' },
        shell: false
    });
    assert.equal(result.status, 'OK');
    assert.equal(result.errorCode, 0);
    assert.equal(result.ErrorDescription, 'Success');
    assert.deepEqual(result.echoedArgs, ['m1cmd', 'alpha', 'beta', '-c', 'hello world', '-v']);
    assert.deepEqual(result.commandOutput, {
        status: 'OK',
        errorCode: 0,
        ErrorDescription: 'Success',
        echoedArgs: ['m1cmd', 'alpha', 'beta', '-c', 'hello world', '-v']
    });
});

test('run falls back to stderr text when the exit code is unknown', async () => {
    const child = createMockChild();

    const runner = new CommandRunner({
        spawnImpl: () => child
    });

    const resultPromise = runner.run('m1cmd');
    await nextTick();
    child.stderr.emit('data', Buffer.from('something failed\n'));
    child.emit('close', 99);

    const result = await resultPromise;

    assert.equal(result.status, 'FAILED');
    assert.equal(result.errorCode, 99);
    assert.equal(result.ErrorDescription, 'something failed');
    assert.equal(result.commandOutput, null);
});
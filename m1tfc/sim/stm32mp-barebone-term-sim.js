#!/usr/bin/env node

'use strict';

/* eslint-disable no-console */

const { SerialPort } = require('serialport');
const { ReadlineParser } = require('@serialport/parser-readline');

function getArg(name, fallback = null) {
    const idx = process.argv.indexOf(name);
    if (idx < 0 || idx + 1 >= process.argv.length) {
        return fallback;
    }
    return process.argv[idx + 1];
}

const portPath = getArg('--port');
const baudRate = Number(getArg('--baud', '115200'));

if (!portPath) {
    console.error('Usage: node sim/stm32mp-barebone-term-sim.js --port <tty> [--baud 115200]');
    process.exit(2);
}

let linuxBooted = false;
let loggedIn = false;

const port = new SerialPort({ path: portPath, baudRate });
const parser = port.pipe(new ReadlineParser({ delimiter: '\r' }));

function writeLine(line) {
    port.write(`${line}\r`);
}

function writePrompt() {
    if (!linuxBooted) {
        writeLine('U-Boot>');
        return;
    }
    if (!loggedIn) {
        writeLine('login:');
        return;
    }
    writeLine('#');
}

function handleBareboneCommand(cmd) {
    if (cmd === 'help') {
        writeLine('boot reset printenv setenv saveenv');
        writePrompt();
        return;
    }
    if (cmd.startsWith('boot') || cmd === 'run bootcmd') {
        linuxBooted = true;
        loggedIn = false;
        writeLine('Starting kernel ...');
        setTimeout(() => writeLine('login:'), 100);
        return;
    }
    if (cmd === 'reset') {
        linuxBooted = false;
        loggedIn = false;
        writeLine('resetting ...');
        setTimeout(() => writePrompt(), 100);
        return;
    }

    writeLine(`OK: ${cmd}`);
    writePrompt();
}

function handleLinuxCommand(cmd) {
    if (!loggedIn) {
        if (cmd === 'root') {
            writeLine('Password:');
            return;
        }
        if (cmd.length > 0) {
            loggedIn = true;
            writeLine('Welcome to STM32MP simulator');
            writePrompt();
            return;
        }
        writePrompt();
        return;
    }

    if (cmd === 'halt') {
        linuxBooted = false;
        loggedIn = false;
        writeLine('System halted.');
        writePrompt();
        return;
    }

    if (cmd.includes('ifconfig eth0')) {
        writeLine('eth0 configured');
        writePrompt();
        return;
    }

    if (cmd.includes('sshd')) {
        writeLine('sshd ok');
        writePrompt();
        return;
    }

    writeLine(`cmd: ${cmd}`);
    writePrompt();
}

port.on('open', () => {
    writeLine('STM32MP barebone serial simulator ready');
    writePrompt();
});

parser.on('data', raw => {
    const cmd = String(raw || '').trim();
    if (!cmd) {
        writePrompt();
        return;
    }

    if (!linuxBooted) {
        handleBareboneCommand(cmd);
        return;
    }

    handleLinuxCommand(cmd);
});

port.on('error', err => {
    console.error(err.message);
    process.exit(1);
});

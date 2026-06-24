'use strict';

const express = require('express');
const crypto = require('crypto');
const fs = require('fs');
const path = require('path');
const { CommandRunner, getSupportedCommands } = require('./commandRunner');

const port = Number(process.env.PORT || 3300);
const host = process.env.HOST || '0.0.0.0';

// PIN storage — use SNAP_DATA if available, else same dir as server.js
const pinDir  = process.env.SNAP_DATA || path.dirname(__filename);
const pinFile = path.join(pinDir, 'pin.json');
const PIN_ITERATIONS = 100000;
const PIN_KEYLEN     = 64;
const PIN_DIGEST     = 'sha512';

function hashPin(pin, salt) {
    return crypto.pbkdf2Sync(pin, salt, PIN_ITERATIONS, PIN_KEYLEN, PIN_DIGEST).toString('hex');
}

function loadPin() {
    try { return JSON.parse(fs.readFileSync(pinFile, 'utf8')); } catch { return null; }
}

function savePin(pin) {
    const salt = crypto.randomBytes(32).toString('hex');
    fs.writeFileSync(pinFile, JSON.stringify({ hash: hashPin(pin, salt), salt }), 'utf8');
}

function verifyPin(pin) {
    const stored = loadPin();
    if (!stored) return pin === '1234'; // default if no pin file yet
    return hashPin(pin, stored.salt) === stored.hash;
}

const commandRunner = new CommandRunner({
    baseCommand: defaultCliPath,
    baseArgs: defaultCliArgs,
    cwd: cliCwd,
    env: process.env
});

const app = express();
app.use(express.json({ limit: '1mb' }));

app.get('/health', (req, res) => {
    res.json({
        status: 'OK',
        errorCode: 0,
        ErrorDescription: 'Server is running'
    });
});

app.get('/config', (req, res) => {
    res.json({
        status: 'OK',
        machineName: process.env.MACHINE_NAME || 'FC?'
    });
});

app.get('/commands', (req, res) => {
    res.json({
        status: 'OK',
        errorCode: 0,
        ErrorDescription: 'Supported command list',
        commands: getSupportedCommands()
    });
});

app.post('/auth', (req, res) => {
    const { pin } = req.body || {};
    if (!pin || typeof pin !== 'string' || !/^\d{4,6}$/.test(pin)) {
        return res.status(400).json({ status: 'FAILED', ErrorDescription: 'Invalid PIN format' });
    }
    if (verifyPin(pin)) return res.json({ status: 'OK' });
    res.status(401).json({ status: 'FAILED', ErrorDescription: 'Wrong PIN' });
});

app.post('/changepin', (req, res) => {
    const { pin } = req.body || {};
    if (!pin || typeof pin !== 'string' || !/^\d{4,6}$/.test(pin)) {
        return res.status(400).json({ status: 'FAILED', ErrorDescription: 'Invalid PIN format' });
    }
    savePin(pin);
    res.json({ status: 'OK' });
});

app.post('/command', async (req, res) => {
    const { command, argument } = req.body || {};

    if (!command || typeof command !== 'string') {
        res.status(400).json({
            status: 'FAILED',
            errorCode: 14,
            ErrorDescription: 'Field "command" must be a non-empty string'
        });
        return;
    }

    const result = await commandRunner.run(command, argument);
    res.status(result.status === 'OK' ? 200 : 500).json(result);
});

app.use((err, req, res, next) => {
    res.status(500).json({
        status: 'FAILED',
        errorCode: 3,
        ErrorDescription: err && err.message ? err.message : 'Internal server error'
    });
});

app.listen(port, host, () => {
    console.log(`m1tfc REST server listening on http://${host}:${port}`);
    console.log(`CLI command: ${defaultCliPath} ${defaultCliArgs.join(' ')}`.trim());
    console.log(`CLI cwd: ${cliCwd}`);
});

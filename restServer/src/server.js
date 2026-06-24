'use strict';

const express = require('express');
const crypto = require('crypto');
const fs = require('fs');
const path = require('path');
const { CommandRunner, getSupportedCommands } = require('./commandRunner');

const port = Number(process.env.PORT || 3300);
const host = process.env.HOST || '0.0.0.0';
const defaultCliPath = process.env.M1TFC_CMD || 'm1tfc';
const defaultCliArgs = process.env.M1TFC_BASE_ARGS
    ? process.env.M1TFC_BASE_ARGS.split(' ').filter(Boolean)
    : [];
const cliCwd = process.env.M1TFC_CWD || process.cwd();

// PIN storage — use SNAP_DATA if available, else same dir as server.js
const pinDir  = process.env.SNAP_DATA || path.dirname(__filename);
const pinFile = path.join(pinDir, 'pin.json');
const PIN_ITERATIONS = 100000;
const PIN_KEYLEN     = 64;
const PIN_DIGEST     = 'sha512';
const VALID_PIN_MODES = new Set(['production', 'debug']);

function hashPin(pin, salt) {
    return crypto.pbkdf2Sync(pin, salt, PIN_ITERATIONS, PIN_KEYLEN, PIN_DIGEST).toString('hex');
}

function loadPinData() {
    try {
        const parsed = JSON.parse(fs.readFileSync(pinFile, 'utf8'));
        // Backward compatibility with old single-pin format: { hash, salt }
        if (parsed && parsed.hash && parsed.salt) {
            return {
                production: { hash: parsed.hash, salt: parsed.salt },
                debug: { hash: parsed.hash, salt: parsed.salt }
            };
        }
        return parsed;
    } catch {
        return null;
    }
}

function savePinData(pinData) {
    fs.mkdirSync(pinDir, { recursive: true });
    fs.writeFileSync(pinFile, JSON.stringify(pinData), 'utf8');
}

function savePin(mode, pin) {
    const current = loadPinData() || {};
    const salt = crypto.randomBytes(32).toString('hex');
    current[mode] = { hash: hashPin(pin, salt), salt };
    savePinData(current);
}

function verifyPin(mode, pin) {
    const stored = loadPinData();
    if (!stored || !stored[mode]) {
        // defaults when not provisioned yet
        const defaultPin = mode === 'production' ? '1223' : '4321';
        return pin === defaultPin;
    }
    return hashPin(pin, stored[mode].salt) === stored[mode].hash;
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
    const { pin, mode } = req.body || {};
    if (!mode || typeof mode !== 'string' || !VALID_PIN_MODES.has(mode)) {
        return res.status(400).json({ status: 'FAILED', ErrorDescription: 'Invalid PIN mode' });
    }
    if (!pin || typeof pin !== 'string' || !/^\d{4,6}$/.test(pin)) {
        return res.status(400).json({ status: 'FAILED', ErrorDescription: 'Invalid PIN format' });
    }
    if (verifyPin(mode, pin)) return res.json({ status: 'OK' });
    res.status(401).json({ status: 'FAILED', ErrorDescription: 'Wrong PIN' });
});

app.post('/changepin', (req, res) => {
    const { pin, mode } = req.body || {};
    if (!mode || typeof mode !== 'string' || !VALID_PIN_MODES.has(mode)) {
        return res.status(400).json({ status: 'FAILED', ErrorDescription: 'Invalid PIN mode' });
    }
    if (!pin || typeof pin !== 'string' || !/^\d{4,6}$/.test(pin)) {
        return res.status(400).json({ status: 'FAILED', ErrorDescription: 'Invalid PIN format' });
    }
    savePin(mode, pin);
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

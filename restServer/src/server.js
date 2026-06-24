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
const defaultConfigFile = process.env.CONFIG_JSON
    || path.join(process.env.SNAP_DATA || process.cwd(), 'config.json');
const sseHeartbeatMs = Number(process.env.LOG_SSE_HEARTBEAT_MS || 15000);

// PIN storage — use SNAP_DATA if available, else same dir as server.js
const pinDir  = process.env.SNAP_DATA || path.dirname(__filename);
const pinFile = path.join(pinDir, 'pin.json');
const PIN_ITERATIONS = 100000;
const PIN_KEYLEN     = 64;
const PIN_DIGEST     = 'sha512';
const VALID_PIN_MODES = new Set(['production', 'debug']);

function loadRuntimeConfig() {
    try {
        return JSON.parse(fs.readFileSync(defaultConfigFile, 'utf8'));
    } catch {
        return {};
    }
}

function resolveLogFile() {
    const cfg = loadRuntimeConfig();
    const configured = process.env.LOG_FILE
        || cfg.teensyLogFilename
        || cfg.logFile
        || cfg.logFilename;
    if (!configured || typeof configured !== 'string' || !configured.trim()) return null;
    return path.resolve(configured.trim());
}

function ensureLogFile(logFile) {
    fs.mkdirSync(path.dirname(logFile), { recursive: true });
    if (!fs.existsSync(logFile)) fs.writeFileSync(logFile, '', 'utf8');
}

function tailLogLines(logFile, numLines) {
    const content = fs.readFileSync(logFile, 'utf8');
    return content.split('\n').filter(line => line.trim()).slice(-numLines);
}

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
    const cfg = loadRuntimeConfig();
    res.json({
        status: 'OK',
        machineName: process.env.MACHINE_NAME || cfg.machineName || 'FC?',
        logFile: resolveLogFile()
    });
});

app.get('/logs/stream', (req, res) => {
    const logFile = resolveLogFile();
    if (!logFile) {
        return res.status(400).json({
            status: 'FAILED',
            errorCode: 14,
            ErrorDescription: 'Log file not configured'
        });
    }

    ensureLogFile(logFile);

    res.setHeader('Content-Type', 'text/event-stream');
    res.setHeader('Cache-Control', 'no-cache');
    res.setHeader('Connection', 'keep-alive');
    res.setHeader('Access-Control-Allow-Origin', '*');
    if (typeof res.flushHeaders === 'function') res.flushHeaders();

    res.write('retry: 3000\n\n');

    const initialLines = Math.min(Math.max(Number(req.query.lines || 500), 1), 2000);
    tailLogLines(logFile, initialLines).forEach(line => {
        res.write(`data: ${JSON.stringify({ line })}\n\n`);
    });

    let lastSize = fs.statSync(logFile).size;
    let buffer = '';
    let watcher;

    const readNewBytes = () => {
        try {
            const currentSize = fs.statSync(logFile).size;
            if (currentSize < lastSize) {
                lastSize = currentSize;
                buffer = '';
                return;
            }
            if (currentSize === lastSize) return;

            const fd = fs.openSync(logFile, 'r');
            const byteCount = currentSize - lastSize;
            const chunk = Buffer.alloc(byteCount);
            fs.readSync(fd, chunk, 0, byteCount, lastSize);
            fs.closeSync(fd);

            buffer += chunk.toString('utf8');
            const lines = buffer.split('\n');
            buffer = lines.pop() || '';
            lines.forEach(line => {
                if (line.trim()) res.write(`data: ${JSON.stringify({ line })}\n\n`);
            });
            lastSize = currentSize;
        } catch (err) {
            // Ignore transient read failures during rotate/truncate windows.
        }
    };

    const startWatcher = () => {
        try {
            if (watcher) watcher.close();
            watcher = fs.watch(logFile, (eventType) => {
                if (eventType === 'change') {
                    readNewBytes();
                    return;
                }
                if (eventType === 'rename') {
                    setTimeout(() => {
                        try {
                            ensureLogFile(logFile);
                            lastSize = fs.statSync(logFile).size;
                            buffer = '';
                            startWatcher();
                        } catch {
                            // Keep stream alive; next rename/change will retry.
                        }
                    }, 200);
                }
            });
        } catch {
            // If watch cannot start, stream still stays open with heartbeat.
        }
    };

    startWatcher();

    const heartbeat = setInterval(() => {
        try {
            res.write(': heartbeat\n\n');
        } catch {
            // Closed by client.
        }
    }, sseHeartbeatMs);

    const cleanup = () => {
        clearInterval(heartbeat);
        if (watcher) watcher.close();
    };

    req.on('close', cleanup);
    res.on('error', cleanup);
});

app.get('/logs/tail', (req, res) => {
    const logFile = resolveLogFile();
    if (!logFile) {
        return res.status(400).json({
            status: 'FAILED',
            errorCode: 14,
            ErrorDescription: 'Log file not configured'
        });
    }

    ensureLogFile(logFile);
    const maxLines = 2000;
    const linesReq = Number(req.query.lines || 100);
    const numLines = Math.min(Math.max(linesReq, 1), maxLines);
    res.json({
        status: 'OK',
        lines: tailLogLines(logFile, numLines)
    });
});

app.get('/logs/download', (req, res) => {
    const logFile = resolveLogFile();
    if (!logFile) {
        return res.status(400).json({
            status: 'FAILED',
            errorCode: 14,
            ErrorDescription: 'Log file not configured'
        });
    }

    ensureLogFile(logFile);
    return res.download(logFile, path.basename(logFile));
});

app.post('/logs/clear', (req, res) => {
    const logFile = resolveLogFile();
    if (!logFile) {
        return res.status(400).json({
            status: 'FAILED',
            errorCode: 14,
            ErrorDescription: 'Log file not configured'
        });
    }

    ensureLogFile(logFile);
    fs.writeFileSync(logFile, `${new Date().toISOString()}\n`, 'utf8');
    res.json({
        status: 'OK',
        errorCode: 0,
        ErrorDescription: 'Log file cleared'
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

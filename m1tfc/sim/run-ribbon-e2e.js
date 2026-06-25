'use strict';

const fs = require('fs');
const path = require('path');
const { spawn } = require('child_process');
const { SerialPort } = require('serialport');
const { ReadlineParser } = require('@serialport/parser-readline');

const runtimeContext = require('../utils/runtimeContext');
const testBoardLink = require('../src/testBoardLink');
const targetICTLink = require('../src/m1ICTLink');
const { runRibbonCableTest } = require('../tests/ribbonCable');
const {
    parseSerialCommand,
    createSharedFixtureState,
    createFixtureApi,
    createUutIctApi,
    createTeensyRestApi,
    startRestApiServer
} = require('./deviceApi');

const SIM_DIR = `/tmp/m1tfc-sim-${process.pid}`;

function sleep(ms) {
    return new Promise((resolve) => setTimeout(resolve, ms));
}

function ensureDir(dirPath) {
    if (!fs.existsSync(dirPath)) {
        fs.mkdirSync(dirPath, { recursive: true });
    }
}

function waitForFile(filePath, timeoutMs = 5000) {
    const start = Date.now();
    return new Promise((resolve, reject) => {
        const timer = setInterval(() => {
            if (fs.existsSync(filePath)) {
                clearInterval(timer);
                resolve();
                return;
            }
            if (Date.now() - start > timeoutMs) {
                clearInterval(timer);
                reject(new Error(`Timed out waiting for ${filePath}`));
            }
        }, 50);
    });
}

async function startPtyPair(name) {
    const hostPath = path.join(SIM_DIR, `${name}.host`);
    const simPath = path.join(SIM_DIR, `${name}.sim`);

    const socat = spawn('socat', [
        '-d',
        '-d',
        `pty,raw,echo=0,link=${hostPath}`,
        `pty,raw,echo=0,link=${simPath}`
    ], {
        stdio: ['ignore', 'pipe', 'pipe']
    });

    let stderrBuf = '';
    socat.stderr.on('data', (chunk) => {
        stderrBuf += chunk.toString();
    });

    socat.on('exit', (code) => {
        if (code !== 0) {
            // Best effort diagnostics for unexpected early exits.
            process.stderr.write(`socat ${name} exited with code ${code}\n${stderrBuf}\n`);
        }
    });

    await waitForFile(hostPath);
    await waitForFile(simPath);

    return {
        name,
        hostPath,
        simPath,
        process: socat
    };
}

function openJsonResponder(portPath, onCommand) {
    const port = new SerialPort({ path: portPath, baudRate: 115200 });
    const parser = port.pipe(new ReadlineParser({ delimiter: '\n\r' }));

    parser.on('data', async(raw) => {
        const request = parseSerialCommand(raw);
        if (!request.cmd) {
            return;
        }
        try {
            const response = await onCommand(request);
            port.write(`${JSON.stringify(response)}\n\r`);
        } catch (err) {
            port.write(`${JSON.stringify({ status: false, error: err.message })}\n\r`);
        }
    });

    return { port, parser };
}

function createLogger() {
    return {
        info: (...args) => console.log('[INFO]', ...args),
        error: (...args) => console.error('[ERROR]', ...args),
        debug: (...args) => console.log('[DEBUG]', ...args),
        warn: (...args) => console.warn('[WARN]', ...args)
    };
}

function createDbStub() {
    return {
        errors: [],
        updateErrorCode(serial, errorCode, suffix) {
            this.errors.push({ serial, errorCode, suffix });
        }
    };
}

async function main() {
    ensureDir(SIM_DIR);

    const logger = createLogger();
    const db = createDbStub();

    const fixtureState = createSharedFixtureState();
    const fixtureApi = createFixtureApi(fixtureState);
    const uutApi = createUutIctApi(fixtureState);
    const teensy2RestApi = createTeensyRestApi();

    const pairs = [];
    const serialServers = [];
    let restServer;
    try {
        pairs.push(await startPtyPair('fixture'));
        pairs.push(await startPtyPair('uut-ict'));
        pairs.push(await startPtyPair('uut-term'));

        serialServers.push(openJsonResponder(pairs[0].simPath, async(request) => fixtureApi.handle(request)));
        serialServers.push(openJsonResponder(pairs[1].simPath, async(request) => uutApi.handle(request)));

        restServer = await startRestApiServer('127.0.0.1', 18081, teensy2RestApi, logger);

        runtimeContext.setRuntime({
            serial: 'SIM-RIBBON-001',
            productName: 'm1-3200',
            dbPath: SIM_DIR,
            m1defaultIP: '127.0.0.1'
        });

        await testBoardLink.initSerial(pairs[0].hostPath, 115200, logger);
        await targetICTLink.initSerial(pairs[1].hostPath, 115200, logger);
        await sleep(200);
        await testBoardLink.retrieveIoDef();

        const passed = await runRibbonCableTest(false, 0.1, logger, db, {}, false, null);

        if (!passed) {
            console.error('Ribbon cable E2E simulation failed');
            if (db.errors.length) {
                console.error('Captured error codes:', JSON.stringify(db.errors, null, 2));
            }
            process.exitCode = 1;
            return;
        }

        console.log('Ribbon cable E2E simulation passed');
        console.log('Second Teensy REST simulator is available at http://127.0.0.1:18081/command');
        process.exitCode = 0;
    } finally {
        if (restServer) {
            await new Promise((resolve) => restServer.close(resolve));
        }

        for (const server of serialServers) {
            if (server && server.port && server.port.isOpen) {
                await new Promise((resolve) => server.port.close(() => resolve()));
            }
        }

        for (const pair of pairs) {
            if (pair && pair.process && !pair.process.killed) {
                pair.process.kill('SIGTERM');
            }
        }
    }
}

main().catch((err) => {
    console.error(err);
    process.exit(1);
});

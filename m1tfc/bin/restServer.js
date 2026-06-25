'use strict';

const http = require('http');
const { spawn } = require('child_process');
const exitCodes = require('../src/exitCodes');

const supportedCommands = new Set([
    'm1dfu',
    'm1tbcmd',
    'm1cmd',
    'mnpcmd',
    'ict',
    'eeprom',
    'progmac',
    'flash',
    'pingM1apps',
    'cleanup',
    'functest',
    'makelabel'
]);

const exitCodeDescriptions = {
    [exitCodes.normalExit]: 'Success',
    [exitCodes.configFileMissing]: 'Configuration file is missing',
    [exitCodes.macMissing]: 'MAC address is missing',
    [exitCodes.commandFailed]: 'Command execution failed',
    [exitCodes.ictTestFailed]: 'ICT test failed',
    [exitCodes.testPointTestFailed]: 'Test point test failed',
    [exitCodes.programEepromFailed]: 'EEPROM programming failed',
    [exitCodes.programMacFailed]: 'MAC programming failed',
    [exitCodes.tamperSensorTestFailed]: 'Tamper sensor test failed',
    [exitCodes.configVendorSiteMissing]: 'Vendor site is missing in configuration',
    [exitCodes.otpIsNotBlank]: 'OTP is not blank',
    [exitCodes.eepromIsNotBlank]: 'EEPROM is not blank',
    [exitCodes.notAllTestsPassed]: 'Not all tests passed',
    [exitCodes.functTestFailed]: 'Functional test failed',
    [exitCodes.invalidArgument]: 'Invalid argument',
    [exitCodes.precheckHWFailed]: 'Hardware precheck failed',
    [exitCodes.poeTestFailed]: 'POE test failed'
};

function parseQuotedArgs(argumentString) {
    const args = [];
    const regex = /"([^"]*)"|'([^']*)'|([^\s"']+)/g;
    let match;

    while ((match = regex.exec(argumentString)) !== null) {
        args.push(match[1] || match[2] || match[3]);
    }

    return args;
}

function normalizeFlagName(key) {
    if (key.startsWith('-')) {
        return key;
    }
    return key.length === 1 ? `-${key}` : `--${key}`;
}

function normalizeArguments(argument) {
    if (argument === undefined || argument === null) {
        return [];
    }

    if (Array.isArray(argument)) {
        return argument.map(value => String(value));
    }

    if (typeof argument === 'string') {
        return parseQuotedArgs(argument);
    }

    if (typeof argument === 'object') {
        const args = [];
        const positional = argument.positional || argument.args;

        if (Array.isArray(positional)) {
            positional.forEach(value => args.push(String(value)));
        }

        Object.entries(argument).forEach(([key, value]) => {
            if (key === 'positional' || key === 'args') {
                return;
            }
            if (value === undefined || value === null || value === false) {
                return;
            }

            const flag = normalizeFlagName(key);
            if (value === true) {
                args.push(flag);
                return;
            }

            if (Array.isArray(value)) {
                value.forEach(item => {
                    args.push(flag, String(item));
                });
                return;
            }

            args.push(flag, String(value));
        });

        return args;
    }

    return [String(argument)];
}

function createResponse(status, errorCode, description) {
    return {
        status,
        errorCode,
        ErrorDescription: description
    };
}

function getErrorDescription(exitCode, stderrOutput) {
    if (exitCode === exitCodes.normalExit) {
        return 'Success';
    }
    if (exitCodeDescriptions[exitCode]) {
        return exitCodeDescriptions[exitCode];
    }

    const fallback = stderrOutput
        .split(/\r?\n/)
        .map(line => line.trim())
        .filter(Boolean)
        .pop();

    return fallback || 'Unknown error';
}

function executeCliCommand(entryFile, command, commandArgs) {
    return new Promise((resolve, reject) => {
        const child = spawn(process.execPath, [entryFile, command, ...commandArgs], {
            cwd: process.cwd(),
            env: process.env
        });

        let stderrOutput = '';

        child.stderr.on('data', chunk => {
            stderrOutput += chunk.toString();
        });

        child.on('error', reject);
        child.on('close', exitCode => {
            const normalizedExit = Number.isInteger(exitCode) ? exitCode : exitCodes.commandFailed;
            resolve({
                exitCode: normalizedExit,
                stderr: stderrOutput
            });
        });
    });
}

function writeJsonResponse(res, statusCode, payload) {
    res.writeHead(statusCode, { 'Content-Type': 'application/json' });
    res.end(JSON.stringify(payload));
}

function parseRequestBody(req) {
    return new Promise((resolve, reject) => {
        let body = '';

        req.on('data', chunk => {
            body += chunk.toString();
            if (body.length > 1024 * 1024) {
                reject(new Error('Request body too large'));
            }
        });

        req.on('end', () => {
            try {
                resolve(body ? JSON.parse(body) : {});
            } catch (err) {
                reject(new Error('Invalid JSON body'));
            }
        });

        req.on('error', reject);
    });
}

async function handleCommandRequest(req, res, options) {
    const body = await parseRequestBody(req);
    const { command, argument } = body;

    if (!command || typeof command !== 'string') {
        writeJsonResponse(
            res,
            400,
            createResponse(
                'FAILED',
                exitCodes.invalidArgument,
                'Field "command" must be a non-empty string'
            )
        );
        return;
    }

    if (!supportedCommands.has(command)) {
        writeJsonResponse(
            res,
            400,
            createResponse('FAILED', exitCodes.invalidArgument, `Unsupported command "${command}"`)
        );
        return;
    }

    const commandArgs = normalizeArguments(argument);
    const result = await executeCliCommand(options.entryFile, command, commandArgs);
    const description = getErrorDescription(result.exitCode, result.stderr);
    const status = result.exitCode === exitCodes.normalExit ? 'OK' : 'FAILED';
    const httpCode = result.exitCode === exitCodes.normalExit ? 200 : 500;

    writeJsonResponse(res, httpCode, createResponse(status, result.exitCode, description));
}

function startRestServer(options) {
    const server = http.createServer(async(req, res) => {
        try {
            if (req.method === 'GET' && req.url === '/health') {
                writeJsonResponse(res, 200, createResponse('OK', 0, 'Server is running'));
                return;
            }

            if (req.method === 'POST' && req.url === '/command') {
                await handleCommandRequest(req, res, options);
                return;
            }

            writeJsonResponse(
                res,
                404,
                createResponse('FAILED', exitCodes.invalidArgument, 'Route not found')
            );
        } catch (err) {
            writeJsonResponse(
                res,
                500,
                createResponse(
                    'FAILED',
                    exitCodes.commandFailed,
                    err.message || 'Internal server error'
                )
            );
        }
    });

    return new Promise((resolve, reject) => {
        server.on('error', reject);
        server.listen(options.port, options.host, () => {
            options.log.log(`REST server listening on http://${options.host}:${options.port}`);
            resolve(server);
        });
    });
}

module.exports = {
    startRestServer
};

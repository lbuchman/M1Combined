'use strict';

const { spawn } = require('child_process');
const { exitCodeDescriptions } = require('./errorMap');
const logger = require('./logger');

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

function getSupportedCommands() {
    return Array.from(supportedCommands);
}



function parseJsonStdout(stdout) {
    if (!stdout) return null;

    const trimmed = stdout.trim();
    if (!trimmed) return null;

    try {
        return JSON.parse(trimmed);
    }
    catch (err) {
        // Fall through to scan the output line by line.
    }

    const lines = trimmed
        .split(/\r?\n/)
        .map(line => line.trim())
        .filter(Boolean);

    for (let idx = lines.length - 1; idx >= 0; idx -= 1) {
        try {
            return JSON.parse(lines[idx]);
        }
        catch (err) {
            // ignore and keep scanning backward
        }
    }

    return null;
}

function flagFor(key) {
    return key.length === 1 ? `-${key}` : `--${key}`;
}

function toArgv(argument) {
    if (argument === undefined || argument === null || argument === '') return [];
    if (typeof argument === 'string') {
        return argument.split(/\s+/).filter(Boolean);
    }
    if (typeof argument === 'number') return [String(argument)];
    if (typeof argument !== 'object' || Array.isArray(argument)) return [String(argument)];

    const argv = [];

    if (Array.isArray(argument.positional)) {
        for (const item of argument.positional) {
            argv.push(String(item));
        }
    }

    for (const [key, value] of Object.entries(argument)) {
        if (key === 'positional') continue;
        if (value === undefined || value === null || value === false) continue;

        const flag = flagFor(key);
        if (value === true) {
            argv.push(flag);
            continue;
        }

        argv.push(flag, String(value));
    }

    return argv;
}

function buildResult(exitCode, stdout, stderr) {
    const parsedOutput = parseJsonStdout(stdout);
    const fallbackStatus = exitCode === 0 ? 'OK' : 'FAILED';
    const status = parsedOutput && typeof parsedOutput.status === 'string'
        ? parsedOutput.status
        : fallbackStatus;
    const errorCode = parsedOutput && Number.isInteger(parsedOutput.errorCode)
        ? parsedOutput.errorCode
        : exitCode;
    const description = parsedOutput && typeof parsedOutput.ErrorDescription === 'string'
        ? parsedOutput.ErrorDescription
        : parsedOutput && typeof parsedOutput.errorDescription === 'string'
            ? parsedOutput.errorDescription
            : descriptionFor(exitCode, stderr);

    if (parsedOutput && typeof parsedOutput === 'object' && !Array.isArray(parsedOutput)) {
        return {
            ...parsedOutput,
            status,
            errorCode,
            ErrorDescription: description,
            commandOutput: parsedOutput
        };
    }

    return {
        status,
        errorCode,
        ErrorDescription: description,
        commandOutput: parsedOutput
    };
}

function descriptionFor(exitCode, stderr) {
    if (exitCodeDescriptions[exitCode]) return exitCodeDescriptions[exitCode];
    const fallback = stderr
        .split(/\r?\n/)
        .map(line => line.trim())
        .filter(Boolean)
        .pop();
    return fallback || 'Unknown error';
}

class CommandRunner {
    constructor(options = {}) {
        this.baseCommand = options.baseCommand || 'm1tfd1.cli';
        this.baseArgs = Array.isArray(options.baseArgs) ? options.baseArgs : [];
        this.cwd = options.cwd || process.cwd();
        this.env = options.env || process.env;
        this.spawnImpl = typeof options.spawnImpl === 'function' ? options.spawnImpl : spawn;
    }

    run(command, argument) {
        if (!supportedCommands.has(command)) {
            return Promise.resolve({
                status: 'FAILED',
                errorCode: 14,
                ErrorDescription: `Unsupported command "${command}"`,
                commandOutput: null
            });
        }

        return this.execute(command, argument || '');
    }

    execute(command, argument) {
        return new Promise((resolve) => {
            const cmdArgs = [...this.baseArgs, command, ...toArgv(argument)];
            
            // Log environment context for debugging
            logger.info('Executing command with environment', {
                command: this.baseCommand,
                args: cmdArgs,
                cwd: this.cwd,
                PATH: this.env.PATH,
                SNAP_DATA: this.env.SNAP_DATA,
                SNAP_COMMON: this.env.SNAP_COMMON,
                USER: this.env.USER,
                HOME: this.env.HOME,
                NODE_ENV: this.env.NODE_ENV
            });

            const child = this.spawnImpl(this.baseCommand, cmdArgs, {
                cwd: this.cwd,
                env: this.env,
                shell: false
            });

            let stdout = '';
            let stderr = '';

            child.stdout.on('data', chunk => {
                const data = chunk.toString();
                stdout += data;
                logger.info(`[STDOUT] ${data.trim()}`);
            });

            child.stderr.on('data', chunk => {
                const data = chunk.toString();
                stderr += data;
                logger.warn(`[STDERR] ${data.trim()}`);
            });

            child.on('error', err => {
                const errorMsg = `Failed to start command process: ${err.message}`;
                logger.error(errorMsg, {
                    command: this.baseCommand,
                    args: cmdArgs,
                    cwd: this.cwd,
                    envPath: this.env.PATH
                });
                resolve({
                    status: 'FAILED',
                    errorCode: 3,
                    ErrorDescription: errorMsg,
                    commandOutput: null
                });
            });

            child.on('close', code => {
                const exitCode = Number.isInteger(code) ? code : 3;
                const result = buildResult(exitCode, stdout, stderr);
                if (exitCode !== 0) {
                    logger.error(`Command exited with code ${exitCode}`, {
                        command: this.baseCommand,
                        args: cmdArgs,
                        cwd: this.cwd,
                        stderr: stderr.substring(0, 200)
                    });
                }
                resolve(result);
            });
        });
    }
}

module.exports = {
    CommandRunner,
    supportedCommands,
    getSupportedCommands
};

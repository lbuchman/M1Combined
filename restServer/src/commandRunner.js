'use strict';

const { spawn } = require('child_process');
const { exitCodeDescriptions } = require('./errorMap');

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
    if (key.startsWith('-')) return key;
    return key.length === 1 ? `-${key}` : `--${key}`;
}

function normalizeArguments(argument) {
    if (argument === undefined || argument === null) return [];

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
            if (key === 'positional' || key === 'args') return;
            if (value === undefined || value === null || value === false) return;

            const flag = normalizeFlagName(key);
            if (value === true) {
                args.push(flag);
                return;
            }

            if (Array.isArray(value)) {
                value.forEach(item => args.push(flag, String(item)));
                return;
            }

            args.push(flag, String(value));
        });

        return args;
    }

    return [String(argument)];
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
        this.baseCommand = options.baseCommand || 'm1tfc';
        this.baseArgs = Array.isArray(options.baseArgs) ? options.baseArgs : [];
        this.cwd = options.cwd || process.cwd();
        this.env = options.env || process.env;
        this.spawnImpl = typeof options.spawnImpl === 'function' ? options.spawnImpl : spawn;
        this.queue = Promise.resolve();
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

        const commandArgs = normalizeArguments(argument);

        const task = () => this.execute(command, commandArgs);
        const resultPromise = this.queue.then(task, task);

        this.queue = resultPromise.then(() => undefined, () => undefined);
        return resultPromise;
    }

    execute(command, commandArgs) {
        return new Promise((resolve) => {
            const child = this.spawnImpl(this.baseCommand, [...this.baseArgs, command, ...commandArgs], {
                cwd: this.cwd,
                env: this.env,
                shell: false
            });

            let stdout = '';
            let stderr = '';

            child.stdout.on('data', chunk => {
                stdout += chunk.toString();
            });

            child.stderr.on('data', chunk => {
                stderr += chunk.toString();
            });

            child.on('error', err => {
                resolve({
                    status: 'FAILED',
                    errorCode: 3,
                    ErrorDescription: `Failed to start command process: ${err.message}`,
                    commandOutput: null
                });
            });

            child.on('close', code => {
                const exitCode = Number.isInteger(code) ? code : 3;
                resolve(buildResult(exitCode, stdout, stderr));
            });
        });
    }
}

module.exports = {
    CommandRunner,
    supportedCommands,
    getSupportedCommands
};

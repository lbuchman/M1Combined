'use strict';

const winston = require('winston');
const _ = require('lodash');
// eslint-disable-next-line no-unused-vars
const fs = require('fs-extra');
const path = require('path');
const { mkdirp } = require('mkdirp');

let logger;

const addCallerInfo = winston.format((info) => {
    const err = new Error();
    const stack = err.stack.split('\n');
    const callerLine = stack.find(line =>
        line.includes('.js:') &&
        !line.includes('node_modules') &&
        !line.includes('logger.js')
    );
    if (callerLine) {
        const match = callerLine.match(/\((.+):(\d+):\d+\)/) || callerLine.match(/at (.+):(\d+):\d+/);
        if (match) {
            const file = match[1].split('/').slice(-2).join('/');
            info.caller = `${file}:${match[2]}`;
        }
    }
    return info;
});

/** path.join(configData.logdir, `${options.serial}.log`)
* @public
* Gets the logger.
* @param {string} name The logger name.
* @returns {object} A logger object.
*/
function getLogger(name, test, serial, logFilePath, logdebug) {
    let consoleLogLevel;
    const logFileDir = `${logFilePath}/logs/${serial}`;
    mkdirp.sync(logFileDir);
    switch (logdebug) {
    case '0':
        consoleLogLevel = 'error';
        break;
    case '1':
        consoleLogLevel = 'info';
        break;
    case '2':
        consoleLogLevel = 'debug';
        break;
    default: throw new Error('Invalid debug level, shall be 0-2');
    }

    const lofFileNamePathDebug = path.join(logFileDir, `${serial}-debug.log`);
    const lofFileNamePathInfo = path.join(logFileDir, `${serial}-info.log`);
    const lofFileNamePathError = path.join(logFileDir, `${serial}-error.log`);
    if (logger) {
        return logger;
    }

    const msg = i => (_.isObject(i.message) ? JSON.stringify(i.message) : i.message);
    logger = winston.createLogger({
        level: 'info',
        format: winston.format.combine(
            addCallerInfo(),
            winston.format.timestamp({ format: 'YYYY-MM-DD HH:mm:ss' }),
            winston.format.align(),
            winston.format.printf(i => `[${i.timestamp}] [${name}, ${test}] [${i.caller || '?'}] ${i.level}: ${msg(i)}`)
        ),
        transports: [
            new winston.transports.Console({ level: consoleLogLevel }),
            new winston.transports.File({ filename: `${lofFileNamePathError}`, level: 'error' }),
            new winston.transports.File({ filename: `${lofFileNamePathDebug}`, level: 'debug' }),
            new winston.transports.File({ filename: `${lofFileNamePathInfo}`, level: 'info' })
        ]
    });
    winston.addColors({
        error: 'red',
        warn: 'yellow',
        info: 'cyan',
        debug: 'green'
    });
    return logger;
}

module.exports = {
    getLogger
};

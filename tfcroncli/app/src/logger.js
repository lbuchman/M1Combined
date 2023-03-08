'use strict';

const winston = require('winston');
const _ = require('lodash');
// eslint-disable-next-line no-unused-vars
const fs = require('fs-extra');
const path = require('path');
const { mkdirp } = require('mkdirp');

let logger;

/** path.join(configData.logdir, `${options.serial}.log`)
* @public
* Gets the logger.
* @param {string} name The logger name.
* @returns {object} A logger object.
*/
function getLogger(name, test, serial, logFilePath, logdebug) {
    let consoleLogLevel;
    const logFileDir = `${logFilePath}`;
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

    const lofFileNamePathDebug = path.join(logFileDir, `${serial}-debug-${test}.log`);
    const lofFileNamePathInfo = path.join(logFileDir, `${serial}-info-${test}.log`);
    const lofFileNamePathError = path.join(logFileDir, `${serial}-error-${test}.log`);
    if (logger) return logger;

    const msg = i => (_.isObject(i.message) ? JSON.stringify(i.message) : i.message);
    logger = winston.createLogger({
        level: 'info',
        format: winston.format.combine(
            winston.format.timestamp({ format: 'YYYY-MM-DD HH:mm:ss' }),
            winston.format.align(),
            winston.format.printf(i => `[${i.timestamp}] [${name}, ${test}] ${i.level}: ${msg(i)}`)
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

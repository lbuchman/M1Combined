'use strict';

const winston = require('winston');
const _ = require('lodash');

let logger;

/**
* @public
* Gets the logger.
* @param {string} name The logger name.
* @returns {object} A logger object.
*/
function getLogger(name, logFilePath) {
    if (logger) return logger;
    const msg = i => (_.isObject(i.message) ? JSON.stringify(i.message) : i.message);
    logger = winston.createLogger({
        level: 'info',
        format: winston.format.combine(
            winston.format.colorize(),
            winston.format.timestamp({ format: 'YYYY-MM-DD HH:mm:ss' }),
            winston.format.printf(i => `[${i.timestamp}] [${name}] ${i.level}: ${msg(i)}`)
        ),
        transports: [
            new winston.transports.Console({ level: 'debug' }),
            new winston.transports.File({ filename: `${logFilePath}.log`, level: 'info' }),
            new winston.transports.File({ filename: `${logFilePath}Debug.log`, level: 'debug' })
        ]
    });
    return logger;
}

module.exports = {
    getLogger
};

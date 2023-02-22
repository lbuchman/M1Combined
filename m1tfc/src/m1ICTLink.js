'use strict';

const { ReadlineParser } = require('@serialport/parser-readline');
const SerialLink = require('../src/serialLink');

module.exports = new SerialLink('M1-3200', 'm1', new ReadlineParser({ delimiter: '\n\r' }));

'use strict';

const m1dfu = require('./m1dfu');
const tbcmd = require('./tbcmd');
const m1cmd = require('./m1cmd');
const mnpcmd = require('./mnpcmd');
const ict = require('./ict');
const eeprom = require('./eeprom');
const progmac = require('./progmac');
const flash = require('./flash');
const pingM1apps = require('./pingM1apps');
const cleanup = require('./cleanup');
const functest = require('./functest');
const makelabel = require('./makelabel');

function registerAll(program) {
    [
        m1dfu,
        tbcmd,
        m1cmd,
        mnpcmd,
        ict,
        eeprom,
        progmac,
        flash,
        pingM1apps,
        cleanup,
        functest,
        makelabel
    ].forEach(moduleRef => moduleRef.register(program));
}

module.exports = {
    registerAll
};

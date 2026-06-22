'use strict';

const registerM1Dfu = require('./commands/m1dfu');
const registerTbCmd = require('./commands/tbcmd');
const registerM1Cmd = require('./commands/m1cmd');
const registerMnpCmd = require('./commands/mnpcmd');
const registerIct = require('./commands/ict');
const registerEeprom = require('./commands/eeprom');
const registerProgMac = require('./commands/progmac');
const registerFlash = require('./commands/flash');
const registerPingM1Apps = require('./commands/pingM1apps');
const registerCleanup = require('./commands/cleanup');
const registerFuncTest = require('./commands/functest');
const registerMakeLabel = require('./commands/makelabel');

function registerCommands(program) {
    registerM1Dfu(program);
    registerTbCmd(program);
    registerM1Cmd(program);
    registerMnpCmd(program);
    registerIct(program);
    registerEeprom(program);
    registerProgMac(program);
    registerFlash(program);
    registerPingM1Apps(program);
    registerCleanup(program);
    registerFuncTest(program);
    registerMakeLabel(program);
}

module.exports = {
    registerCommands
};

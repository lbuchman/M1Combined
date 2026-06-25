#!/usr/bin/env node

'use strict';

const { Command } = require('commander');
const { mkdirp } = require('mkdirp');
const os = require('../utils/os');
const { ensureSnapEnv, loadConfig } = require('./commandSupport');
const { registerAll } = require('./commands');
const { registerGlobalHandlers } = require('../utils/errorHandler');

const program = new Command();

ensureSnapEnv();
registerGlobalHandlers(console);

program
    .name('m1test')
    .description('CLI utility to test and program M1-3200 boards')
    .version(process.env.SNAP_VERSION || 'dev');

registerAll(program);

async function main() {
    await os.executeShellCommand('killall -9 STM32_Programmer_CLI', console, true);
    const configData = await loadConfig();
    mkdirp.sync(configData.mtfDir);
    mkdirp.sync(`${configData.mtfDir}/logs`);
    await program.parseAsync(process.argv);
}

main();

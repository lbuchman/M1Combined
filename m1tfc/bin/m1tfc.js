#!/usr/bin/env node

'use strict';

const program = require('commander');
const os = require('../utils/os');
const config = require('../utils/config');
const { mkdirp } = require('mkdirp');
const { registerCommands } = require('./m1tfcCommands');
const { configuration } = require('./m1tfcShared');

/* Just for running out of snap */
if (!process.env.SNAP) {
    process.env.SNAP_COMMON = `${process.env.HOME}/snap_common`;
    process.env.SNAP_DATA = `${process.env.HOME}/snap_data`;
    process.env.SNAP = '/snap/m1tfd/current';
    process.env.SNAP_VERSION = '08c433e';
}

program
    .name('m1test')
    .description('CLI utility to test and program M1-3200 boards')
    .version(process.env.SNAP_VERSION);

registerCommands(program);

const log = console;

os.executeShellCommand('killall -9 STM32_Programmer_CLI', log, true)
    .then(async () => {
        const configData = await config.getConfig(configuration);
        process.env.DBPATH = configData.mtfDir;
        mkdirp.sync(configData.mtfDir);
        mkdirp.sync(`${configData.mtfDir}/logs`);
        program.parse(process.argv);
    });

#!/usr/bin/env node

'use strict';

const program = require('commander');
const { mkdirp } = require('mkdirp');
const os = require('../utils/os');
const { ensureSnapEnv, loadConfig } = require('./commandSupport');
const { registerAll } = require('./commands');

ensureSnapEnv();

program
    .name('m1test')
    .description('CLI utility to test and program M1-3200 boards')
    .version(process.env.SNAP_VERSION || 'dev');

registerAll(program);

const log = console;

os.executeShellCommand('killall -9 STM32_Programmer_CLI', log, true)
    .then(async () => {
        const configData = await loadConfig();
        mkdirp.sync(configData.mtfDir);
        mkdirp.sync(`${configData.mtfDir}/logs`);
        program.parse(process.argv);
    })
    .catch(async () => {
        const configData = await loadConfig();
        mkdirp.sync(configData.mtfDir);
        mkdirp.sync(`${configData.mtfDir}/logs`);
        program.parse(process.argv);
    });

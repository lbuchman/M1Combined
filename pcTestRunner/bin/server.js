#!/usr/bin/env node

'use strict';

const app = require('../index');
const mkdirs = require('mkdirs');
const path = require('path');
const config = require('../utils/config');
const logger = require('../utils/logger');

const TestRunner = require('../tests/testRunner');

const configuration = config();

const name = 'm1TesterServer';
mkdirs(config().logdir);
const logFilePath = path.join(config().logdir, config().m1TestfServerLogFilename);
const log = logger.getLogger(name, logFilePath, config().m1TestfServerLogFilename, 'debug');

const testRunner = new TestRunner();
testRunner.init(log);

app.listen(configuration.tcpPort, (err) => {
    if (err) throw err;
    log.debug(`Server is running on port ${configuration.tcpPort}`);
});

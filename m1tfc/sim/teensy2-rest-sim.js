#!/usr/bin/env node

'use strict';

const { createTeensyRestApi, startRestApiServer } = require('./deviceApi');

function getArg(name, fallback) {
    const idx = process.argv.indexOf(name);
    if (idx < 0 || idx + 1 >= process.argv.length) {
        return fallback;
    }
    return process.argv[idx + 1];
}

const host = getArg('--host', '127.0.0.1');
const port = Number(getArg('--port', '18081'));

async function main() {
    const api = createTeensyRestApi();
    await startRestApiServer(host, port, api, console);
}

main().catch((err) => {
    console.error(err);
    process.exit(1);
});

'use strict';

const fs = require('fs');
const path = require('path');

/**
 * Auto-discovers and registers all command modules in this directory.
 * Each file (except index.js) must export a `register(program)` function.
 * @param {import('commander').Command} program - Commander program instance
 */
function registerAll(program) {
    const commandsDir = __dirname;
    const files = fs.readdirSync(commandsDir)
        .filter(f => f.endsWith('.js') && f !== 'index.js')
        .sort();

    for (const file of files) {
        const mod = require(path.join(commandsDir, file));
        if (typeof mod.register === 'function') {
            mod.register(program);
        }
    }
}

module.exports = {
    registerAll
};

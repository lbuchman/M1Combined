'use strict';

const { exec } = require('child_process');
const fs = require('fs-extra');


/**
 * @private
 * execute shell command
 */
function executeShellCommand(command, log, nothrow = true, errorStdout = true, maxBuffer = 1024 * 1024, cdw = process.cwd()) {
    const cirrentDir = process.cwd();
    process.chdir(cdw);
    return new Promise((resolve, reject) => {
        exec(command, { maxBuffer }, (err, stdout, stderr) => {
            if (err) {
                // log.error(`${command},  stdout =:\n${stdout}`);
                // stderrLocal = stderr;
                // log.debug(stdout);
                let output = errorStdout ? stdout : err;
                if (output === '') output = stderr;
                process.chdir(cirrentDir);
                reject(new Error(output));
            }
            else {
                // log.debug(`"${command}" - command complete success`);
                let output = stdout;
                if (!output) output = 'no output';
                process.chdir(cirrentDir);
                resolve(output);
            }
            if (stdout) {
                //  log.debug(`${command},  stdout =:\n${stdout}`);
            }
            if (stderr) {
                // log.debug(`${command},  stderr =:\n -> ${stderr}`);
            }
        });
    })
        .catch((err) => {
            process.chdir(cirrentDir);
            if (nothrow) {
                // log.error(err.message);
                // log.error(`${command},  stderr =:\n${stderrLocal}`);
                return err;
            }
            throw (err);
        });
}

/**
 * @private
 * execute shell command
 */
function getFrontendPid() {
    try {
        const pid = fs.readFileSync(`${process.env.HOME}/m1mtf/m1tfd1app.pid`);
        return pid.toString().trim();
    }
    catch (err) {
        //
    }
    return null;
}

/**
 * @private
 * execute shell command
 */
function getDate() {
    const now = new Date().toISOString().replace(/T/, ' ').replace(/\..+/, '');
    return now;
}

module.exports = {
    executeShellCommand,
    getFrontendPid,
    getDate
};

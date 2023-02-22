'use strict';

const { exec } = require('child_process');

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
                const output = errorStdout ? stdout : err;
                process.chdir(cirrentDir);
                reject(output);
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

module.exports = {
    executeShellCommand
};

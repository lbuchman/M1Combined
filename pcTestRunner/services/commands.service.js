'use strict';

const fs = require('fs-extra');

const teensy = require('../teensy/teensy');
const logger = require('../utils/logger');

const logFile = '/home/pi/teensyControl.log';

module.exports = {
    sendCommand: async (req, res) => {
        const cmdJson = req.body;
        const log = logger();
        try {
            let cmd;
            if (!cmdJson.cmd) {
                log.error('empty cmd, rejected');
                return res.status(500).json({ error: 'empty cmd, rejected' });
            }
            if (cmdJson.arg !== 'null') cmd = `${cmdJson.cmd} ${cmdJson.arg}`;
            else cmd = `${cmdJson.cmd}`;
            let ret;
            if (cmdJson.timeout) ret = await teensy.sendCommand(cmd, cmdJson.timeout);
            else ret = await teensy.sendCommand(cmd);
            return res.status(200).json(ret);
        }
        catch (err) {
            log.error(err.message);
            return res.status(500).json({ error: err.message });
        }
    },
    clearLog: () => {
        fs.writeFileSync(logFile, `${new Date()}\n`);
    }
};

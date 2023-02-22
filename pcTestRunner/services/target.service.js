'use strict';

const miTerminal = require('../utils/targetTerminal');
const logger = require('../utils/logger');

module.exports = {
    executeCommand: async (req, res) => {
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
            let data;
            if (cmdJson.timeout) data = await miTerminal.executeCommand(cmd, cmdJson.timeout);
            else data = await miTerminal.executeCommand(cmd);

            // eslint-disable-next-line no-useless-escape
            let ret = await miTerminal.executeCommand('echo \$?');
            // eslint-disable-next-line no-useless-escape
            ret = await miTerminal.executeCommand('echo \$?');
            const result1 = ret.replace(/stm32mp1/g, '');
            const result = result1.replace(/\D/g, '');
            const retJson = { cmd: cmdJson.cmd, arg: cmdJson.arg, data, ret: result };

            return res.status(200).json(retJson);
        }
        catch (err) {
            log.error(err.message);
            return res.status(500).json({ error: err.message });
        }
    },
    loginCommand: async (req, res) => {
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
            if (cmdJson.timeout) ret = await miTerminal.executeCommand(cmd, cmdJson.timeout);
            else ret = await miTerminal.executeCommand(cmd);
            return res.status(200).json(ret);
        }
        catch (err) {
            log.error(err.message);
            return res.status(500).json({ error: err.message });
        }
    }

};

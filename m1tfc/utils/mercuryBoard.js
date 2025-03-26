'use strict';

const gwUdpDgram = require('@lenel/gwudpdgram');

module.exports = class MercuryBoardLink {
    constructor(log) {
        this.mercuryBoardIpAddress = '192.168.0.60:4111'; /*192.168.2.48*/
        this.logger = log;
        this.reply = [];
        this.udpDgram = gwUdpDgram.gwUdpSocket(this.logger, 0); // eslint-disable-line
        this.udpDgram.on('message', (event) => {
            this.reply.push(event);
        });
    }

    async begin() {
        await this.udpDgram.startServer();
    }

    async sendCommand(cmd, arg) {
        let count;
        let ret = { status: false };
        /* eslint-disable no-restricted-syntax */
        // eslint-disable-next-line no-plusplus
        for (count = 0; count <= 3; count++) {
            try {
                 // eslint-disable-next-line no-await-in-loop
                ret = await this.sendCommandRaw(cmd, arg);
                if (ret.status !== true) throw (new Error(`command ${cmd} ${arg} failed, ${ret.error}`));
                return ret;
            }
            catch (err) {
                if (count === 3) throw (err);
            }
        }
        return ret;
    }

    async sendCommandRaw(cmd, arg) {
        this.udpDgram.sendTo(this.mercuryBoardIpAddress, { cmd, arg });
        let count = 20;
        return new Promise((resolve, reject) => {
            const handler = setInterval(() => {
                if (this.reply.length) {
                    clearInterval(handler);
                    const reply = this.reply[0];
                    if (!reply.status) {
                        reject(new Error(`Error, command to Mercury Board ${cmd} ${arg} failed, ${reply.error} `));
                        clearInterval(handler);
                        return;
                    }
                    resolve(this.reply[0]);
                    this.reply = [];
                    return;
                }
                count -= 1;
                if (count === 0) {
                    reject(new Error(`Error, command to Mercury Board "${cmd} ${arg}" failed, udp timeout `));
                    clearInterval(handler);
                }
            }, 100);
        });
    }
};

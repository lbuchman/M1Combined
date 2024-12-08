'use strict';

const gwUdpDgram = require('@lenel/gwudpdgram');

module.exports = class MercuryBoardLink {
    constructor(log) {
        this.mercuryBoardIpAddress = '192.168.0.60:4111';
        this.logger = log;
        this.reply = [];
        this.udpDgram = gwUdpDgram.gwUdpSocket(this.logger, 0); // eslint-disable-line
        this.udpDgram.on('message', (event) => {
            this.reply.push(event);
        });
    }

    async begin() {
        this.udpDgram.startServer();
    }

    async sendCommand(cmd, arg) {
        this.udpDgram.sendTo(this.mercuryBoardIpAddress, { cmd, arg });
        let count = 20;
        return new Promise((resolve, reject) => {
            const handler = setInterval(() => {
                if (this.reply.length) {
                    clearInterval(handler);
                    resolve(this.reply[0]);
                    this.reply = [];
                    return;
                }
                count -= 1;
                if (count === 0) {
                    reject(new Error('udp timeout'));
                    clearInterval(handler);
                }
            }, 100);
        });
    }
};

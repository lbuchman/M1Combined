'use strict';

const Udp = require('./udpDgram');
const MessageFormats = require('./messageFormats');

const defaultOptions = {
    socketRetryMin: 1000,
    socketRetryMax: 5000,
    socketRetryCount: 5,
    maxRetryTime: 300000,
    messageFormat: MessageFormats.JSON
};

const gwUdpSocket = (log, port, options = defaultOptions) => new Udp(log, port, options);

module.exports = {
    gwUdpSocket,
    MessageFormats
};

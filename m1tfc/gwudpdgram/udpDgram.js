'use strict';

const EventEmitter = require('events');
const udp = require('dgram');
const retry = require('retry');
const MessageFormats = require('./messageFormats');

module.exports = class UdpGWSocket extends EventEmitter {
    constructor(log, port, options) {
        super();
        this.log = log;
        this.options = options;
        this.port = port;
        this.socket = null;
    }

        /**
    * @public
    * Create udp socket
    *
    */
    createSocket() {
        this.log.debug(`Attempting to create UDP socket on port ${this.port}`);

        this.socket = udp.createSocket('udp4', (message, rinfo) => {
            if (this.options.messageFormat === MessageFormats.Buffer) {
                this.emit('message', message, rinfo);
                return;
            }

            let jsonMessage;
            try {
                jsonMessage = JSON.parse(message);
            }
            catch (err) {
                this.log.error(`could not parse message to Json - ${err.message}`);
                return;
            }

            this.emit('message', jsonMessage, rinfo);
        });
    }

    /**
    * @public
    * starts the domain socket server
    *
    */
    async startServer() {
        return new Promise((resolve, reject) => {
            this.createSocket();

            this.socket.on('listening', () => {
                const address = this.socket.address();
                this.log.debug(`server listening ${address.address}:${address.port}`);
            });

            this.socket.on('error', (err) => {
                this.log.error(`UDP socket error - ${err.message} - ${this.systemErrorDescription(err.errno)}`);
                this.emit('error', err);
            });

            this.socket.bind(this.port, (err) => {
                if (err) {
                    this.log.error(`UDP socket bind error - ${err.message}`);
                    reject(err);
                    return;
                }

                resolve();
            });
        });
    }

    /**
    * @public
    * close udp socket  listeners so event loop is released
    *
    */
    closeSocketAndListeners() {
        this.socket.removeAllListeners('message');
        this.socket.removeAllListeners('listening');
        this.socket.removeAllListeners('error');
        this.socket.close((err) => {
            if (err) {
                this.log.error(`UDP close error: ${err.message}`);
            }
        });
        this.socket = null;
    }

    /**
    * @public
    * close listeners so event loop is released
    *
    */
    close() {
        this.removeAllListeners('message');
        this.removeAllListeners('error');
        this.closeSocketAndListeners();
    }

    /**
    * @private
    * Sends data payload over the socket to the specified destination.
    * Example: send('azurebridge', { data: foo })
        destination: 'azurebridge'
     * @param {string} destination socket Name of the destination socket
     * @param {Buffer} data The data payload to be sent.
    */
    sendAsync(destination, data) {
        return new Promise((resolve, reject) => {
            let host = 'localhost';
            let port;
            const dest = destination.toString().split(':');
            if (dest.length > 1) {
                host = dest[0];
                port = dest[1];
            }
            else {
                port = destination;
            }
            if (!this.socket) {
                this.log.error('Udp socket is null');
                reject(new Error('Udp socket is null'));
                return;
            }
            this.socket.send(data, 0, data.length, port, host, (err) => {
                if (err) {
                    reject(err);
                }
                else {
                    resolve();
                }
            });
        });
    }

    /**
    * @private
    * converts json object to buffer
    * @param {Object} json object
    * @return buffer
    */
    jsonToBuffer(json) {
        let socketData = null;
        try {
            socketData = JSON.stringify(json);
        }
        catch (err) {
            this.log.error(`Could not parse object => ${json}`);
            throw (err);
        }
        return Buffer.from(socketData);
    }

    /**
    * @public
    * Sends data payload over the socket to the specified destination.
    * Example: send('azurebridge', { data: foo })
        destination: 'azurebridge'
    * @param {Object} data The data payload to be sent.
    */
    sendTo(destination, data, enableRetry = true) { // eslint-disable-line

        let dataBuffer;
        if (!Buffer.isBuffer(data)) {
            try {
                dataBuffer = this.jsonToBuffer(data);
            }
            catch (err) {
                return Promise.reject(err);
            }
        }
        else {
            dataBuffer = data;
        }
        const retryOptions = {
            forever: false,
            retries: this.options.socketRetryCount,
            maxRetryTime: this.options.maxRetryTime,
            factor: 2,
            minTimeout: this.options.socketRetryMin,
            maxTimeout: this.options.socketRetryMax,
            randomize: true
        };
        if (!enableRetry) {
            retryOptions.retries = 0;
        }
        const operation = retry.operation(retryOptions);

        return new Promise((resolve, reject) => {
            operation.attempt((currentAttempt) => { // eslint-disable-line
                this.sendAsync(destination, dataBuffer)
                .then(() => {
                    resolve();
                })
                .catch((err) => {
                    // eslint-disable-next-line max-len
                    this.log.warn(`Retrying to send datagram (retry = ${currentAttempt}) to ${destination} "${err.message}", message: ${dataBuffer.toString('utf8')}`);
                    if (!operation.retry(err)) {
                        // eslint-disable-next-line max-len
                        this.log.error(`Failed to send datagram to ${destination} "${this.systemErrorDescription(err.errno)}", message: ${dataBuffer.toString('utf8')}`);
                        reject(operation.mainError());
                    }
                });
            });
        });
    }

    /**
    * @private
    * decode Linux system error to string
    * @param {integer} errCode
    * @return {string} string error description
    */
    systemErrorDescription(errCode) { // eslint-disable-line
        if (errCode === undefined) {
            return 'undefined error code';
        }
        switch (errCode) {
            case -11:
                return 'Try again';
            case -13:
                return 'Permission denied';
            case -98:
                return 'Address already in use';
            case -111:
                return 'Connection refused';
            case -104:
                return 'Connection reset by peer';
            case -17:
                return 'File exists';
            case -21:
                return 'Is a directory';
            case -24:
                return 'Too many open files in system';
            case -2:
                return 'No such file or directory';
            case -20:
                return 'Not a directory';
            case -1:
                return 'Operation not permitted';
            case -32:
                return 'Broken pipe';
            case -110:
                return 'Operation timed out';
            default:
                return errCode.toString();
        }
    }
};

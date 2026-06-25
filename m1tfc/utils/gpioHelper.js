'use strict';

/* eslint-disable no-await-in-loop */

const CommandHelper = require('./commandHelper');

/**
 * GPIO operations helper - centralizes all GPIO command patterns
 */
class GPIOHelper {
    constructor(targetICTLink, logger, db) {
        this.targetICTLink = targetICTLink;
        this.logger = logger;
        this.db = db;
        this.cmdHelper = new CommandHelper(logger, db);
    }

    /**
     * Configure GPIO pin as output
     * @param {string} port - GPIO port (a-k)
     * @param {number} pin - Pin number
     * @param {string} errorCodeName - Error code key
     * @returns {boolean} - Success
     */
    async configureOutput(port, pin, errorCodeName = null) {
        return await this.cmdHelper.execute(
            () => this.targetICTLink.sendCommand(`confgpio ${port} ${pin} output none`),
            `Configure GPIO ${port}.${pin}`,
            errorCodeName,
            'T'
        );
    }

    /**
     * Set GPIO pin to specific level
     * @param {string} port - GPIO port (a-k)
     * @param {number} pin - Pin number
     * @param {number} level - Level (0 or 1)
     * @param {string} errorCodeName - Error code key
     * @returns {boolean} - Success
     */
    async setLevel(port, pin, level, errorCodeName = null) {
        return await this.cmdHelper.execute(
            () => this.targetICTLink.sendCommand(`setgpio ${port} ${pin} ${level}`),
            `Set GPIO ${port}.${pin} to ${level}`,
            errorCodeName,
            'E'
        );
    }

    /**
     * Read GPIO pin value
     * @param {string} port - GPIO port (a-k)
     * @param {number} pin - Pin number
     * @param {string} errorCodeName - Error code key
     * @returns {number|false} - Pin value or false if failed
     */
    async readLevel(port, pin, errorCodeName = null) {
        const ret = await this.cmdHelper.execute(
            () => this.targetICTLink.sendCommand(`getgpio ${port} ${pin}`),
            `Read GPIO ${port}.${pin}`,
            errorCodeName,
            'T'
        );
        return ret ? ret.value : false;
    }

    /**
     * Configure and set GPIO to specific level (atomic operation)
     * @param {Object} pin - Pin config {port, pin, pinNameOnTestBoard}
     * @param {number} level - Level to set
     * @returns {boolean} - Success
     */
    async configureAndSet(pin, level) {
        // Configure
        if (!(await this.configureOutput(pin.port, pin.pin, pin.pinNameOnTestBoard))) {
            return false;
        }

        // Set level
        return await this.setLevel(pin.port, pin.pin, level, pin.pinNameOnTestBoard);
    }

    /**
     * Configure multiple pins to input mode
     * @param {Array} pins - Array of pin objects
     * @returns {boolean} - All succeeded
     */
    async configureMultipleInput(pins) {
        for (const pin of pins) {
            const ret = await this.targetICTLink.sendCommand(
                `confgpio ${pin.port} ${pin.pin} input none`
            );
            if (!ret.status) {
                this.logger.error(
                    `Failed to configure ${pin.port}.${pin.pin} as input: ${ret.error}`
                );
                return false;
            }
        }
        return true;
    }
}

module.exports = GPIOHelper;

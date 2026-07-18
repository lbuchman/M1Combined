'use strict';

const fs = require('fs-extra');
const _ = require('lodash');

const inConfig = '/var/snap/m1tfc/current/config.json';
const outConfig = inConfig;
const inCalibration = '/var/snap/m1tfc/current/calibration.json';
const outCalibration = inCalibration;

/* Additional parameters the user can specify in $SNAP_DATA/config.json
{
    skipTestpointCheck,
    memTestSize1MBBlocks,
    forceEppromOverwrite,
    vendorSite
};
*/

async function getConfig(configDataDefaults) {
    let configDataUser;
    let calibrationData;
    try {
        configDataUser = await fs.readJSON(inConfig, 'utf8');
    } catch (err) {
        configDataUser = {};
    }

    try {
        calibrationData = await fs.readJSON(inCalibration, 'utf8');
    } catch (err) {
        calibrationData = {};
    }

    return _.merge(configDataDefaults, configDataUser, calibrationData);
}

async function saveConfig(configData) {
    try {
        const configDataRuntime = _.omit(configData, ['boards']);
        await fs.writeJSON(outConfig, configDataRuntime, { spaces: 2 });
    } catch (err) {
        //
    }
}

async function getCalibration() {
    try {
        return await fs.readJSON(inCalibration, 'utf8');
    } catch (err) {
        const configData = await getConfig({});
        return configData.boards ? { boards: configData.boards } : {};
    }
}

async function saveCalibration(calibrationData) {
    try {
        await fs.writeJSON(outCalibration, calibrationData, { spaces: 2 });
    } catch (err) {
        //
    }
}

module.exports = {
    getConfig,
    saveConfig,
    getCalibration,
    saveCalibration
};

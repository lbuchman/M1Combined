'use strict';

const fs = require('fs-extra');
const _ = require('lodash');

const inConfig = '/var/snap/m1tfd1/current/config.json';
const outConfig = "/home/lenel/config.json"

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
    try {
        configDataUser = await fs.readJSON(inConfig, 'utf8');
    }
    catch (err) {
        configDataUser = {};
    }

    const ret = _.merge(configDataDefaults, configDataUser);
    process.env.productName = ret.productName;
    process.env.coinCellDebug = ret.coinCellDebug;
    process.env.skipBatteryTest = ret.skipBatteryTest;

    return ret;
}

async function saveConfig(configData) {
    let configDataUser;
    try {
        await fs.writeJSON(outConfig, configData, { spaces: 2 });
    }
    catch (err) {
        configDataUser = {};
    }
}

module.exports = {
    getConfig,
    saveConfig
}

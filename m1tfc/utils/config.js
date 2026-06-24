'use strict';

const fs = require('fs-extra');
const _ = require('lodash');

const inConfig = '/var/snap/m1tfd1/current/config.json';
const outConfig = inConfig;

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

    return _.merge(configDataDefaults, configDataUser);
}

async function saveConfig(configData) {
    try {
        await fs.writeJSON(outConfig, configData, { spaces: 2 });
    }
    catch (err) {
        //
    }
}

module.exports = {
    getConfig,
    saveConfig
};

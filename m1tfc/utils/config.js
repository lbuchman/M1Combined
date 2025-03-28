'use strict';

const fs = require('fs-extra');
const _ = require('lodash');

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
        configDataUser = await fs.readJSON('/var/snap/m1tfd1/current/config.json', 'utf8');
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

module.exports = getConfig;

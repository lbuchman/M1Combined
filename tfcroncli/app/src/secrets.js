'use strict';

const Database = require('better-sqlite3');
const fs = require('fs');

/**
* @class
* wrapper around sqlite3 sync lib
*
*/
class DBClass {
    constructor(options, log) {
        this.log = log;

        try {
            fs.statSync(options.dbPath); // check if DB file exists
        }
        catch (error) {
            throw new Error('database file does not exist');
        }

        try { // optimize sqlite3 settings for speed
            this.db = new Database(options.dbPath, { readonly: false });
            this.db.pragma('cache_size = 128000');
            this.db.pragma('journal_mode = MEMORY');
            this.db.pragma('temp_store = MEMORY');
            this.db.pragma('synchronous  = off');
            this.db.pragma('auto_vacuum = FULL');
        }
        catch (err) {
            throw new Error(err.message);
        }
    }

    /**
    * @public
    *
    *
    * @param {string} correlationId
    */
    getRecords() {
        if (!this.db) throw new Error('DB file is not open, cannot get next MAC');
        const select = this.db.prepare('SELECT *  FROM records  where downloadedCumulus = 0');
        const retValue = select.all();
        return retValue;
    }

    /**
    * @public
    *
    *
    * @param {string} correlationId
    */
    updateRecord(serial) {
        if (!this.db) throw new Error('DB file is not open');
        const update = this.db.prepare('UPDATE records set downloadedCumulus = 1 WHERE vendorSerial = ?');
        update.run(serial);
    }
}

function initialize(m1mtfDir, log) {
    const options = {
        dbPath: `${m1mtfDir}/tf.db`
    };
    return new DBClass(options, log);
}

module.exports = {
    initialize
};

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
        let stat;
        try {
            stat = fs.statSync(options.dbPath); // check if DB file exists
        }
        catch (error) {
            this.copyTemplateDB(options.templateDB, options.dbPath); // copy template file
        }

        if (stat !== undefined && stat.size < 1024) {
            this.copyTemplateDB(options.templateDB, options.dbPath); // make sure that DB file is not ZERO file
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
    * copy file
    * @param {string} templateDB
    * @param {string} dbPath
    */
    copyTemplateDB(templateDB, dbPath) {
        try {
            fs.copyFileSync(templateDB, dbPath);
        }
        catch (err) {
            this.log.error(`cannot copy ${templateDB} ${err.message} file, database cannot be open`);
        }
    }

    /**
    * @public
    * add event to the DB
    * @param {object} event
    * @param {string} correlationId
    */
    getRecord(serial) {
        if (!this.db) { this.log.error('DB is closed'); return false; }
        if (!serial) {
            throw new Error('cannot add board to DB, serial = NULL');
        }

        const select = this.db.prepare('Select * from records  where vendorSerial = ?');
        const ret = select.all(serial);
        return ret;
    }

    /**
    * @public
    *
    */
    updateLastUsedMac(mac) {
        if (!this.db) throw new Error('DB file is not open, cannot get next MAC');
        const insert = this.db.prepare('INSERT INTO uid (uid) VALUES (?)');
        insert.run(mac);
    }

    /**
    * @public
    *
    */
    getRecordFromMac(mac) {
        if (!this.db) throw new Error('DB file is not open, cannot get next MAC');
        const select = this.db.prepare('SELECT *  FROM records where  uid = ?');
        const retValue = select.all(mac);
        return retValue;
    }

    /**
    * @public
    *
    */
    getLastUsedMac() {
        if (!this.db) throw new Error('DB file is not open, cannot get next MAC');
        const select = this.db.prepare('SELECT uid FROM uid ORDER BY ROWID DESC');
        const retValue = select.all();
        if (!retValue.length) throw new Error('DB is not initialized with MAC start range');
        return retValue[0].uid;
    }

    /**
    * @public
    *
    */
    getUid(serial) {
        if (!this.db) throw new Error('DB file is not open, cannot get next MAC');
        const select = this.db.prepare('SELECT uid  FROM records  where vendorSerial = ?');
        const retValue = select.all(serial);
        if (retValue && retValue[0] && retValue[0].uid) return retValue[0].uid;
        throw new Error(`DB does not have MAC set for serial ${serial}`);
    }

    /**
    * @public
    *
    */
    updateUid(serial, uid) {
        try {
            if (!this.db) throw new Error('DB file is not open');
            const update = this.db.prepare('UPDATE records set uid=? WHERE vendorSerial = ?');
            let ret = update.run(uid, serial);

            if (ret.changes === 0) {
                const insert = this.db.prepare('INSERT INTO records (uid, vendorSerial) VALUES (?,?)');
                ret = insert.run(uid, serial);
                if (ret) {
                    throw new Error('DB call to update uid failed');
                }
            }
        }
        catch (err) {
            throw new Error(`cannot update uid in DB: ${err.message}`);
        }
    }

    /**
     * @public
     *
     */
    deleteRecord(serial) {
        try {
            this.log.debug(`deleting record with Serial ${serial} in the DB`);
            if (!this.db) throw new Error('DB file is not open');
            const deleteRecord = this.db.prepare('Delete FROM records where vendorSerial = ?');
            const ret = deleteRecord.run(serial);
            if (ret.changes === 0) {
                throw new Error('DB call to delete record failed');
            }
        }
        catch (err) {
            this.log.debug(`${err.message}`);
        }
    }

    /**
    * @public
    *
    */
    updateSerial(serial) {
        try {
            this.log.debug(`adding Serial ${serial} to the DB`);
            if (!this.db) throw new Error('DB file is not open');
            const insert = this.db.prepare('INSERT INTO records (vendorSerial,downloadedCumulus, ictTestPassed, functionalTestPassed, flashProgrammed, cloudPushed) VALUES (?,?,?,?,?,?)');
            const ret = insert.run(serial, 0, 0, 0, 0, 0);
            if (ret.changes === 0) {
                throw new Error('DB call to update vendorSerial failed');
            }
        }
        catch (err) {
            this.log.debug(`${err.message}`);
        }
    }

    /**
    * @public
    *
    */
    updateCPUSerial(serial, cpuSN) {
        this.log.debug(`adding CPU Serial ${serial} to the DB`);
        if (!this.db) throw new Error('DB file is not open');

        const select = this.db.prepare('SELECT cpuSerial  FROM records  where vendorSerial = ?');
        const retValue = select.all(serial);
        if (retValue[0].cpuSerial) {
            if (retValue[0].cpuSerial !== cpuSN) throw Error('Duplicate Board Barcode');
        }
        const update = this.db.prepare('UPDATE records set cpuSerial = ? WHERE vendorSerial = ?');
        const ret = update.run(cpuSN, serial);
        if (ret.changes === 0) {
            throw new Error('DB call to update cpuSN failed');
        }
    }

    /**
    * @public
    *
    */
    updateIctStatus(serial, status) {
        try {
            if (!this.db) throw new Error('DB file is not open');
            const update = this.db.prepare('UPDATE records set ictTestPassed = ? WHERE vendorSerial = ?');
            const ret = update.run(status, serial);
            if (ret.changes === 0) {
                throw new Error('DB call to update ictTestPassed Status failed');
            }
        }
        catch (err) {
            throw new Error(`cannot ICTPassed Status in DB: ${err.message}`);
        }
    }

    /**
    * @public
    *
    */
    updateFuncTestStatus(serial, status) {
        try {
            if (!this.db) throw new Error('DB file is not open');
            const update = this.db.prepare('UPDATE records set functionalTestPassed = ? WHERE vendorSerial = ?');
            const ret = update.run(status, serial);
            if (ret.changes === 0) {
                throw new Error('DB call to update functionalTestPassed Status failed');
            }
        }
        catch (err) {
            throw new Error(`cannot update ICT Status in DB: ${err.message}`);
        }
    }

    /**
    * @public
    *
    */
    updateFlashStatus(serial, status) {
        try {
            if (!this.db) throw new Error('DB file is not open');
            const update = this.db.prepare('UPDATE records set flashProgrammed=? WHERE vendorSerial = ?');
            const ret = update.run(status, serial);
            if (ret.changes === 0) {
                throw new Error('DB call to update Flash Status failed');
            }
        }
        catch (err) {
            throw new Error(`cannot update Flash Status in DB: ${err.message}`);
        }
    }

    /**
    * @public
    *
    */
    updateEepromData(serial, secret, boardS2Serial) {
        if (!this.db) throw new Error('DB file is not open');
        const update = this.db.prepare('UPDATE records set secret=?,boardS2Serial=?,dateAndTime=? WHERE vendorSerial = ?');
        const date = new Date();
        const ret = update.run(secret, boardS2Serial, date.toISOString(), serial);
        if (ret.changes === 0) {
            throw new Error('DB call to update EEPROM Data failed');
        }
    }

    /**
   * @public
   *
   */
    // eslint-disable-next-line class-methods-use-this
    updatelog(/* serial, data */) {
        /*
        if (!this.db) throw new Error('DB file is not open');
        const update = this.db.prepare('UPDATE records set log=? WHERE vendorSerial = ?');
        const ret = update.run(data, serial);
        if (ret.changes === 0) {
            throw new Error('DB call to update EEPROM Data failed');
        }
        */
    }

    /**
    * @public
    *
    */
    close() {
        if (!this.db) {
            this.log.error('DB is already closed');
            return;
        }
        this.log.debug('Database is Closed');
        this.db.close();
        this.db = null;
    }
}

function initialize(log) {
    const options = {
        dbPath: `${process.env.DBPATH}/tf.db`,
        templateDB: `${__dirname}/../template/template.db`
    };

    return new DBClass(options, log);
}

module.exports = {
    initialize
};

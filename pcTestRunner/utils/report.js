'use strict';

const fs = require('fs-extra');
// const _ = require('lodash');
const timestamp = require('time-stamp');
const sprintf = require('sprintf-js').sprintf;

// https://medium.com/@thejasonfile/creating-templates-with-handlebars-js-15c2fa45859
module.exports = class Report {
    constructor(filename, log) {
        this.filename = filename;
        fs.ensureFileSync(filename);
        this.logger = log;
    }

    /**
      * @public
      *
      * @param {object} log
      */
    reportRibbonCableGroup1TestResults(testResults) {
        const timeStamp = timestamp('YYYY/MM/DD HH:mm:ss');
        const testResultsSorted = testResults.sort((a, b) => {
            if (a.pinNumber > b.pinNumber) return 1;
            if (a.pinNumber < b.pinNumber) return -1;
            return 0;
        });
        let ret = true;
        testResultsSorted.forEach((test) => {
            const str = sprintf('\tRibbon Cable Pin\t%2d\t%7s\tVoltage\tvalue\t%6.2fV\tLimits[%6.2fV/%6.2fV]\t%s', test.pinNumber, test.pinDesc, test.value, test.minValue, test.maxValue, test.valueValid ? 'Passed' : 'Failed - out of range');
            this.logger.info(str);
            fs.appendFileSync(this.filename, `[${timeStamp}]  ${str}\n`);
            if (!test.valueValid) ret = false;
        });
        return ret;
    }
};

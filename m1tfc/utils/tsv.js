'use strict';

const fs = require('fs');
const path = require('path');

function makeMinimalTsv(tsv) {
    const outFile = `${path.dirname(tsv)}/minimal.tsv`;
    const allFileContents = fs.readFileSync(tsv, 'utf-8');
    try {
        fs.unlinkSync(outFile);
    }
    catch (err) {
        //
    }
    allFileContents.split(/\r?\n/).forEach((line, index) => {
        if (index === 0) {
            fs.writeFileSync(outFile, `${line}\n`);
            return;
        }
        if (index > 2) {
            return;
        }
        fs.writeFileSync(`${outFile}`, `${line}\n`, { flag: 'a+' });
    });
    return outFile;
}


module.exports = {
    makeMinimalTsv
};

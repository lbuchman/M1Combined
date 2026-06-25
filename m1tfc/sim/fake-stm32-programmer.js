#!/usr/bin/env node

'use strict';

const args = process.argv.slice(2);
const joined = ` ${args.join(' ')} `;

function has(str) {
    return joined.includes(str);
}

function getWordArg() {
    const match = joined.match(/word=(\d+)/);
    if (!match) {
        return null;
    }
    return Number(match[1]);
}

function outputOtpDisplay(word) {
    // Keep these lines compatible with parsing in utils/getWordData and getCPUSerial.
    console.log('SN          : SIMCPU0001');

    switch (word) {
    case 57:
        console.log('Data57  0x33221100');
        break;
    case 58:
        console.log('Data58  0x00005544');
        break;
    case 60:
        console.log('Data60  0x00000000');
        break;
    case 61:
        console.log('Data61  0x00000000');
        break;
    case 62:
        console.log('Data62  0x00000000');
        break;
    case 63:
        console.log('Data63  0x00000000');
        break;
    default:
        console.log(`Data${word || 0}  0x00000000`);
        break;
    }
}

function main() {
    if (has(' -l ')) {
        console.log('Device Index           : USB1');
        console.log('DFU in HS Mode');
        process.exit(0);
    }

    if (has(' -otp ') && has(' displ ')) {
        outputOtpDisplay(getWordArg());
        process.exit(0);
    }

    if (has(' -d ')) {
        console.log('Download in Progress...');
        console.log('Download verified successfully');
        process.exit(0);
    }

    if (has(' -otp ') && has(' write ')) {
        console.log('OTP write complete');
        process.exit(0);
    }

    console.log('STM32_Programmer_CLI simulation: command accepted');
    process.exit(0);
}

main();

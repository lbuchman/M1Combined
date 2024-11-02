'use strict';

const mnpIOMap = [
    // Rd1 RS485
    { port: 'B', pin: 5, mode: 'output', name: 'Rd1_RS485_1_RX', testPoint: 'TP2003' },
    { port: 'B', pin: 6, mode: 'output', name: 'Rd1_RS485_1_TX', testPoint: 'TP2004' },
    { port: 'C', pin: 8, mode: 'output', name: 'Rd1_RS485_1_DE', testPoint: 'TP2005' },
    { port: 'B', pin: 12, mode: 'output', name: 'Rd1_RS485_1_TE', testPoint: 'na' },
    // WGD1
    { port: 'F', pin: 9, mode: 'input', name: 'WGD1_D0_3V3', testPoint: 'TP2001' },
    { port: 'K', pin: 3, mode: 'input', name: 'WGD1_D1_3V3', testPoint: 'TP2002' },
    { port: 'H', pin: 11, mode: 'output', name: 'WGD1_BPR', testPoint: 'na' },
    // WGD1
    { port: 'C', pin: 2, mode: 'input', name: 'WGD2_D0_3V3', testPoint: 'na' },
    { port: 'J', pin: 15, mode: 'input', name: 'WGD2_D1_3V3', testPoint: 'na' },
    { port: 'J', pin: 12, mode: 'output', name: 'WGD2_BPR', testPoint: 'na' },
    // Rd2 RS485
    { port: 'E', pin: 0, mode: 'output', name: 'Rd2_RS485_2_RX', testPoint: 'TP2103' },
    { port: 'E', pin: 1, mode: 'output', name: 'Rd2_RS485_2_TX', testPoint: 'TP2104' },
    { port: 'E', pin: 14, mode: 'output', name: 'Rd2_RS485_2_DE', testPoint: 'TP2105' },
    { port: 'E', pin: 12, mode: 'output', name: 'Rd2_RS485_2_TE', testPoint: 'na' },
    // Latch Open drain outputs
    { port: 'F', pin: 10, mode: 'output', name: 'LADR0', testPoint: 'na' },
    { port: 'G', pin: 7, mode: 'output', name: 'LADR1', testPoint: 'na' },
    { port: 'K', pin: 5, mode: 'output', name: 'LADR1', testPoint: 'na' },
    { port: 'J', pin: 14, mode: 'output', name: 'LDAT', testPoint: 'na' },
    { port: 'G', pin: 10, mode: 'output', name: 'LEN_L', testPoint: 'na' },
    // Strike boost PS control
    { port: 'B', pin: 8, mode: 'output', name: 'STRIKE1_KICKER_EN', testPoint: 'na' },
    { port: 'F', pin: 15, mode: 'output', name: 'STRIKE2_KICKER_EN', testPoint: 'na' },
    // Analog
    { port: 'A', pin: 0, mode: 'input', name: 'IN1_ADC', testPoint: 'TP1801' },
    { port: 'A', pin: 1, mode: 'input', name: 'IN2_ADC', testPoint: 'TP1802' },
    { port: 'F', pin: 11, mode: 'input', name: 'IN3_ADC', testPoint: 'TP1901' },
    { port: 'F', pin: 12, mode: 'input', name: 'IN4_ADC', testPoint: 'TP1902' },
    // Latch
    { port: '7', pin: -1, mode: 'latch', name: 'RD2_GLED', testPoint: 'na' },
    { port: '6', pin: -1, mode: 'latch', name: 'RD2_RLED', testPoint: 'na' },
    { port: '5', pin: -1, mode: 'latch', name: 'RD1_GLED', testPoint: 'na' },
    { port: '4', pin: -1, mode: 'latch', name: 'RD1_RLED', testPoint: 'na' },
    { port: '3', pin: -1, mode: 'latch', name: 'RLY4_EN', testPoint: 'na' },
    { port: '2', pin: -1, mode: 'latch', name: 'RLY3_EN', testPoint: 'na' },
    { port: '1', pin: -1, mode: 'latch', name: 'RLY2_EN', testPoint: 'na' },
    { port: '0', pin: -1, mode: 'latch', name: 'RLY1_EN', testPoint: 'na' },
    // POE
    { port: 'D', pin: 5, mode: 'input', name: 'nPoEP_PSE', testPoint: 'na' },
    { port: 'K', pin: 0, mode: 'input', name: 'nPoEP_PSE', testPoint: 'na' },
];

function getCommand(action, signalNameOrTespoint, value, log) {
    if (action === 'printio') {
        mnpIOMap.forEach((item) => {
            log.info(JSON.stringify(item));
        })
        throw new Error('No Error');
    }
    let output = mnpIOMap.filter((value) => {
        return value.name == signalNameOrTespoint;

    });

    if (!output.length) {
        output = mnpIOMap.filter((value) => {
            return value.testPoint == signalNameOrTespoint;
        });
    }
    if (!output.length) throw new Error('no such test point or signal');
    switch (output[0].mode) {
        case 'input':
            if (action !== 'read') throw new Error('invalid action for IO line');
            return `getgpio ${output[0].port} ${output[0].pin}`
        case 'output':
            if (action !== 'write') throw new Error('invalid action for IO line');
            return `setgpio ${output[0].port} ${output[0].pin} ${value}`
        case 'latch':
            if (action !== 'write') throw new Error('invalid action for IO line');
            return `latchctl ${output[0].port}  ${value}`
    }
    return '';
}

module.exports = {
    getCommand
};

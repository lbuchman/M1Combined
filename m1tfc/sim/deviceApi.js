'use strict';

const http = require('http');

function ok(extra = {}) {
    return { status: true, error: null, ...extra };
}

function fail(error) {
    return { status: false, error: String(error || 'unknown error') };
}

function parseSerialCommand(raw) {
    const text = String(raw || '').trim();
    const parts = text.split(/\s+/).filter(Boolean);
    const cmd = parts[0] || '';
    const arg = parts.length > 1 ? parts.slice(1) : [];
    return { cmd, arg };
}

function createSharedFixtureState() {
    const gpioState = new Map();
    const ioDef = [
        { pinId: 1, pinName: 'M1Boot1', group: 0, reqValue: 0 },
        { pinId: 2, pinName: 'J5.18', group: 0, reqValue: 0 },
        { pinId: 3, pinName: 'J5.15', group: 0, reqValue: 0 },
        { pinId: 4, pinName: 'J5.16', group: 0, reqValue: 0 },
        { pinId: 5, pinName: 'J5.19', group: 0, reqValue: 0 },
        { pinId: 6, pinName: 'J5.17', group: 0, reqValue: 0 },
        { pinId: 7, pinName: 'J5.6', group: 0, reqValue: 0 },
        { pinId: 8, pinName: 'J5.3', group: 0, reqValue: 0 }
    ];

    const pinById = new Map(ioDef.map(pin => [String(pin.pinId), pin]));
    const pinNameToGpio = {
        'J5.18': 'd.5',
        'J5.15': 'c.6',
        'J5.16': 'd.4',
        'J5.19': 'c.10',
        'J5.17': 'k.0',
        'J5.6': 'g.15',
        'J5.3': 'd.7'
    };

    return {
        ioDef,
        pinById,
        pinNameToGpio,
        gpioState
    };
}

function createFixtureApi(state) {
    return {
        async handle(request) {
            const { cmd, arg } = request;

            if (cmd === 'iodef') {
                return state.ioDef;
            }

            if (
                cmd === 'targetpoweron' ||
                cmd === 'targetpoweroff' ||
                cmd === 'batteryon' ||
                cmd === 'batteryoff'
            ) {
                return ok();
            }

            if (cmd === 'setiopin') {
                return ok();
            }

            if (cmd === 'getiopin') {
                const pinId = String(arg[0]);
                const pin = state.pinById.get(pinId);
                if (!pin) {
                    return fail(`unknown pinId ${pinId}`);
                }
                const gpioKey = state.pinNameToGpio[pin.pinName];
                const level = gpioKey ? state.gpioState.get(gpioKey) || 0 : 0;
                return ok({ value: level ? 3.3 : 0.0 });
            }

            return ok();
        }
    };
}

function createUutIctApi(state) {
    return {
        async handle(request) {
            const { cmd, arg } = request;
            if (cmd === 'confgpio') {
                const key = `${arg[0]}.${arg[1]}`;
                if (!state.gpioState.has(key)) {
                    state.gpioState.set(key, 0);
                }
                return ok();
            }

            if (cmd === 'setgpio') {
                const key = `${arg[0]}.${arg[1]}`;
                state.gpioState.set(key, Number(arg[2]));
                return ok();
            }

            if (cmd === 'testrs422') {
                return ok();
            }

            if (cmd === 'getfwrev') {
                return ok({ fwrev: 'SIM-ICT-1.0' });
            }

            return ok();
        }
    };
}

function createTeensyRestApi() {
    return {
        async handle(request) {
            const { cmd } = request;
            if (!cmd) {
                return fail('missing cmd');
            }
            return ok({ data: { cmd } });
        }
    };
}

function startRestApiServer(host, port, api, logger = console) {
    const server = http.createServer(async(req, res) => {
        if (req.method === 'GET' && req.url === '/health') {
            res.writeHead(200, { 'Content-Type': 'application/json' });
            res.end(JSON.stringify(ok({ service: 'teensy2-rest-sim' })));
            return;
        }

        if (req.method !== 'POST' || req.url !== '/command') {
            res.writeHead(404, { 'Content-Type': 'application/json' });
            res.end(JSON.stringify(fail('route not found')));
            return;
        }

        let body = '';
        req.on('data', chunk => {
            body += chunk.toString();
        });

        req.on('end', async() => {
            try {
                const parsed = body ? JSON.parse(body) : {};
                const response = await api.handle(parsed);
                res.writeHead(200, { 'Content-Type': 'application/json' });
                res.end(JSON.stringify(response));
            } catch (err) {
                res.writeHead(500, { 'Content-Type': 'application/json' });
                res.end(JSON.stringify(fail(err.message)));
            }
        });
    });

    return new Promise((resolve, reject) => {
        server.on('error', reject);
        server.listen(port, host, () => {
            logger.info(`REST teensy simulator listening on http://${host}:${port}`);
            resolve(server);
        });
    });
}

module.exports = {
    parseSerialCommand,
    createSharedFixtureState,
    createFixtureApi,
    createUutIctApi,
    createTeensyRestApi,
    startRestApiServer
};

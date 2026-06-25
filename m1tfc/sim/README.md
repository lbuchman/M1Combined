# Serial Hardware Simulation (Old-School E2E)

This folder contains bare-bone simulators for running fixture-style tests without real hardware.

## What is simulated

- `fake-stm32-programmer.js`:
  - Drop-in CLI replacement for `STM32_Programmer_CLI`.
  - Supports DFU list (`-l`), image download (`-d`), OTP display, OTP write.

- `stm32mp-barebone-term-sim.js`:
  - Serial terminal simulator for STM32MP target in bare-bone state.
  - Starts in `U-Boot>` mode (no Linux).
  - Can transition to Linux/login prompt when boot command is sent.

- `teensy2-rest-sim.js`:
  - REST simulator for the second Teensy board.
  - Endpoint: `POST /command`
  - Contract: request `{ "cmd": "...", "arg": [...] }`, response `{ "status": true|false, "error": "..." }`

- `deviceApi.js`:
  - Shared command API layer used by serial and REST simulators.
  - Normalizes legacy serial commands to `{cmd, arg}` and returns JSON status objects.

- `run-ribbon-e2e.js`:
  - End-to-end serial simulation for ribbon-cable test path.
  - Creates fake PTY links using `socat`.

## Prerequisites

- Linux host
- `socat` installed and available in PATH
- Node dependencies installed

## Fake STM32 programmer usage

Use this as `programmingCommand` value in your config/run script:

```bash
node ./sim/fake-stm32-programmer.js
```

Quick checks:

```bash
node ./sim/fake-stm32-programmer.js -l
node ./sim/fake-stm32-programmer.js -c port=usb1 -otp displ word=57
node ./sim/fake-stm32-programmer.js -c port=usb1 -d fw/fsbl.stm32
```

## Bare-bone STM32MP terminal usage

Run against the simulator side of a PTY pair:

```bash
node ./sim/stm32mp-barebone-term-sim.js --port /tmp/m1tfc-sim-1234/uut-term.sim --baud 115200
```

## Second Teensy REST usage

```bash
node ./sim/teensy2-rest-sim.js --host 127.0.0.1 --port 18081
curl -s -X POST http://127.0.0.1:18081/command \
  -H 'content-type: application/json' \
  -d '{"cmd":"setrelay","arg":["K1",1]}'
```

Behavior summary:

- On start: prints `U-Boot>`
- `boot` or `run bootcmd`: transitions to Linux and emits `login:`
- `reset`: returns to `U-Boot>`
- `halt`: returns to `U-Boot>`

## Notes

- The fake programmer output intentionally matches parsing expectations used by OTP helpers.
- For full ICT/functional E2E, pair these with test-board/UUT serial simulators and DB sandbox paths.

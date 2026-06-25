# m1tfc API Reference

## CLI Commands

All commands share common options:

| Option                  | Description                               |
| ----------------------- | ----------------------------------------- |
| `-s, --serial <string>` | Vendor board serial number                |
| `-d, --debug <level>`   | Log level: `0` error, `1` info, `2` debug |

---

### `ict`

Executes ICT (In-Circuit Test).

```bash
m1tfc ict -s <serial> -d <level> [-b <cellBatTol>]
```

| Option                 | Description                                  |
| ---------------------- | -------------------------------------------- |
| `-b, --cellBatTol`     | Coin cell tolerance: `new` or `used`         |
| `-c, --callibrate`     | Calibrate A/D and save to config             |
| `-v, --cellBatVoltage` | Measured cell bat voltage (calibration only) |

---

### `flash`

Programs STM32MP1 firmware via STM32CubeProgrammer.

```bash
m1tfc flash -s <serial> -d <level>
```

---

### `progmac`

Programs MAC address into EEPROM.

```bash
m1tfc progmac -s <serial> -d <level>
```

---

### `eeprom`

Programs EEPROM configuration data.

```bash
m1tfc eeprom -s <serial> -d <level>
```

---

### `functest`

Executes functional test suite.

```bash
m1tfc functest -s <serial> -d <level>
```

---

### `pingM1apps`

Pings M1 application ports to verify services are running.

```bash
m1tfc pingM1apps -s <serial> -d <level>
```

---

### `makelabel`

Prints a label for the board using the configured label printer.

```bash
m1tfc makelabel -s <serial> -d <level>
```

---

### `cleanup`

Cleans up temporary files and state after a test run.

```bash
m1tfc cleanup -s <serial>
```

---

## Internal Modules

### `bin/commandSupport.js`

| Export                            | Description                                     |
| --------------------------------- | ----------------------------------------------- |
| `ensureSnapEnv()`                 | Sets Snap env vars if not running inside a Snap |
| `errorAndExit(msg, log)`          | Log error and exit with code 3                  |
| `applyRuntime(configData, extra)` | Apply config to global runtime context          |
| `loadConfig()`                    | Load and merge config, apply runtime            |
| `defaultConfiguration`            | Default config object                           |

### `utils/errorHandler.js`

| Export                        | Description                                               |
| ----------------------------- | --------------------------------------------------------- |
| `wrapAction(fn, log)`         | Wraps async command action with error handling            |
| `registerGlobalHandlers(log)` | Registers uncaughtException / unhandledRejection handlers |

### `utils/runtimeContext.js`

| Export               | Description                            |
| -------------------- | -------------------------------------- |
| `setRuntime(values)` | Merge values into global runtime state |
| `getRuntime()`       | Returns current runtime state object   |

### `src/exitCodes.js`

| Code                       | Value | Meaning                   |
| -------------------------- | ----- | ------------------------- |
| `normalExit`               | 0     | Success                   |
| `commandFailed`            | 3     | General command failure   |
| `ictTestFailed`            | 4     | ICT test failed           |
| `precheckHWFailed`         | 15    | Hardware pre-check failed |
| _(see file for full list)_ |       |                           |

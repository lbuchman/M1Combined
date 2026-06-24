# gui-react

Standalone React replacement project for the Pascal GUI.

## Purpose

This app mimics the test panel UX from the Pascal GUI and drives the same command flow via the REST server endpoint.

- Commands: `ict`, `progmac`, `flash`, `functest`, `eeprom`, `pingM1apps`, `makelabel`
- Cleanup request after run: `cleanup`
- Serial/debug arguments are sent as JSON to `POST /command`

Sequence profiles in UI:

- `Pascal Legacy (main_backup)`:
  - Order: `ict`, `progmac`, `flash`, `functest`, `eeprom`, `pingM1apps`, `makelabel`
  - Re-test skips only `flash`
- `Pascal Core Factory`:
  - Order: `makelabel`, `eeprom`, `flash`, `functest`, `ict`, `progmac`, `pingM1apps`
  - Re-test skips `flash` and `pingM1apps`

## Run

```bash
npm install
npm run dev
```

Open the shown Vite URL (default `http://127.0.0.1:5173`).

## Backend

Set API endpoint in the UI (default `http://127.0.0.1:3300`).

Expected REST contract:

- `POST /command` body:

```json
{
  "command": "ict",
  "argument": {
    "serial": "3226120077",
    "debug": "1",
    "cellBatTol": "new"
  }
}
```

- `GET /health` and `GET /commands` optional checks.

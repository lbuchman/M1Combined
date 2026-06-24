# m1tfc REST Server

Standalone REST service that executes `m1tfc` commands in a separate process.

## Why this design

- Service is independent from `m1tfc` runtime.
- Commands execute one-at-a-time through an internal queue to avoid concurrent serial port access.
- Existing CLI stdout JSON is preserved in `commandOutput` and merged into the top-level response when the payload includes `status`, `errorCode`, or `ErrorDescription`.

## API

### Endpoints

| Method | Endpoint | Description |
| --- | --- | --- |
| GET | `/health` | Returns service health using the base response contract. |
| GET | `/commands` | Returns the list of supported `m1tfc` command names. |
| POST | `/command` | Runs one `m1tfc` command and returns the CLI JSON stdout when available. |

### POST /command

Request JSON:

```json
{
  "command": "ict",
  "argument": {
    "serial": "ABCD1234",
    "cellBatTol": "new",
    "debug": "1"
  }
}
```

Response JSON (required contract):

```json
{
  "status": "OK",
  "errorCode": 0,
  "ErrorDescription": "Success"
}
```

When command stdout already contains JSON, it is returned as `commandOutput` and its fields are exposed on the top-level response too:

```json
{
  "status": "OK",
  "errorCode": 0,
  "ErrorDescription": "Success",
  "commandOutput": {
    "result": "..."
  }
}
```

CLI commands currently routed through the server:

- `m1dfu`
- `m1tbcmd`
- `m1cmd`
- `mnpcmd`
- `ict`
- `eeprom`
- `progmac`
- `flash`
- `pingM1apps`
- `cleanup`
- `functest`
- `makelabel`

### GET /health
Returns service health using the same base contract fields.

### GET /commands
Returns supported `m1tfc` command names.

## Configuration

Environment variables:

- `PORT` (default `3300`)
- `HOST` (default `0.0.0.0`)
- `M1TFC_CMD` (default `m1tfc`)
- `M1TFC_BASE_ARGS` (default empty)
- `M1TFC_CWD` (default current working directory)

### Dev example with local repo

If the CLI is not installed globally:

```powershell
$env:M1TFC_CMD = "node"
$env:M1TFC_BASE_ARGS = "../m1tfc/bin/m1tfc.js"
$env:M1TFC_CWD = "../m1tfc"
npm start
```

## Run

```bash
npm install
npm start
```

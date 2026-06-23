# Refactoring Implementation Guide

## Status
Created skeleton files to support restructuring. Files are ready to use/enhance.

## Skeleton Files Created

### 1. `core/teststate.pas` ✅
**Purpose**: Centralize all test state in one object

**Key Classes**:
- `TTestState` - Manages current test mode, status, error codes, serial number, busy flag

**Usage Example**:
```pascal
FState := TTestState.Create;
FState.SetRunning;
FState.SerialNumber := targetVendorSerial.Text;
if FState.IsSuccess then ShowMessage('Test passed!');
```

**Replace in main.pas**:
- `testStatus` boolean
- `busyFlag` boolean
- `testRet` integer
- `testMode` enum

---

### 2. `core/exceptions.pas` ✅
**Purpose**: Structured exception hierarchy for consistent error handling

**Key Classes**:
- `TTestException` - Base class
- `TSerialValidationError`
- `TProcessExecutionError`
- `TConfigurationError`
- `THardwareError`
- `TUIError`
- `TTimeoutError`

**Usage Example**:
```pascal
try
  ValidateSerial(serial);
except
  on E: TSerialValidationError do
    ShowError('Invalid serial: ' + E.Message);
end;
```

---

### 3. `core/commandlinebuilder.pas` ✅
**Purpose**: Eliminate repeated array building for process arguments

**Key Classes**:
- `TCommandLineBuilder` - Constructs command-line arguments consistently

**Usage Example**:
```pascal
var Builder: TCommandLineBuilder;
var Args: TStringList;
begin
  Builder := TCommandLineBuilder.Create;
  Args := Builder.BuildTestCommand('ict', Serial, DebugLevel);
  RunProcess('m1tfc', Args);
  Args.Free;
end;
```

**Replace in main.pas**:
- All `arg[0..10]` array constructions
- Scattered command-line building logic

---

### 4. `core/testcommandfactory.pas` ✅
**Purpose**: Centralize test command metadata (name, progress, retry requirements)

**Key Classes**:
- `TTestCommandFactory` - Provides command info for all test types

**Usage Example**:
```pascal
var Factory: TTestCommandFactory;
begin
  Factory := TTestCommandFactory.Create;
  Command := Factory.GetCommand(tcICT);      // Returns 'ict'
  Progress := Factory.GetProgressValue(tcICT);   // Returns 5
  IsRetest := Factory.IsRetestRequired(tcICT);   // Returns True
end;
```

**Replace in main.pas**:
- Hardcoded progress values in Tests array
- Magic strings ('ict', 'progmac', 'flash', etc.)
- Return code constants

---

### 5. `core/itesthandler.pas` ✅
**Purpose**: Interface for pluggable test handlers

**Key Interfaces**:
- `ITestHandler` - Execute, get metadata, manage errors for a single test

**Why Important**:
- Replaces 6+ wrapper procedures with unified interface
- Each test is self-contained
- Easy to test in isolation
- Extensible for new tests

**Example Usage**:
```pascal
var Handler: ITestHandler;
begin
  Handler := TICTestHandler.Create(ICTTestSwitch);
  Handler.Execute(Serial, DebugLevel);
  if Handler.GetLastErrorCode <> 0 then
    ShowError(Handler.GetLastErrorMessage);
end;
```

---

### 6. `core/ictesthandler.pas` ✅
**Purpose**: Sample handler implementation showing the pattern

**Key Classes**:
- `TICTestHandler` - Implements ITestHandler for ICT test

**Study This To Create**:
- `core/mactesthandler.pas` (MAC programming)
- `core/flashtesthandler.pas` (Flash programming)
- `core/functesthandler.pas` (Functional test)
- `core/eepromtesthandler.pas` (EEPROM test)
- `core/dolabeltesthandler.pas` (Label programming)
- `core/appschecktesthandler.pas` (Apps check)

---

## Next Steps (Priority Order)

### Phase 1: Quick Cleanup (This Week)
1. ✅ Skeleton files created - review and understand structure
2. **[ ] Add constants file** - Extract all magic numbers to `core/constants.pas`
3. **[ ] Update `main.pas`** - Replace `testRet` int with `FState` object
4. **[ ] Fix error handling** - Wrap file I/O with try/except blocks

### Phase 2: Handler Migration (Next Week)
1. **[ ] Create handler implementations** - Flesh out the 6 handler classes
2. **[ ] Create `core/testhandlerregistry.pas`** - Manage handler collection
3. **[ ] Refactor `RunTests()`** - Use handler registry instead of array
4. **[ ] Remove wrapper procedures** - Delete the 6 `*_Wrapper()` methods

### Phase 3: Architecture (Week 3)
1. **[ ] Create `core/uicontroller.pas`** - Decouple form field access
2. **[ ] Create `core/processexecutor.pas`** - Wrap process execution
3. **[ ] Create `core/serialvalidator.pas`** - Centralize serial validation
4. **[ ] Refactor `main.pas` event handlers** - Slim down to 300 lines

---

## Quick Win: Add Constants File

Create `core/constants.pas`:

```pascal
unit constants;

{$mode objfpc}{$H+}

interface

const
  { Process exit codes }
  PROCESS_TERMINATED = -9;
  NORMAL_EXIT = 0;
  PROCESS_EXEC_ERROR = 1;
  PRECHECK_HW_FAILED = 15;
  OTP_NOT_BLANK = 10;
  EEPROM_NOT_BLANK = 11;

  { File paths }
  CONFIG_FILE = '/var/snap/m1tfd1/current/config.json';
  FW_TIMESTAMP_FILE = 'UpdateFwTimeStamp.txt';
  SECRETS_TIMESTAMP_FILE = 'UpdateSycretsTimeStamp.txt';
  LOGS_TIMESTAMP_FILE = 'UpdateLogsTimeStamp.txt';

  { Intervals }
  INTERVAL_7_DAYS = 24 * 60 * 60 * 7;
  PROCESS_TIMEOUT_MS = 30000;

  { Default values }
  DEFAULT_DATETIME = '2023-01-24 21:20:08';
  DEFAULT_DEBUG_LEVEL = '0';

implementation

end.
```

Then in `main.pas`:
```pascal
uses constants;

// Replace all:
if DebugLevel <> '1' Then DebugLevel := '0';
// With:
if DebugLevel <> '1' Then DebugLevel := DEFAULT_DEBUG_LEVEL;
```

---

## Testing Handlers Before Integration

Create a test unit to verify handler behavior:

```pascal
unit tests.handlers;

interface

uses
  fpcunit, testutils, testregistry, itesthandler, ictesthandler;

type
  TICTestHandlerTests = class(TTestCase)
  published
    procedure TestGetName;
    procedure TestGetProgressValue;
    procedure TestIsRetestRequired;
  end;

implementation

procedure TICTestHandlerTests.TestGetName;
var
  Handler: ITestHandler;
begin
  Handler := TICTestHandler.Create(nil);
  AssertEquals('In-Circuit Test (ICT)', Handler.GetName);
end;

{ ... more tests ... }

initialization
  RegisterTest(TICTestHandlerTests);
end.
```

---

## Measuring Progress

| Metric | Before | Target |
|--------|--------|--------|
| main.pas LOC | 1300+ | <400 |
| Duplicate code | 6 wrappers | 0 |
| Test handler classes | 0 | 7 |
| Exception types | 1 (generic) | 7 (specific) |
| Try/except blocks | <5 | >20 |
| Code coverage (theoretical) | ~40% | ~85% |

---

## File Organization After Full Refactor

```
gui/
├── DESIGN_REVIEW.md ................... Architecture document (this file)
├── REFACTOR_GUIDE.md ................. Implementation guide (this file)
├── main.pas ........................... TmainForm (LEAN - event handlers only)
├── gui.lpi, gui.lpr, gui.lfm
├── core/
│   ├── state/
│   │   └── teststate.pas ............. State management (✅ CREATED)
│   ├── exceptions.pas ................ Exception hierarchy (✅ CREATED)
│   ├── constants.pas ................. Constants (TODO)
│   ├── commandlinebuilder.pas ........ Command building (✅ CREATED)
│   ├── testcommandfactory.pas ........ Test metadata (✅ CREATED)
│   ├── processexecutor.pas ........... Process wrapper (TODO)
│   ├── serialvalidator.pas ........... Serial validation (TODO)
│   ├── uicontroller.pas .............. UI coordination (TODO)
│   ├── interfaces/
│   │   └── itesthandler.pas .......... Handler interface (✅ CREATED)
│   ├── testhandlerregistry.pas ....... Handler collection (TODO)
│   ├── testrunner.pas ................ Test runner (EXISTING - IMPROVE)
│   ├── testexecution.pas ............. Test executor (EXISTING - ENHANCE)
│   ├── ledmanager.pas ................ LED management (EXISTING)
│   └── cloudops.pas .................. Cloud operations (EXISTING)
├── handlers/ ......................... Test implementations (TODO)
│   ├── ictesthandler.pas ............. ICT handler (✅ CREATED)
│   ├── mactesthandler.pas ............ MAC handler (TODO)
│   ├── flashtesthandler.pas .......... Flash handler (TODO)
│   ├── functesthandler.pas ........... Func test handler (TODO)
│   ├── eepromtesthandler.pas ......... EEPROM handler (TODO)
│   ├── dolabeltesthandler.pas ........ Label handler (TODO)
│   └── appschecktesthandler.pas ...... Apps check handler (TODO)
├── ui/
│   ├── logform.pas ................... Log form (EXISTING)
│   ├── about.pas ..................... About form (EXISTING)
│   └── errorreportform.pas ........... Error form (EXISTING)
├── utils/
│   ├── logger.pas .................... Logging (EXISTING)
│   └── configurationjson.pas ......... Config (EXISTING)
└── tests/
    ├── tests.handlers.pas ............ Handler unit tests (TODO)
    └── tests.state.pas ............... State unit tests (TODO)
```

---

## Validation Checklist

After each phase, verify:

- [ ] All Pascal files compile without errors
- [ ] Existing functionality still works
- [ ] No new compiler warnings introduced
- [ ] Exception hierarchy properly linked
- [ ] File organization matches structure above
- [ ] Handler interface is implemented consistently
- [ ] State management is centralized
- [ ] Tests can run end-to-end

---

## Notes for Developers

1. **Start Small**: Begin with Phase 1 quick wins
2. **Test as You Go**: Compile and verify after each change
3. **Keep Backup**: Git commit frequently
4. **Document Changes**: Update this guide with findings
5. **Reuse Skeletons**: Use created files as templates

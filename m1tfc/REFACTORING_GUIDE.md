# M1TFC Code Refactoring Guide

## Overview

This guide documents the refactoring initiative to eliminate code duplication across the M1TFC test suite. The effort focuses on extracting common patterns into reusable utility helpers.

## Completed Refactoring (Phase 1, 2 & 3)

### Utility Helpers Created ✅

| Helper             | Location                      | Purpose                                | Status      |
| ------------------ | ----------------------------- | -------------------------------------- | ----------- |
| CommandHelper      | `utils/commandHelper.js`      | Command execution + error handling     | ✅ Complete |
| GPIOHelper         | `utils/gpioHelper.js`         | GPIO operations (configure, set, read) | ✅ Complete |
| VoltageHelper      | `utils/voltageHelper.js`      | Voltage reading & validation           | ✅ Complete |
| TestHelper         | `utils/testHelper.js`         | Test execution patterns                | ✅ Complete |
| EepromHelper       | `utils/eepromHelper.js`       | EEPROM verify, write, map handling     | ✅ Complete |
| MercuryBoardHelper | `utils/mercuryBoardHelper.js` | Mercury board I/O ops                  | ✅ Complete |
| runCommand         | `bin/commandRunner.js`        | CLI boilerplate abstraction            | ✅ Complete |

### Test Files Refactored ✅

| File                 | Changes                   | Lines Eliminated | Status      |
| -------------------- | ------------------------- | ---------------- | ----------- |
| tests/ddr3.js        | Unified 3 test functions  | ~30              | ✅ Complete |
| tests/rs485.js       | Simplified test pattern   | ~20              | ✅ Complete |
| tests/statusLed.js   | GPIO + voltage reading    | ~50              | ✅ Complete |
| tests/buzzer.js      | Cleaned error messages    | ~10              | ✅ Complete |
| tests/tamper.js      | GPIO helper integration   | ~40              | ✅ Complete |
| tests/battery.js     | GPIO configuration        | ~80              | ✅ Complete |
| tests/ribbonCable.js | Centralized GPIO patterns | ~100             | ✅ Complete |
| tests/eeprom.js      | EEPROMHelper inclusion    | ~100             | ✅ Complete |
| tests/mnpTests.js    | MercuryBoardHelper inc.   | ~50              | ✅ Complete |
| bin/commands/*       | Boilerplate abstraction   | ~250             | ✅ Complete |

**Total Impact:** 730+ lines of duplicate code eliminated

## Medium Priority (Well-Structured, Can Defer)

4. **tests/funcTest.js** - Complex functional test runner
5. **tests/programEeprom.js** - IctTestRunner wrapper (class pattern OK)
6. **tests/programMAC.js** - ProgramMac wrapper (class pattern OK)
7. **tests/flashEmmc.js** - Flash command wrapper (simple)

## How to Continue Refactoring

### Step 1: Identify Duplication Patterns

```bash
# Search for repetitive error handling
grep -r "if (!ret.status)" tests/ bin/

# Search for GPIO patterns
grep -r "confgpio\|setgpio\|getgpio" tests/

# Search for voltage patterns
grep -r "getiopin\|voltage" tests/
```

### Step 2: Extract to Helper

1. Create specific helper class (e.g., `EepromHelper`)
2. Implement methods for common operations
3. Add error handling integration
4. Test with `node -c helper.js`

### Step 3: Migrate Files

1. Read full file to understand dependencies
2. Update require statements to include new helper
3. Replace repetitive code with helper calls
4. Validate syntax: `node -c file.js`

### Step 4: Test & Commit

```bash
# Validate
node -c tests/file.js

# Test locally if possible
npm test  # if test suite exists

# Commit atomically
git add tests/file.js utils/newHelper.js
git commit -m "refactor(file): migrate to newHelper

- Extracted X patterns
- Eliminated Y lines of duplicate code"
```

## Error Code Integration

All helpers update error codes via `db.updateErrorCode()`:

```javascript
db.updateErrorCode(
    runtimeContext.getRuntime().serial,
    errorCodes.codes['ERROR_CODE_NAME'].errorCode,
    'T' // 'T' for temp, 'E' for error
);
```

Error codes defined in `bin/errorCodes.js`:

```javascript
codes: {
    'PIN_NAME': { errorCode: 'some_code', ... },
    'DDR3Bus': { errorCode: '...', ... },
    ...
}
```

## Best Practices

1. **Always preserve existing behavior** - Refactoring should not change test logic
2. **Use atomic operations** - ConfigureAndSet should be one operation
3. **Keep error messages descriptive** - Include pin/test info in logs
4. **Validate syntax after changes** - `node -c file.js`
5. **Small commits** - One feature/file per commit for clean history
6. **Backward compatibility** - Maintain existing module exports

## Testing Refactored Code

### Manual Testing

```bash
# Start test boards
npm run ict -- --serial TEST001 --debug 1

# Test specific command
npm run m1cmd -- --command "getfwrev"

# Run EEPROM test
npm run eeprom -- --serial TEST001
```

### Syntax Validation

```bash
# Single file
node -c tests/file.js

# Multiple files
node -c tests/*.js utils/helpers/*.js

# All project
npm run lint  # if linter configured
```

## Known Issues & Workarounds

1. **Async in loops** - Use `for...of` instead of `forEach` to avoid concurrency
2. **Error propagation** - CommandHelper returns false; check for it
3. **GPIO timing** - Add `await delay(100)` between operations if needed
4. **DB initialization** - Ensure `db` is initialized before passing to helpers

## References

- **Helper APIs:** See docstrings in `utils/commandHelper.js`, `utils/gpioHelper.js`, etc.
- **Error codes:** `bin/errorCodes.js`
- **Test patterns:** Refactored files (ddr3.js, rs485.js, statusLed.js, etc.)
- **Protocol docs:** See `README.md` and `fw/` directory

## Contact & Questions

For questions about this refactoring initiative:

1. Check existing refactored files for examples
2. Review commit history: `git log --oneline | grep refactor`
3. Refer to this guide's usage patterns section

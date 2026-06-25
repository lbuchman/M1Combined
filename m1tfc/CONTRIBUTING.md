# Contributing to m1tfc

## Prerequisites

- Node.js 24+
- Ubuntu 22.04 or later (for hardware tests)
- USB/Serial adapters connected for hardware integration tests

## Setup

```bash
git clone https://github.com/lbuchman/M1Combined.git
cd M1Combined/m1tfc
npm install
```

## Code Style

This project uses ESLint and Prettier.

Check before committing:

```bash
npm run lint
npm run format
```

Auto-fix most issues:

```bash
npm run lint:fix
npm run format:fix
```

## Running Tests

```bash
npm test               # run all unit tests
npm test -- --coverage # with coverage report
```

Unit tests are in `__tests__/`. They do **not** require hardware.

## Adding a New Command

1. Create `bin/commands/yourcommand.js`
2. Export a `register(program)` function:

```javascript
'use strict';

function register(program) {
    program
        .command('yourcommand')
        .description('What it does')
        .option('-s, --serial <string>', 'vendor serial number')
        .action(async options => {
            // implementation
        });
}

module.exports = { register };
```

3. The auto-loader picks it up automatically — no other file needs to change.
4. Add a test in `__tests__/bin/commands/yourcommand.test.js`

## Commit Messages

Use the format: `<type>: <short description>`

Types: `feat`, `fix`, `refactor`, `docs`, `test`, `chore`

Examples:

- `feat: add voltage calibration command`
- `fix: handle serial port disconnect gracefully`
- `docs: update README with new options`

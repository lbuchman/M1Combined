'use strict';

module.exports = {
    testEnvironment: 'node',
    testMatch: ['**/__tests__/**/*.test.js'],
    collectCoverageFrom: ['bin/**/*.js', 'src/**/*.js', 'utils/**/*.js', '!**/node_modules/**'],
    coverageReporters: ['text', 'lcov'],
    verbose: true
};

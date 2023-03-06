module.exports = {
    extends: [
      'eslint-config-airbnb-base'
    ].map(require.resolve),
    parser: 'esprima',
    env: {
        node: true,
        mocha: true
    },
    parserOptions: {
      ecmaVersion: 2017,
      sourceType: 'module',
      ecmaFeatures: {
        experimentalObjectRestSpread: true
      }
    },
    rules: {
      indent: 'off',
      'indent-legacy': ['error', 4, {
          SwitchCase: 1,
          VariableDeclarator: 1,
          outerIIFEBody: 1,
          FunctionDeclaration: {
              parameters: 1,
              body: 1
          },
          FunctionExpression: {
              parameters: 1,
              body: 1
          }
      }],
      'brace-style': ['error', 'stroustrup', { allowSingleLine: true }],
      'linebreak-style': 'off',
      'strict': ['off', 'global'],
      'max-len': 'off',
      'comma-dangle': 'off',
      'no-console': 'warn',
      'arrow-body-style': 'off',
      'function-paren-newline': ['error', 'consistent'],
      'object-curly-newline': ['error', { consistent: true }],
      'no-multi-spaces': ['error', { ignoreEOLComments: true }],
      'prefer-destructuring': 'off'
    }
  };
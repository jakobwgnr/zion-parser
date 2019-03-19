# zion-parser [![travis][travis-image]][travis-url] [![npm][npm-image]][npm-url] [![Coverage Status][coverage-image]][coverage-url]

[travis-image]: https://travis-ci.org/jakobwgnr/zion-parser.svg?branch=master
[travis-url]: https://travis-ci.org/jakobwgnr/zion-parser
[npm-image]: https://img.shields.io/npm/v/zion-parser.svg
[npm-url]: https://www.npmjs.com/package/zion-parser
[coverage-image]: https://coveralls.io/repos/github/jakobwgnr/zion-parser/badge.svg
[coverage-url]: https://coveralls.io/github/jakobwgnr/zion-parser


A typescript parser (and Lexer) implementation for COBOL

Note: This is currently just for trying out JS/TS myself & shouldn't be used anywhere.

Anyways if you want to try it:

```bash
$ npm install --save zion-parser
```

```js
const zionParser = require('zion-parser');

console.log(zionParser.lex('        IDENTIFICATION DIVISION.', { fromPath: false }));
```

##Development (Notes)

```bash
$ npm install - Install dependencies
$ npm build - Compiles Typescrip
$ npm test - Run unit tests
> npm run mocktest - Run mock tests
> npm run masstest - Run mass tests (currently not implemented)
> npm version [major|minor|patch] - Increase npm version (including linting, etc.)
> npm publish - Publishes new version to npm (including linting, testing, etc.)
> npm lint - Linter
> npm format - Formater
> npm clean - clean project (prior npm install state)
```
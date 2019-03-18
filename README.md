# zion-parser [![travis][travis-image]][travis-url] [![npm][npm-image]][npm-url] [![Codecov Coverage][coverage-image]][coverage-url]

[travis-image]: https://travis-ci.org/jakobwgnr/zion-parser.svg?branch=master
[travis-url]: https://travis-ci.org/jakobwgnr/zion-parser
[npm-image]: https://img.shields.io/npm/v/zion-parser.svg
[npm-url]: https://www.npmjs.com/package/zion-parser
[coverage-image]: https://img.shields.io/codecov/c/github/jakobwgnr/zion-parser/master.svg?style=flat-square
[coverage-url]: https://codecov.io/gh/jakobwgnr/zion-parser/

A typescript parser (and Lexer) implementation for COBOL

Note: This is currently just for trying out JS/TS myself & shouldn't be used anywhere.

Anyways if you want to try it:

```bash
> npm install --save zion-parser
```

```js
const zionParser = require('zion-parser');

console.log(zionParser.lex('        IDENTIFICATION DIVISION.', { fromPath: false }));
```
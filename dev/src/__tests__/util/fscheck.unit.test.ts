import { fscheck } from '../../util/fscheck';

import * as path from 'path';

test('fscheck.isDirectory work expected', () => {
  expect(fscheck.isDirectory(path.join(__dirname, '../../util/'))).toBeTruthy();
  expect(fscheck.isDirectory(path.join(__dirname, './no/such/dir'))).toBeFalsy();
});

test('fscheck.isFile work expected', () => {
  expect(fscheck.isFile(path.join(__dirname, '../../util/fscheck.ts'))).toBeTruthy();
  expect(fscheck.isDirectory(path.join(__dirname, './no/such/file.ts'))).toBeFalsy();
});

test('fscheck.codeFromPath work expected', () => {
  expect(fscheck.codeFromPath('./dev/src/__tests__/testfiles/comment.cbl')).toBeDefined();
  expect(fscheck.codeFromPath(path.join(__dirname, '../testfiles/comment.cbl'))).toBeDefined();
});

import * as fscheck from '../../util/fscheck';

import * as path from 'path';

path.join(__dirname, '../../util/fscheck');
test('fscheck.isDirectory work expected', () => {
  expect(fscheck.isDirectory(path.join(__dirname, '../../util/'))).toBeTruthy();
  expect(fscheck.isDirectory(path.join(__dirname, './no/such/dir'))).toBeFalsy();
});

test('fscheck.isFile work expected', () => {
  expect(fscheck.isFile(path.join(__dirname, '../../util/fscheck.js'))).toBeTruthy();
  expect(fscheck.isDirectory(path.join(__dirname, './no/such/file.js'))).toBeFalsy();
});

// this test is not executed on standard 'npm test'

import * as fs from 'fs';
import * as path from 'path';

/**
 * List all files in a directory recursively in a synchronous fashion
 */
function* walkSync(dir: string): any {
  const files = fs.readdirSync(dir);

  for (const file of files) {
    const pathToFile = path.join(dir, file);
    const isDirectory = fs.statSync(pathToFile).isDirectory();
    if (isDirectory) {
      yield* walkSync(pathToFile);
    } else {
      if (
        pathToFile.substring(pathToFile.lastIndexOf('.') + 1, pathToFile.length) === 'cob' ||
        pathToFile.substring(pathToFile.lastIndexOf('.') + 1, pathToFile.length) === 'cbl'
      ) {
        yield pathToFile;
      }
    }
  }
}

const absolutePath = path.resolve(__dirname, 'masstest/');

// Execute on each cobol file
for (const file of walkSync(absolutePath)) {
  test('Execute on each file', () => {
    // TODO
  });
}

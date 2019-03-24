/**
 * @fileoverview helper to wrap fs.lstatSync(<path/to/file>)
 * to provide a Boolean instead of exceptions
 * @author Jakob Wagner
 */

// ------------------------------------------------------------------------------
// Requirements
// ------------------------------------------------------------------------------
import * as appRoot from 'app-root-path';
import * as fs from 'fs';
import * as path from 'path';

// ------------------------------------------------------------------------------
// Public Interface
// ------------------------------------------------------------------------------

export const fscheck = {
  /**
   * Checks whether the given string points to a directory
   * @param {string} path A path to a directory
   * @returns {boolean} `true` if a directory exists at the given location
   */
  isDirectory(codePath: string) {
    try {
      return fs.lstatSync(codePath).isDirectory();
    } catch (e) {
      return false;
    }
  },

  /**
   * Checks whether the given string points to a file
   * @param {string} path A path to a file
   * @returns {boolean} `true` if a file exists at the given location
   */
  isFile(codePath: any) {
    try {
      return fs.lstatSync(codePath).isFile();
    } catch (e) {
      return false;
    }
  },
  codeFromPath(filePath: string): string {
    if (!path.isAbsolute(filePath)) {
      filePath = path.join(appRoot.path, filePath);
    }
    if (this.isFile(filePath)) {
      return fs.readFileSync(filePath, 'utf8');
    } else {
      throw new Error('Not a file');
    }
  },
};

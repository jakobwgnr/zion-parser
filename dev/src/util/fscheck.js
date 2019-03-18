/**
 * @fileoverview helper to wrap fs.lstatSync(<path/to/file>)
 * to provide a Boolean instead of exceptions
 * @author Jakob Wagner
 */

//------------------------------------------------------------------------------
// Requirements
//------------------------------------------------------------------------------

const fs = require('fs');

//------------------------------------------------------------------------------
// Public Interface
//------------------------------------------------------------------------------

const fscheck = {
  /**
   * Checks whether the given string points to a directory
   * @param {string} path A path to a directory
   * @returns {boolean} `true` if a directory exists at the given location
   */
  isDirectory(path) {
    try {
      return fs.lstatSync(path).isDirectory();
    } catch (e) {
      return false;
    }
  },

  /**
   * Checks whether the given string points to a file
   * @param {string} path A path to a file
   * @returns {boolean} `true` if a file exists at the given location
   */
  isFile(path) {
    try {
      return fs.lstatSync(path).isFile();
    } catch (e) {
      return false;
    }
  },
};

module.exports = fscheck;

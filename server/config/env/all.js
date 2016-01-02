'use strict';

var path = require('path'),
  rootPath = path.normalize(__dirname + '/../../..');

module.exports = {
  hashLength: 5,  // length of compilation hash
  compileTimeout: 60, // how many seconds before the compilation of a program is terminated
  executeTimeout: 8, // how many seconds before the execution of a program is terminated
  validProjectTime: 30, // number of minutes until the project is still valid. After this time, the ID is invalid and a new ID would be generated.
  sessionCookieKey: 'CoboKaliId' // cookies are attached to the response
};

'use strict';

/**
 * Returns the prefix of the command for running a compilation in a docker.
 * @param aLanguage the language that should be compiled
 * @param aProjectPath the path were the files are that should be compiled
 */
var getVMCompilationConfig = function (aLanguage, aProjectPath) {
  return '';
}


/**
 * Returns the prefix for the command to testing a program in a docker.
 * @param aLanguage the language of the project that should be tested
 * @param aProjectPath
 */
var getVMRunConfig = function (aLanguage, aProjectPath) {
  return '';
}


module.exports = {
  env: 'development',
  getVMCompilationConfig: getVMCompilationConfig,
  getVMRunConfig: getVMRunConfig,
  // when developing on Mac and using boot2docker, we can use any file system path but only things in /User
  mantraPath: '/tmp/projects/',
  removeFolders: false
};
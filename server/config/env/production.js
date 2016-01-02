'use strict';

/**
 * Returns the prefix of the command for running a compilation in a docker.
 * @param aLanguage the language that should be compiled
 * @param aProjectPath the path were the files are that should be compiled
 */
var getVMCompilationConfig = function (aLanguage, aProjectPath) {

  // after how many seconds should the compilation timeout?
  var _timeout = '60s';

  switch (aLanguage) {
    case 'Python-UnitTest':
      _timeout = '8s'; // Note: a Python program is run under the "compile" command; thus, a timeout after 8s
      break;
    default:
      _timeout = '60s';
      break;
  }

  var _commandPrefix = 'docker-run-timeout.sh ' + _timeout;
  return _getVMConfig(_commandPrefix, aLanguage, aProjectPath);
};


/**
 * Returns the prefix for the command to testing a program in a docker.
 * @param aLanguage the language of the project that should be tested
 * @param aProjectPath
 */
var getVMRunConfig = function (aLanguage, aProjectPath) {

  var _timeout = '8s';
  var _commandPrefix = 'docker-run-timeout.sh ' + _timeout;
  return _getVMConfig(_commandPrefix, aLanguage, aProjectPath);
};


var _getVMConfig = function (aCommandPrefix, aLanguage, aProjectPath) {

  var _cmd = aCommandPrefix + ' -u cobo --ulimit nproc=1024:1536 --net="none" -v ' + aProjectPath + ':' + aProjectPath + ' ';

  switch (aLanguage) {
    case 'Python-UnitTest':
      _cmd += 'cobo/python';
      break;
    case 'Haskell-HSpec':
      _cmd += 'cobo/haskell-hspec';
      break;
    case 'Java-JUnit':
      _cmd += 'cobo/java8-junit4';
      break;
    default:
      _cmd += 'cobo/java8-junit4'; // use java-junit as default; there's really no good default if we can't match the language
      break;
  }

  // make sure we have a whitespace at the end of the command
  _cmd += ' ';

  return _cmd;
};


module.exports = {
  env: 'production',
  getVMCompilationConfig: getVMCompilationConfig,
  getVMRunConfig: getVMRunConfig,
  mantraPath: '/tmp/projects/',
  removeFolders: true
};

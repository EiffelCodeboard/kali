/**
 * Created by Martin on 01/08/14.
 */
'use strict';

var mantraConfig = require('./config/config.js'),
  fs = require('fs'),
  path = require('path'),
  mkdirp = require('mkdirp'),
  Promise = require('bluebird'),
  rimraf = require('rimraf');
/**
 * Copies the files in the array files and store them in dstPath
 * @files: files array with files[i].filename and files[i].content
 */
exports.copyFiles = function (files, dstPath) {
  /* the promises is an array of promises */
  var promises = [];

  if (files != undefined) {
    for (var i = 0; i < files.length; i++) {
      // create a new promise for the creation of the file
      var x = new Promise(function (resolve, reject) {
        var name = files[i].filename;
        var content = files[i].content;
        var filePath = dstPath + '/' + name;
        var dirName = path.dirname(filePath);
        fs.exists(dirName, function (exists) {
          if (!exists) {
            mkdirp.sync(dirName); //
          }
          fs.writeFile(filePath, content, function (error) {
            if (!error) {
              resolve(); // resolve the promise
            }
            else {
              // console.log('ERROR writing the file' + filePath);
              // console.log('Trace ' + e);
            }
          });
        });

      });
      promises.push(x); // store the promise
    }
  }
  return Promise.all(promises);
};


/**
 * Generates an unique ID for a new project, and creates the forlder for the project
 * @callBack: callback function
 * @files: files array with files[i].filename and files[i].content
 */
exports.generateId = function (path, callBack) {
  // generates an unique ID and creates the folder
  var dstPath;
  var exists;
  var id1 = Math.random().toString(36).substring(mantraConfig.hashLength);
  var id2 = Math.random().toString(36).substring(mantraConfig.hashLength);
  var id3 = Math.random().toString(36).substring(mantraConfig.hashLength);
  var id4 = Math.random().toString(36).substring(mantraConfig.hashLength);
  var id5 = Math.random().toString(36).substring(mantraConfig.hashLength);
  var id = String();
  id = id1 + '-' + id2 + '-' + id3 + '-' + id4 + '-' + id5;
  dstPath = mantraConfig.mantraPath + id + '/';
  fs.exists(dstPath, function (exists) {
    if (!exists) {
      //console.log('Created folder'+dstPath);
      fs.mkdir(dstPath, function (error) {
        if (path != undefined && path != null) {
          // create the folder
          var ncp = require('ncp').ncp;
          ncp.limit = 16;
          ncp(path, dstPath, function (err) {
            if (err) {
              console.error(err);
              callBack("WRONG_ID");
            }
            callBack(id);

          });
        }
        else {
          callBack(id);
        }
      });
    }
    else {
      // ID already exists
      modules.exports.generateId(path, callBack);
    }
  });

};


/**
 * Executes a command line, and returns its output
 * @param command the command that will be execute as a new process
 * @param timeout a timeout value in seconds after which the process will be killed
 * @param callBack the callback that's invoked when the command has finished
 */
exports.executeCommand = function (command, timeout, callBack) {
  var exec = require('child_process').exec,
    child;

  child = exec(command, function (error, stdout, stderr) {
    if (error !== null) {
      console.log('utiljs.executeCommand error: ' + error + ' error signal' + error.signal);
      console.log('stderr is: ' + stderr);
      if (error.signal == 'SIGTERM') {
        stderr = 'Waring: your command exceeded the time limit of ' + timeout + ' seconds.' +
          '\nCompilations and executions may not exceed their time limits.' +
          '\nContact us at info@codeboard.io if your programs need to run for longer times.'
      }
    }
    callBack(stdout, stderr);
  });
};


exports.executeCommandWithPath = function (command, currentPath, timeout, callBack) {
  var exec = require('child_process').exec,
    child;
  // console.log('Current path to run ' + currentPath);
  // TODO: remove the time here. We do timeouts via the bash scripts.
  child = exec(command, {cwd: currentPath, timeout: timeout * 1000}, function (error, stdout, stderr) {
    if (error !== null) {
      //console.log('Execution error: ' + error+' error signal'+error);
      //console.log('stderr is: '+stderr);
      if (error != null && error.signal == 'SIGTERM') {
        stderr = 'Waring: your command exceeded the time limit of ' + timeout + 'seconds.' +
          '\nCompilations and executions may not exceed their time limits.' +
          '\nTherefore, Codeboard terminated your program.'
      }
    }
    callBack(stdout, stderr);
  });
};


/**
 * Given an id, it returns true if the folder PATH+id exists; otherwise false
 */
exports.isValidId = function (id) {
  return new Promise(function (resolve, reject) {
    var dstPath = mantraConfig.mantraPath + id + '/';
    fs.exists(dstPath, function (exists) {
      if (!exists) {
        resolve(false);
      }
      else {
        fs.stat(dstPath, function (error, stats) {
          if (!error && stats != null && stats.mtime != null) {
            var now = new Date();
            var minutes = (Math.abs(now - stats.mtime) / 1000) / 60; // number of minutes since last modified
            if (minutes > mantraConfig.validProjectTime) {
              resolve(false);
            }
            else {
              resolve(true);
            }
          }
          else {
            resolve(false);
          }
        });
      }
    });
  });
};


/**
 * Generates an string containing all files names of a file list that end with 'end'
 * @param files
 * @param end
 * @returns {string}
 */
exports.generateFileNames = function (files, end, excludeFile) {
  var allFiles = '';
  if (files != undefined) {
    for (var i = 0; i < files.length; i++) {
      var name = files[i].filename;


      if ((name.indexOf(end, this.length - end.length) !== -1)) { // &&
        //(name.indexOf(excludeFile, this.length - excludeFile.length) == -1)) {
        allFiles = allFiles + ' ./' + name;
        //console.log('Ends with '+end);
      }

    }
  }
  return allFiles;
};


/**
 * Generates an string containing a command for all files names of a file list that end with 'end'.
 * For example for t1.hs t2.hs, and command "runhaskell", it generates runhaskell t1.hs; runhaskell t2.hs;
 * @param files
 * @param end
 * @returns {string}
 */
exports.generateCommandWithFileNames = function (files, end, command, separator) {
  var allFiles = '';
  if (files != undefined) {
    for (var i = 0; i < files.length; i++) {
      var name = files[i].filename;
      if (name.indexOf(end, this.length - end.length) !== -1) {
        allFiles = allFiles + command + ' ./' + name + separator;
        //console.log('Ends with '+end);
      }

    }
  }
  //console.log('Generated command ' + allFiles);
  return allFiles;
};


exports.getCompilerSettings = function (files) {
  var result = {};
  result.error = true;
  if (files != undefined) {
    var i = 0;
    while (i < files.length) {
      var name = files[i].filename;
      if (name.indexOf("codeboard.json") !== -1) {
        try {
          //console.log("FOUND");
          result = JSON.parse(files[i].content);
          result.error = false;
          return result;
        }
        catch (e) {
          console.log("utiljs.getCompilerSettings error: error trying to read codeboard.json file");
          result.error = true;
          return result;
        }

      }
      i++;
    }
  }
  return result;
};


exports.getRunSettings = function (id, callBack) {
  var result = {};
  result.error = true;
  var filePath = mantraConfig.mantraPath + id + '/Root/codeboard.json';
  fs.readFile(filePath, function (error, data) {
    if (error) {
      console.log('utiljs.getRunSettings error: error reading codeboard.json');
      callBack(result);
    }
    else {
      try {
        result = JSON.parse(data);
        result.error = false;
        callBack(result);
      }
      catch (e) {
        result.error = true;
        callBack(result);
      }

    }
  });
};


/**
 * Writes an object as a JSON file. Mainly using for testing
 * @param fileName
 * @param content
 */
exports.writeJsonToFile = function (fileName, content) {
  var c = JSON.stringify(content);
  //console.log('Content ' + c);
  fs.writeFile(fileName, c, function (error) {
    if (error) {
      console.log("utiljs.writeJsonToFile error: error writing JSON to file: " + error);
    }
  });
};


exports.getCodeboardSettingError = function () {
  var result = {};
  result.compilationError = true;
  result.warningError = false;
  result.errors = [];
  result.warnings = [];
  var output = 'Error in codeboard setting file; check the file is valid JSON file';
  result.dump = output;
  result.output = output;
  return result;
};


/**
 * Removes the folder 'id'
 * @param id
 * @returns {Promise}
 */
exports.removeFolder = function (id) {
  return new Promise(function (resolve, reject) {
    var dstPath = mantraConfig.mantraPath + id + '/';
    //console.log(dstPath);
    rimraf(dstPath, function (err) {
      if (!err) {
        //console.log('Folder removed' + dstPath);
        resolve(true);
      }
      else {
        //console.log('Error removing folder ' + err);
        resolve(false);
      }
    });
  });
};

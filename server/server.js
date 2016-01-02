/**
 * Created by Martin on 28/07/14.
 * Mantra server in JavaScript
 * (an Eiffel compilation server)
 */
'use strict';

// Set default node environment to development
var env = process.env.NODE_ENV = process.env.NODE_ENV || 'development';

var express = require('express'),
  path = require('path'),
  fs = require('fs'),
  bodyParser = require('body-parser'),
  Promise = require('bluebird');
//util = require('util');


var app = express();
app.use(bodyParser());


var javaCtrl = require('./testingRunnerJava.js'),
  pythonCtrl = require('./testingRunnerPython.js'),
  mantraConfig = require('./config/config.js'),
  mantraUtilCtrl = require('./util.js'),
  haskellCtrl = require('./testingRunnerHaskell.js');


/**
 * Compiles and runs the JUnit test of a Java project
 *  * @param req expects
 *              req.body.files: files to compile, or req.body.path: path (in this server) where the files are
 *              req.body.path: usually undefined. If a path is provided, files from the path are copied
 *              req.body.testFiles: files with tests to run
 *
 */
app.post('/java', function (req, res) {
  //console.log('request Java for Kali');
  generateIdAndCopyFiles(req)
    .then(function (id) {
      javaCtrl.testProject(id, req, res);
    });
});

/**
 * Compiles and runs the Python Unit test of a Python project
 *  * @param req expects
 *              req.body.files: files to compile, or req.body.path: path (in this server) where the files are
 *              req.body.path: usually undefined. If a path is provided, files from the path are copied
 *              req.body.testFiles: files with tests to run
 *
 */
app.post('/python', function (req, res) {
  //console.log('request Python for Kali');
  generateIdAndCopyFiles(req)
    .then(function (id) {
      pythonCtrl.testProject(id, req, res);
    });
});

/**
 * Compiles and runs the Hspec Testing Framework for a Haskell project
 *  * @param req expects
 *              req.body.files: files to compile, or req.body.path: path (in this server) where the files are
 *              req.body.path: usually undefined. If a path is provided, files from the path are copied
 *              req.body.testFiles: files with tests to run
 *
 */
app.post('/haskell', function (req, res) {
  //console.log('request Haskell for Kali');
  generateIdAndCopyFiles(req)
    .then(function (id) {
      haskellCtrl.testProject(id, req, res);
    });
});

/**
 * Generates a new Id creating the folder where to store the files, and then stores
 * the source files as well as the test files.
 * @param req.body.files: source files
 *        req.body.testFiles: test files
 * @returns {Promise (id)}
 */
function generateIdAndCopyFiles(req) {
  return new Promise(function (resolve, reject) {
    var path = req.body.path;
    var files = req.body.files;
    var testFiles = req.body.testFiles;
    mantraUtilCtrl.generateId(path, function (id) {
      //console.log('Generated ID ' + id);
      if (id != undefined) {
        var dstPath = mantraConfig.mantraPath + id + '/';
        mantraUtilCtrl.copyFiles(files, dstPath)
          .then(function () {
            mantraUtilCtrl.copyFiles(testFiles, dstPath)
              .then(function () {
                resolve(id);
              });
          });
      }
    });
  });
}


var port = process.env.NODE_PORT = process.env.NODE_PORT || '7070';

app.listen(port, function () {
  console.log('Kali server listening on port ' + port);
});


module.exports = app;

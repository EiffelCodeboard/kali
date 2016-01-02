/**
 * Created by Martin on 15/10/14.
 */
'use strict';

var should = require('should'),
  mantraUtilCtrl = require('../server/util.js'),
  mantraConfig = require('../server/config/config.js'),
  fs = require('fs');

describe('Compilers setup: installation and folder access', function () {

  // set timeout for tests to 50 secs
  this.timeout(1000 * 50);


  it('Trivial test', function () {
    [1, 2, 3].indexOf(2).should.equal(1);
  });


  it('tmp/projects folder has write access', function (done) {
    var command = 'javac -help';
    var randomId = Math.random().toString(36).substring(5);
    var dstPath = '/tmp/projects/' + randomId + '/Root';
    var elem1 = {filename: "application.e", content: "class APPLICATION end"};
    var elem2 = {filename: "bar.e", content: "class BAR end"};
    var elem3 = {filename: "foo.e", content: "class FOO end"};
    var files = [elem1, elem2, elem3];
    mantraUtilCtrl.copyFiles(files, dstPath)
      .then(function () {
        // promise for copying files is resolved
        var exists = fs.existsSync(dstPath);
        //console.log("Path in test tmp"+dstPath);
        var existsFile1 = fs.existsSync(dstPath + "/" + elem1.filename);
        var existsFile2 = fs.existsSync(dstPath + "/" + elem2.filename);
        var existsFile3 = fs.existsSync(dstPath + "/" + elem3.filename);
        exists.should.equal(true);
        existsFile1.should.equal(true);
        existsFile2.should.equal(true);
        existsFile3.should.equal(true);
        done();
      });
  });


  it('files send in path are copied', function (done) {
    var command = 'javac -help';
    var randomId = Math.random().toString(36).substring(5);
    var dstPath = '/tmp/projects/' + randomId + '/Root';
    var elem1 = {filename: "application.e", content: "class APPLICATION end"};
    var elem2 = {filename: "bar.e", content: "class BAR end"};
    var elem3 = {filename: "foo.e", content: "class FOO end"};
    var files = [elem1, elem2, elem3];
    mantraUtilCtrl.copyFiles(files, dstPath)
      .then(function () {
        // promise for copying files is resolved
        mantraUtilCtrl.generateId(dstPath, function (id) {
          var newPath = '/tmp/projects/' + id;
          var exists = fs.existsSync(dstPath);
          //console.log("Path in test tmp"+newPath);
          var existsFile1 = fs.existsSync(newPath + "/" + elem1.filename);
          var existsFile2 = fs.existsSync(newPath + "/" + elem2.filename);
          var existsFile3 = fs.existsSync(newPath + "/" + elem3.filename);
          exists.should.equal(true);
          existsFile1.should.equal(true);
          existsFile2.should.equal(true);
          existsFile3.should.equal(true);
          done();
        });


      });
  });

  it('After generating an ID, it is valid', function (done) {
    mantraUtilCtrl.generateId(undefined, function (id) {
      mantraUtilCtrl.isValidId(id)
        .then(function (exists) {
          exists.should.equal(true);
          done();
        });
    });
  });


  it('Java is installed', function (done) {
    var dstPath = '/tmp/projects';
    var command = mantraConfig.getVMCompilationConfig('Java-JUnit', dstPath) + ' javac -help';
    mantraUtilCtrl.executeCommandWithPath(command, dstPath, 10, function (stdout, stderror) {
      // send the result here
      var result = ' ';
      result = stdout + stderror;
      (result.indexOf("Usage: javac ")).should.not.equal(-1);
      done();
    });
  });


  it('Haskell is installed', function (done) {
    var dstPath = '/tmp/projects';
    var command = mantraConfig.getVMCompilationConfig('Haskell-HSpec', dstPath) + ' ghc --help';
    mantraUtilCtrl.executeCommandWithPath(command, dstPath, 10, function (stdout, stderror) {
      // send the result here
      var result = ' ';
      result = stdout + stderror;
      (result.indexOf("To compile and link a complete Haskell program")).should.not.equal(-1);
      (result.indexOf("ghc [command-line-options-and-input-files]")).should.not.equal(-1);
      done();
    });
  });


  it('Python is installed', function (done) {
    var dstPath = '/tmp/projects';
    var command = mantraConfig.getVMCompilationConfig('Python-UnitTest', dstPath) + ' python -h';
    mantraUtilCtrl.executeCommandWithPath(command, dstPath, 10, function (stdout, stderror) {
      // send the result here
      var result = ' ';
      result = stdout + stderror;
      (result.indexOf("usage: python [option] ... [-c cmd | -m mod | file | -] [arg] ...")).should.not.equal(-1);
      (result.indexOf("ignore PYTHON* environment variables")).should.not.equal(-1);
      done();
    });
  });
});
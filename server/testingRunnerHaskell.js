/**
 * Created by Martin on 12/09/14.
 */
'use strict';

var util = require('./util.js'),
  config = require('./config/config.js');

/**
 * Generates the command line string to invoke the compiler
 * @path: a full directory path
 */
function generateHaskellCompilerCommand(path, mainClass) {
  var options = '';
  options = config.getVMCompilationConfig('Haskell-HSpec', path) + 'sh -c "cd ' + path + '; ghc -o output.out ' + mainClass + ';echo "" "';
  //console.log(options);
  return options;
}


/**
 * Generates the command line string to invoke the HSpec (the testing framework)
 * @path: a full directory path
 */
function generateHaskellTestUnitCommand(path, mainClass) {
  var options = '';
  options = config.getVMRunConfig('Haskell-HSpec', path) + 'sh -c "cd ' + path + '; ' + mainClass + ' echo"" "';
  //console.log("RUN COMMAND " + options);
  return options;
}


/**
 * Takes the Haskell output and generates a JSON object.
 */
function parseHaskellCompilerOutput(output) {
  //console.log('Parsing Haskell output');
  var result = {};
  result.compilationError = true;
  result.warningError = false;
  result.errors = [];
  result.warnings = [];
  if (output === '') {
    output = 'Compilation successful';
    result.compilationError = false;
  }
  if (output.indexOf('Linking output.out ...') != -1) {
    // TODO: check that this is correct in Haskell
    result.compilationError = false;
  }
  result.dump = output;
  result.outputCompiler = output;

  return result;
}

/**
 * Takes the HSpec Unit output (Haskell) and generates a JSON object extracting the number of test that pass and the ones that fail.
 */
function parseHaskellTestUnitOutput(output, result) {
  result.output = output;
  var myRegExp = /([0-9]+)(\sexample(s?),\s)([0-9]+)(\sfailure(s?))/g;
  var match = myRegExp.exec(output);
  // console.log(output);
  if (match != null) {
    result.numTestsPassing = 0;
    result.numTestsFailing = 0;
  } else {
    result.numTestsPassing = -1;
    result.numTestsFailing = -1;
  }
  while (match != null) {
    // There was a match.There are failed tests
    var fail = parseInt(match[4]);
    var succeed = parseInt(match[1]) - fail;
    result.numTestsFailing = result.numTestsFailing + fail;
    result.numTestsPassing = result.numTestsPassing + succeed;

    match = myRegExp.exec(output)
  }
};


/**
 * Removes the folder with the given id and sends the given result.
 * Also sets the cookie header.
 * @param res
 * @param result {Object} the result to send back
 * @param id {string} the mantraId representing the folder to delete
 */
function removeFoldersAndSend(res, result, id) {
  // set a cookie with the id as the id of the session
  res.cookie(config.sessionCookieKey, id);

  if (config.removeFolders) {
    util.removeFolder(id)
      .then(function (reply) {
        res.send(result);
      });
  }
  else {
    res.send(result);
  }
};


exports.testProject = function (id, req, res) {
  var testFiles = req.body.testFiles;

  var dstPath = config.mantraPath + id + '/';

  // compiles all test files
  var testToCompile, testsToRun;
  if (req.body.testArgs != undefined) {
    testToCompile = req.body.testArgs.testsToCompile;
    testsToRun = req.body.testArgs.testsToRun;
  }


  if (testToCompile == undefined || testsToRun == undefined) {
    // generate the list of files to compile and rum if they are not provided
    testToCompile = util.generateFileNames(testFiles, ".hs"); // get all test files
    testToCompile = testToCompile + util.generateFileNames(req.body.files, ".hs"); // get all source files

    // generate the command to run HSpec for all Test files
    testsToRun = util.generateCommandWithFileNames(testFiles, ".hs", " runhaskell ", " ; "); // get all test files
  }

  var settings = util.getCompilerSettings(req.body.files);
  if (settings.error) {
    // there was an error parsing the JSON object
    var result = {};
    result = util.getCodeboardSettingError();
    result.numTestsPassing = -1;
    result.numTestsFailing = -1;

    removeFoldersAndSend(res, result, id);
  }
  else {
    var mainClass = settings.MainFileForCompilation;
    mainClass = util.generateFileNames(req.body.files, ".hs"); // get all source files
    var mainArg = " -main-is " + settings.GHCArgumentMainIs;

    var command = generateHaskellCompilerCommand(dstPath, mainArg + mainClass);
    //var command = generateHaskellCompilerCommand(dstPath, testToCompile); //testToCompile);
    // console.log('Options for existing project '+command);
    util.executeCommandWithPath(command, dstPath, config.compileTimeout, function (stdout, stderror) {
      // send the result here
      var result = {};
      result = parseHaskellCompilerOutput(stdout + stderror); // adding the compilation output
      result.id = id;

      //console.log('Options for existing project '+command);
      if (result.compilationError == false) {
        var command = generateHaskellTestUnitCommand(dstPath, testsToRun);
        util.executeCommandWithPath(command, dstPath, config.compileTimeout, function (stdout, stderror) {
          parseHaskellTestUnitOutput(stdout + stderror, result);

          removeFoldersAndSend(res, result, id);
        });
      }
      else {
        result.output = "Compilation error, HSpec (for Haskell) was not run.";
        result.numTestsPassing = -1;
        result.numTestsFailing = -1;

        removeFoldersAndSend(res, result, id);
      }
    });
  }

};


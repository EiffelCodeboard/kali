/**
 * Created by Martin on 16/03/15.
 */
var fs = require('fs');


/**
 * Reads all files from a folder and generates the JSON file for Kali
 */
var generateSrcFilesFromFolder = function (path, language, callBack) {
  var result = {
    language: language,
    files: [],
    testFiles: []
  };

  //console.log("Start")
  var allFiles = walk(path);
  if (allFiles.length > 0) {
    //console.log("Ok reading folder"+path);
    allFiles.forEach(function (file) {
      var currentFile = {
        filename: "",
        content: ""
      };
      currentFile.filename = file.replace(path + "/", "");
      currentFile.filePath = file;
      currentFile.content = fs.readFileSync(file, 'utf8');
      //console.log("File name "+currentFile.filename);
      //console.log("File Path "+currentFile.filePath);
      if (currentFile.filename.indexOf(".DS_Store") == -1) {
        result.files.push(currentFile);
      }
    });
    //console.log("Number of files "+allFiles.length);
    callBack(result);
  }
  else {
    //console.log("EERROR reading folder"+path);
    callBack(result);
  }

}

/**
 * Reads all files from a folder and generates the JSON file for Kali
 */
var generateTestFilesFromFolder = function (path, removePath, result, callBack) {

  //console.log("Start")
  var allFiles = walk(path);
  if (allFiles.length > 0) {
    //console.log("Ok reading folder"+path);
    allFiles.forEach(function (file) {
      var currentFile = {
        filename: "",
        content: ""
      };
      currentFile.filename = file.replace(removePath + "/", "");
      currentFile.filePath = file;
      currentFile.content = fs.readFileSync(file, 'utf8');
      //console.log("File name "+currentFile.filename);
      //console.log("File Path "+currentFile.filePath);
      result.testFiles.push(currentFile);
    });
    //console.log("Number of files "+allFiles.length);
    callBack(result);
  }
  else {
    //console.log("EERROR reading folder"+path);
    callBack(result);
  }

}

var walk = function (dir) {
  var results = []
  var list = fs.readdirSync(dir)
  list.forEach(function (file) {
    file = dir + '/' + file
    var stat = fs.statSync(file)
    if (stat && stat.isDirectory()) results = results.concat(walk(file))
    else results.push(file)
  })
  return results
}

exports.generateSrcFilesFromFolder = generateSrcFilesFromFolder;
exports.generateTestFilesFromFolder = generateTestFilesFromFolder;
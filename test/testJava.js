var app = require('../server/server.js'),
  request = require('supertest'),
  express = require('express'),
  should = require('should'),
  bodyParser = require('body-parser'),
  fs = require('fs'),
  utilTest = require('./util/utilForTesting.js'),
  utilKali = require('../server/util.js');

app.use(bodyParser());

describe('Test Kali with Java ', function () {

  this.timeout(1000 * 50);

  /*
   utilKali.copyFiles(data.files, folder)
   .then(function() {
   utilKali.copyFiles(data.testFiles, folder)
   .then(function() {
   done();
   });
   });
   */
  it('Kali Java: error in compilation (one file)', function (done) {
    var folder = "./test/src_examples/java/j_error";
    var testFolder = folder + "/Root/test";

    utilTest.generateSrcFilesFromFolder(folder, "Java-JUnit", function (data) {
      // get the tests files
      utilTest.generateTestFilesFromFolder(testFolder, folder, data, function (data) {
        request(app)
          .post('/java')
          .send(data)
          .expect('Content-Type', /json/)
          .expect(200)
          .end(function (error, reply) {
            reply.body.outputCompiler.should.not.equal('Compilation successful');
            reply.body.compilationError.should.equal(true);
            reply.body.id.should.not.equal(undefined);
            reply.body.output.should.not.equal(undefined);
            reply.body.output.should.equal("Compilation error, JUnit was not run.");
            reply.body.numTestsPassing.should.equal(-1);
            reply.body.numTestsFailing.should.equal(-1);
            done();
          });
      });
    });
  });

  it('Kali Java: error in compilation (several files)', function (done) {
    var folder = "./test/src_examples/java/j_error_several_files";
    var testFolder = folder + "/Root/test";

    utilTest.generateSrcFilesFromFolder(folder, "Java-JUnit", function (data) {
      // get the tests files
      utilTest.generateTestFilesFromFolder(testFolder, folder, data, function (data) {
        request(app)
          .post('/java')
          .send(data)
          .expect('Content-Type', /json/)
          .expect(200)
          .end(function (error, reply) {
            reply.body.outputCompiler.should.not.equal('Compilation successful');
            reply.body.compilationError.should.equal(true);
            reply.body.id.should.not.equal(undefined);
            reply.body.numTestsPassing.should.equal(-1);
            reply.body.numTestsFailing.should.equal(-1);

            done();
          });
      });
    });
  });

  it('Kali Java: successful test execution (one file, one test)', function (done) {
    var folder = "./test/src_examples/java/j_one_file";
    var testFolder = folder + "/Root/test";

    utilTest.generateSrcFilesFromFolder(folder, "Java-JUnit", function (data) {
      // get the tests files
      utilTest.generateTestFilesFromFolder(testFolder, folder, data, function (data) {

        request(app)
          .post('/java')
          .send(data)
          .expect('Content-Type', /json/)
          .expect(200)
          .end(function (error, reply) {
            reply.body.outputCompiler.should.equal('Compilation successful');
            reply.body.compilationError.should.equal(false);
            reply.body.id.should.not.equal(undefined);
            reply.body.numTestsPassing.should.equal(1);
            reply.body.numTestsFailing.should.equal(0);
            done();
          });
      });
    });
  });

  it('Kali Java: successful test execution (one file, one test in packages)', function (done) {
    var folder = "./test/src_examples/java/j_one_file_packages";
    var testFolder = folder + "/Root/test";

    utilTest.generateSrcFilesFromFolder(folder, "Java-JUnit", function (data) {
      // get the tests files
      utilTest.generateTestFilesFromFolder(testFolder, folder, data, function (data) {

        request(app)
          .post('/java')
          .send(data)
          .expect('Content-Type', /json/)
          .expect(200)
          .end(function (error, reply) {
            reply.body.outputCompiler.should.equal('Compilation successful');
            reply.body.compilationError.should.equal(false);
            reply.body.id.should.not.equal(undefined);
            reply.body.numTestsPassing.should.equal(1);
            reply.body.numTestsFailing.should.equal(0);
            done();
          });
      });
    });
  });

  it('Kali Java: successful test execution (one file, several tests)', function (done) {
    var folder = "./test/src_examples/java/j_one_file_severalTest";
    var testFolder = folder + "/Root/test";

    utilTest.generateSrcFilesFromFolder(folder, "Java-JUnit", function (data) {
      // get the tests files
      utilTest.generateTestFilesFromFolder(testFolder, folder, data, function (data) {

        request(app)
          .post('/java')
          .send(data)
          .expect('Content-Type', /json/)
          .expect(200)
          .end(function (error, reply) {
            reply.body.outputCompiler.should.equal('Compilation successful');
            reply.body.compilationError.should.equal(false);
            reply.body.id.should.not.equal(undefined);
            reply.body.numTestsPassing.should.equal(2);
            reply.body.numTestsFailing.should.equal(0);
            done();
          });
      });
    });
  });

  it('Kali Java: successful test execution (one file, several tests running several times)', function (done) {
    var folder = "./test/src_examples/java/j_one_file_severalTest";
    var testFolder = folder + "/Root/test";

    utilTest.generateSrcFilesFromFolder(folder, "Java-JUnit", function (data) {
      // get the tests files
      utilTest.generateTestFilesFromFolder(testFolder, folder, data, function (data) {
        data.testArgs = {};
        data.testArgs.testsToRun = "TestB TestC TestB TestC TestB TestC TestB TestC TestB TestC";
        data.testArgs.testsToCompile = "Root/test/*.java Root/src/*";


        request(app)
          .post('/java')
          .send(data)
          .expect('Content-Type', /json/)
          .expect(200)
          .end(function (error, reply) {
            reply.body.outputCompiler.should.equal('Compilation successful');
            reply.body.compilationError.should.equal(false);
            reply.body.id.should.not.equal(undefined);
            reply.body.numTestsPassing.should.equal(10);
            reply.body.numTestsFailing.should.equal(0);
            done();
          })
      });
    });
  });


  it('Kali Java: sending several files (6 tests pass, 14 fail)', function (done) {
    var folder = "./test/src_examples/java/j_several_files";
    var testFolder = folder + "/Root/test";

    utilTest.generateSrcFilesFromFolder(folder, "Java-JUnit", function (data) {
      // get the tests files
      utilTest.generateTestFilesFromFolder(testFolder, folder, data, function (data) {

        request(app)
          .post('/java')
          .send(data)
          .expect('Content-Type', /json/)
          .expect(200)
          .end(function (error, reply) {
            reply.body.outputCompiler.should.equal('Compilation successful');
            reply.body.compilationError.should.equal(false);
            reply.body.id.should.not.equal(undefined);

            reply.body.numTestsPassing.should.equal(6);
            reply.body.numTestsFailing.should.equal(14);

            done();
          });
      });
    });
  });


  it('Kali Java: sending several files (4 test pass, 1 fails)', function (done) {
    var folder = "./test/src_examples/java/j_several_files2";
    var testFolder = folder + "/Root/test";

    utilTest.generateSrcFilesFromFolder(folder, "Java-JUnit", function (data) {
      // get the tests files
      utilTest.generateTestFilesFromFolder(testFolder, folder, data, function (data) {

        request(app)
          .post('/java')
          .send(data)
          .expect('Content-Type', /json/)
          .expect(200)
          .end(function (error, reply) {
            reply.body.outputCompiler.should.equal('Compilation successful');
            reply.body.compilationError.should.equal(false);
            reply.body.id.should.not.equal(undefined);

            reply.body.numTestsPassing.should.equal(4);
            reply.body.numTestsFailing.should.equal(1);

            done();
          });
      });
    });
  });

  it('Kali Java: sending several files (4 test pass, 1 fails) setting arguments ToRun and testsToCompile ', function (done) {
    var folder = "./test/src_examples/java/j_several_files2";
    var testFolder = folder + "/Root/test";

    utilTest.generateSrcFilesFromFolder(folder, "Java-JUnit", function (data) {
      // get the tests files
      utilTest.generateTestFilesFromFolder(testFolder, folder, data, function (data) {
        data.testArgs = {};
        data.testArgs.testsToRun = "Test1 TestB TestC";
        data.testArgs.testsToCompile = "Root/test/Test1.java Root/test/TestB.java Root/test/TestC.java Root/src/*.java";

        request(app)
          .post('/java')
          .send(data)
          .expect('Content-Type', /json/)
          .expect(200)
          .end(function (error, reply) {
            reply.body.outputCompiler.should.equal('Compilation successful');
            reply.body.compilationError.should.equal(false);
            reply.body.id.should.not.equal(undefined);

            reply.body.numTestsPassing.should.equal(4);
            reply.body.numTestsFailing.should.equal(1);

            done();
          });
      });
    });
  });


  it('Kali Java: sending several files (1 test pass, 0 fails) setting arguments testsToRun and testsToCompile ', function (done) {
    var folder = "./test/src_examples/java/j_several_files2";
    var testFolder = folder + "/Root/test";

    utilTest.generateSrcFilesFromFolder(folder, "Java-JUnit", function (data) {
      // get the tests files
      utilTest.generateTestFilesFromFolder(testFolder, folder, data, function (data) {
        data.testArgs = {};
        data.testArgs.testsToRun = "Test1 ";
        data.testArgs.testsToCompile = "Root/test/Test1.java Root/test/TestB.java Root/test/TestC.java Root/src/*.java";

        request(app)
          .post('/java')
          .send(data)
          .expect('Content-Type', /json/)
          .expect(200)
          .end(function (error, reply) {
            reply.body.outputCompiler.should.equal('Compilation successful');
            reply.body.compilationError.should.equal(false);
            reply.body.id.should.not.equal(undefined);

            reply.body.numTestsPassing.should.equal(1);
            reply.body.numTestsFailing.should.equal(0);

            done();
          });
      });
    });
  });


  it('Kali Java: sending several files (2 test pass, 1 fails) setting arguments testsToRun and testsToCompile ', function (done) {
    var folder = "./test/src_examples/java/j_several_files2";
    var testFolder = folder + "/Root/test";

    utilTest.generateSrcFilesFromFolder(folder, "Java-JUnit", function (data) {
      // get the tests files
      utilTest.generateTestFilesFromFolder(testFolder, folder, data, function (data) {
        data.testArgs = {};
        data.testArgs.testsToRun = "Test1 TestB ";
        data.testArgs.testsToCompile = "Root/test/Test1.java Root/test/TestB.java Root/test/TestC.java Root/src/*.java";

        request(app)
          .post('/java')
          .send(data)
          .expect('Content-Type', /json/)
          .expect(200)
          .end(function (error, reply) {
            reply.body.outputCompiler.should.equal('Compilation successful');
            reply.body.compilationError.should.equal(false);
            reply.body.id.should.not.equal(undefined);

            reply.body.numTestsPassing.should.equal(2);
            reply.body.numTestsFailing.should.equal(1);

            done();
          });
      });
    });
  });

  it('Kali Java: sending several files (6 test pass, 4 fails)', function (done) {
    var folder = "./test/src_examples/java/j_several_files3";
    var testFolder = folder + "/Root/test";

    utilTest.generateSrcFilesFromFolder(folder, "Java-JUnit", function (data) {
      // get the tests files
      utilTest.generateTestFilesFromFolder(testFolder, folder, data, function (data) {

        request(app)
          .post('/java')
          .send(data)
          .expect('Content-Type', /json/)
          .expect(200)
          .end(function (error, reply) {
            reply.body.outputCompiler.should.equal('Compilation successful');
            reply.body.compilationError.should.equal(false);
            reply.body.id.should.not.equal(undefined);

            reply.body.numTestsPassing.should.equal(6);
            reply.body.numTestsFailing.should.equal(4);

            done();
          });
      });
    });
  });


  it('Kali Java: sending several files in folders (6 test pass, 4 fails)', function (done) {
    var folder = "./test/src_examples/java/j_several_files_folders";
    var testFolder = folder + "/Root/test";

    utilTest.generateSrcFilesFromFolder(folder, "Java-JUnit", function (data) {
      // get the tests files
      utilTest.generateTestFilesFromFolder(testFolder, folder, data, function (data) {

        request(app)
          .post('/java')
          .send(data)
          .expect('Content-Type', /json/)
          .expect(200)
          .end(function (error, reply) {
            reply.body.outputCompiler.should.equal('Compilation successful');
            reply.body.compilationError.should.equal(false);
            reply.body.id.should.not.equal(undefined);

            reply.body.numTestsPassing.should.equal(1);
            reply.body.numTestsFailing.should.equal(0);

            done();
          });
      });
    });
  });

  it('Kali Java: sending several files in folders (2 test pass, 1 fails)', function (done) {
    var folder = "./test/src_examples/java/j_several_files_folders2";
    var testFolder = folder + "/Root/test";

    utilTest.generateSrcFilesFromFolder(folder, "Java-JUnit", function (data) {
      // get the tests files
      utilTest.generateTestFilesFromFolder(testFolder, folder, data, function (data) {

        request(app)
          .post('/java')
          .send(data)
          .expect('Content-Type', /json/)
          .expect(200)
          .end(function (error, reply) {
            reply.body.outputCompiler.should.equal('Compilation successful');
            reply.body.compilationError.should.equal(false);
            reply.body.id.should.not.equal(undefined);

            reply.body.numTestsPassing.should.equal(2);
            reply.body.numTestsFailing.should.equal(1);

            done();
          });
      });
    });
  });

  it('Kali Java: sending several files in folders for grading (6 tests pass, 14 fail)', function (done) {
    var folder = "./test/src_examples/java/j_grading_tests";
    var testFolder = folder + "/Root/test_submission";

    utilTest.generateSrcFilesFromFolder(folder, "Java-JUnit", function (data) {
      // get the tests files
      utilTest.generateTestFilesFromFolder(testFolder, folder, data, function (data) {

        request(app)
          .post('/java')
          .send(data)
          .expect('Content-Type', /json/)
          .expect(200)
          .end(function (error, reply) {
            reply.body.outputCompiler.should.equal('Compilation successful');
            reply.body.compilationError.should.equal(false);
            reply.body.id.should.not.equal(undefined);

            reply.body.numTestsPassing.should.equal(6);
            reply.body.numTestsFailing.should.equal(14);

            done();
          });
      });
    });
  });

  it('Kali Java: From Folder: testProject1 - FindPair (0 test pass, 2 fails))', function (done) {
    // get the source files
    utilTest.generateSrcFilesFromFolder("./test/src_examples/java/testProject1", "Java-JUnit", function (data) {
      // get the tests files
      utilTest.generateTestFilesFromFolder("./test/src_examples/java/testProject1/Root/test", "./test/src_examples/java/testProject1", data, function (data) {
        request(app)
          .post('/java')
          .send(data)
          .expect('Content-Type', /json/)
          .expect(200)
          .end(function (error, reply) {

            reply.body.outputCompiler.should.equal('Compilation successful');
            reply.body.compilationError.should.equal(false);
            reply.body.id.should.not.equal(undefined);

            reply.body.numTestsPassing.should.equal(0);
            reply.body.numTestsFailing.should.equal(2);

            done();
          });
      });
    });
  });

  it('Kali Java: From Folder: testProject1 - FindPair - SUBMISSION (1 test pass, 7 fails))', function (done) {
    // get the source files
    utilTest.generateSrcFilesFromFolder("./test/src_examples/java/testProject1", "Java-JUnit", function (data) {
      // get the tests files
      utilTest.generateTestFilesFromFolder("./test/src_examples/java/testProject1/Root/test_submission", "./test/src_examples/java/testProject1", data, function (data) {
        request(app)
          .post('/java')
          .send(data)
          .expect('Content-Type', /json/)
          .expect(200)
          .end(function (error, reply) {
            reply.body.outputCompiler.should.equal('Compilation successful');
            reply.body.compilationError.should.equal(false);
            reply.body.id.should.not.equal(undefined);

            reply.body.numTestsPassing.should.equal(1);
            reply.body.numTestsFailing.should.equal(7);

            done();
          });
      });
    });
  });

  it('Kali Java: From Folder: testProject3 - Find Template (3 test pass, 0 fails))', function (done) {
    // get the source files
    utilTest.generateSrcFilesFromFolder("./test/src_examples/java/testProject3", "Java-JUnit", function (data) {
      // get the tests files
      utilTest.generateTestFilesFromFolder("./test/src_examples/java/testProject3/Root/test", "./test/src_examples/java/testProject3", data, function (data) {
        request(app)
          .post('/java')
          .send(data)
          .expect('Content-Type', /json/)
          .expect(200)
          .end(function (error, reply) {

            reply.body.outputCompiler.should.equal('Compilation successful');
            reply.body.compilationError.should.equal(false);
            reply.body.id.should.not.equal(undefined);

            reply.body.numTestsPassing.should.equal(3);
            reply.body.numTestsFailing.should.equal(0);

            done();
          });
      });
    });
  });

  it('Kali Java: From Folder: testProject3 - Find Template (1 test pass, 2 fails))', function (done) {
    // get the source files
    utilTest.generateSrcFilesFromFolder("./test/src_examples/java/testProject3", "Java-JUnit", function (data) {
      // get the tests files
      utilTest.generateTestFilesFromFolder("./test/src_examples/java/testProject3/Root/test_submission", "./test/src_examples/java/testProject3", data, function (data) {
        request(app)
          .post('/java')
          .send(data)
          .expect('Content-Type', /json/)
          .expect(200)
          .end(function (error, reply) {

            reply.body.outputCompiler.should.equal('Compilation successful');
            reply.body.compilationError.should.equal(false);
            reply.body.id.should.not.equal(undefined);

            reply.body.numTestsPassing.should.equal(1);
            reply.body.numTestsFailing.should.equal(2);

            done();
          });
      });
    });
  });
});

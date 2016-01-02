var app = require('../server/server.js'),
  request = require('supertest'),
  express = require('express'),
  should = require('should'),
  bodyParser = require('body-parser'),
  fs = require('fs'),
  utilTest = require('./util/utilForTesting.js'),
  util = require('../server/util.js');

app.use(bodyParser());


describe('Test Kali with Haskell ', function () {

  this.timeout(1000 * 50);

  it('Kali Haskell: error in compilation (one file)', function (done) {
    // get the source files
    utilTest.generateSrcFilesFromFolder("./test/src_examples/haskell/error_one_file", "Haskell-HSpec", function (data) {
      // get the tests files
      utilTest.generateTestFilesFromFolder("./test/src_examples/haskell/error_one_file/Root/Test", "./test/src_examples/haskell/error_one_file", data, function (data) {
        request(app)
          .post('/haskell')
          .send(data)
          .expect('Content-Type', /json/)
          .expect(200)
          .end(function (error, reply) {
            reply.body.compilationError.should.equal(true);
            reply.body.output.should.not.equal(undefined);
            reply.body.output.should.equal("Error in codeboard setting file; check the file is valid JSON file");
            reply.body.numTestsPassing.should.equal(-1);
            reply.body.numTestsFailing.should.equal(-1);

            done();
          });
      });
    });
  });

  it('Kali Haskell: error in compilation (several files)', function (done) {
    // get the source files
    utilTest.generateSrcFilesFromFolder("./test/src_examples/haskell/error_several_files", "Haskell-HSpec", function (data) {
      // get the tests files
      utilTest.generateTestFilesFromFolder("./test/src_examples/haskell/error_several_files/Root/Test", "./test/src_examples/haskell/error_several_files", data, function (data) {
        request(app)
          .post('/haskell')
          .send(data)
          .expect('Content-Type', /json/)
          .expect(200)
          .end(function (error, reply) {
            reply.body.outputCompiler.should.not.equal('Compilation successful');
            reply.body.compilationError.should.equal(true);
            reply.body.id.should.not.equal(undefined);
            reply.body.output.should.equal("Compilation error, HSpec (for Haskell) was not run.");
            reply.body.numTestsPassing.should.equal(-1);
            reply.body.numTestsFailing.should.equal(-1);
            done();
          });
      });
    });
  });

  it('Kali Haskell: successful test execution (one file, one test)', function (done) {
    // get the source files
    utilTest.generateSrcFilesFromFolder("./test/src_examples/haskell/one_file", "Haskell-HSpec", function (data) {
      // get the tests files
      utilTest.generateTestFilesFromFolder("./test/src_examples/haskell/one_file/Root/Test", "./test/src_examples/haskell/one_file", data, function (data) {
        request(app)
          .post('/haskell')
          .send(data)
          .expect('Content-Type', /json/)
          .expect(200)
          .end(function (error, reply) {
            reply.body.compilationError.should.equal(false);
            reply.body.id.should.not.equal(undefined);
            reply.body.numTestsPassing.should.equal(1);
            reply.body.numTestsFailing.should.equal(0);

            done();
          })
      });
    });
  });

  it('Kali Haskell: successful test execution (one file, several tests)', function (done) {
    // get the source files
    utilTest.generateSrcFilesFromFolder("./test/src_examples/haskell/one_file_severalTests", "Haskell-HSpec", function (data) {
      // get the tests files
      utilTest.generateTestFilesFromFolder("./test/src_examples/haskell/one_file_severalTests/Root/Test", "./test/src_examples/haskell/one_file_severalTests", data, function (data) {
        request(app)
          .post('/haskell')
          .send(data)
          .expect('Content-Type', /json/)
          .expect(200)
          .end(function (error, reply) {
            reply.body.compilationError.should.equal(false);
            reply.body.id.should.not.equal(undefined);
            reply.body.numTestsPassing.should.equal(2);
            reply.body.numTestsFailing.should.equal(1);

            done();
          })
      });
    });
  });

  it('Kali Haskell: successful test execution (one file, several tests running several times -6 run,3 fail-)', function (done) {
    // get the source files
    utilTest.generateSrcFilesFromFolder("./test/src_examples/haskell/one_file_severalTests", "Haskell-HSpec", function (data) {
      // get the tests files
      utilTest.generateTestFilesFromFolder("./test/src_examples/haskell/one_file_severalTests/Root/Test", "./test/src_examples/haskell/one_file_severalTests", data, function (data) {
        // this is a special case of the previous test
        data.testArgs = {};
        data.testArgs.testsToRun = "runhaskell Root/Test/Test1SpecWE.hs; runhaskell Root/Test/Test1SpecWE.hs; runhaskell Root/Test/Test1SpecWE.hs;";
        data.testArgs.testsToCompile = " ./Root/Test/Test1SpecWE.hs ./Root/Src/Main.hs";

        request(app)
          .post('/haskell')
          .send(data)
          .expect('Content-Type', /json/)
          .expect(200)
          .end(function (error, reply) {
            reply.body.compilationError.should.equal(false);
            reply.body.id.should.not.equal(undefined);
            reply.body.numTestsPassing.should.equal(6);
            reply.body.numTestsFailing.should.equal(3);

            done();
          })
      });
    });
  });

  it('Kali Haskell: successful test execution (one file, several tests running several times -6 run,3 fail-)', function (done) {
    // get the source files
    utilTest.generateSrcFilesFromFolder("./test/src_examples/haskell/one_file_severalTests", "Haskell-HSpec", function (data) {
      // get the tests files
      utilTest.generateTestFilesFromFolder("./test/src_examples/haskell/one_file_severalTests/Root/Test", "./test/src_examples/haskell/one_file_severalTests", data, function (data) {
        // this is a special case of the previous test
        data.testArgs = {};
        data.testArgs.testsToRun = "runhaskell ./Root/Test/Test1SpecWE.hs; runhaskell ./Root/Test/Test1SpecWE.hs; runhaskell ./Root/Test/Test1SpecWE.hs; runhaskell ./Root/Test/Test1SpecWE.hs; runhaskell ./Root/Test/Test1SpecWE.hs;";
        data.testArgs.testsToCompile = " ./Root/Test/Test1SpecWE.hs ./Root/Src/Main.hs";
        request(app)
          .post('/haskell')
          .send(data)
          .expect('Content-Type', /json/)
          .expect(200)
          .end(function (error, reply) {
            reply.body.compilationError.should.equal(false);
            reply.body.id.should.not.equal(undefined);
            reply.body.numTestsPassing.should.equal(10);
            reply.body.numTestsFailing.should.equal(5);

            done();
          })
      });
    });
  });

  it('Kali Haskell: successful test execution (one file, one tests running several times -2 run,1 fail-)', function (done) {
    // get the source files
    utilTest.generateSrcFilesFromFolder("./test/src_examples/haskell/one_file_severalTests", "Haskell-HSpec", function (data) {
      // get the tests files
      utilTest.generateTestFilesFromFolder("./test/src_examples/haskell/one_file_severalTests/Root/Test", "./test/src_examples/haskell/one_file_severalTests", data, function (data) {
        // this is a special case of the previous test
        data.testArgs = {};
        data.testArgs.testsToRun = "runhaskell ./Root/Test/Test1SpecWE.hs;";
        data.testArgs.testsToCompile = " ./Root/Test/Test1SpecWE.hs ./Root/Main.hs";
        request(app)
          .post('/haskell')
          .send(data)
          .expect('Content-Type', /json/)
          .expect(200)
          .end(function (error, reply) {
            reply.body.compilationError.should.equal(false);
            reply.body.id.should.not.equal(undefined);
            reply.body.numTestsPassing.should.equal(2);
            reply.body.numTestsFailing.should.equal(1);

            done();
          })
      });
    });
  });

  it('Kali Haskell: sending several files (3 tests run, 1 fail)', function (done) {
    // get the source files
    utilTest.generateSrcFilesFromFolder("./test/src_examples/haskell/several_files", "Haskell-HSpec", function (data) {
      // get the tests files
      utilTest.generateTestFilesFromFolder("./test/src_examples/haskell/several_files/Root/Test", "./test/src_examples/haskell/several_files", data, function (data) {
        request(app)
          .post('/haskell')
          .send(data)
          .expect('Content-Type', /json/)
          .expect(200)
          .end(function (error, reply) {
            reply.body.compilationError.should.equal(false);
            reply.body.id.should.not.equal(undefined);
            reply.body.output.should.not.equal(undefined);
            reply.body.numTestsPassing.should.equal(3);
            reply.body.numTestsFailing.should.equal(1);
            done();
          });
      });
    });
  });

  it('Kali Haskell: sending several files (13 tests run, 4 fail)', function (done) {
    // get the source files
    utilTest.generateSrcFilesFromFolder("./test/src_examples/haskell/several_files2", "Haskell-HSpec", function (data) {
      // get the tests files
      utilTest.generateTestFilesFromFolder("./test/src_examples/haskell/several_files2/Root/Test", "./test/src_examples/haskell/several_files2", data, function (data) {
        request(app)
          .post('/haskell')
          .send(data)
          .expect('Content-Type', /json/)
          .expect(200)
          .end(function (error, reply) {
            reply.body.compilationError.should.equal(false);
            reply.body.id.should.not.equal(undefined);
            reply.body.output.should.not.equal(undefined);
            reply.body.numTestsPassing.should.equal(13);
            reply.body.numTestsFailing.should.equal(4);

            done();
          });
      });
    });
  });

  it('Kali Haskell: sending several files (13 test run, 4 fails) setting arguments testsToRun and testsToCompile ', function (done) {
    // get the source files
    utilTest.generateSrcFilesFromFolder("./test/src_examples/haskell/several_files2", "Haskell-HSpec", function (data) {
      // get the tests files
      utilTest.generateTestFilesFromFolder("./test/src_examples/haskell/several_files2/Root/Test", "./test/src_examples/haskell/several_files2", data, function (data) {
        data.testArgs = {};
        data.testArgs.testsToRun = "runhaskell ./Root/Test/Test1Spec.hs; runhaskell ./Root/Test/Test2Spec.hs; runhaskell ./Root/Test/Test3.hs;";
        data.testArgs.testsToCompile = " ./Root/Test/Test1Spec.hs ./Root/Test/Test2Spec.hs ./Root/Src/Main.hs ./Root/Src/B.hs ./Root/Src/C.hs";
        request(app)
          .post('/haskell')
          .send(data)
          .expect('Content-Type', /json/)
          .expect(200)
          .end(function (error, reply) {
            reply.body.compilationError.should.equal(false);
            reply.body.id.should.not.equal(undefined);
            reply.body.numTestsPassing.should.equal(13);
            reply.body.numTestsFailing.should.equal(4);

            done();
          });
      });
    });
  });

  it('Kali Haskell: sending several files (13 test run, 4 fails) setting arguments testsToRun and testsToCompile ', function (done) {
    // get the source files
    utilTest.generateSrcFilesFromFolder("./test/src_examples/haskell/several_files2", "Haskell-HSpec", function (data) {
      // get the tests files
      utilTest.generateTestFilesFromFolder("./test/src_examples/haskell/several_files2/Root/Test", "./test/src_examples/haskell/several_files2", data, function (data) {
        data.testArgs = {};
        data.testArgs.testsToRun = "runhaskell ./Root/Test/Test1Spec.hs; runhaskell ./Root/Test/Test2Spec.hs;";
        data.testArgs.testsToCompile = " ./Root/Test/Test1Spec.hs ./Root/Test/Test2Spec.hs ./Root/Src/Main.hs ./Root/Src/B.hs ./Root/Src/C.hs";
        request(app)
          .post('/haskell')
          .send(data)
          .expect('Content-Type', /json/)
          .expect(200)
          .end(function (error, reply) {
            reply.body.compilationError.should.equal(false);
            reply.body.id.should.not.equal(undefined);
            reply.body.numTestsPassing.should.equal(13);
            reply.body.numTestsFailing.should.equal(0);

            done();
          });
      });
    });
  });


  it('Kali Haskell: sending several files (26 test run, 12 fails) setting arguments testsToRun and testsToCompile ', function (done) {
    // get the source files
    utilTest.generateSrcFilesFromFolder("./test/src_examples/haskell/several_files2", "Haskell-HSpec", function (data) {
      // get the tests files
      utilTest.generateTestFilesFromFolder("./test/src_examples/haskell/several_files2/Root/Test", "./test/src_examples/haskell/several_files2", data, function (data) {
        data.testArgs = {};
        data.testArgs.testsToRun = "runhaskell ./Root/Test/Test1Spec.hs; runhaskell ./Root/Test/Test1Spec.hs; runhaskell ./Root/Test/Test2Spec.hs; runhaskell ./Root/Test/Test3.hs; runhaskell ./Root/Test/Test3.hs; runhaskell ./Root/Test/Test3.hs;";
        data.testArgs.testsToCompile = " ./Root/Test/Test1Spec.hs ./Root/Test/Test2Spec.hs ./Root/Main.hs ./Root/Src/B.hs ./Root/Src/C.hs";
        request(app)
          .post('/haskell')
          .send(data)
          .expect('Content-Type', /json/)
          .expect(200)
          .end(function (error, reply) {
            reply.body.compilationError.should.equal(false);
            reply.body.id.should.not.equal(undefined);
            reply.body.numTestsPassing.should.equal(25);
            reply.body.numTestsFailing.should.equal(12);
            done();
          });
      });
    });
  });


  it('Kali Haskell: sending several files (12 test run, 0 fails)', function (done) {
    // get the source files
    utilTest.generateSrcFilesFromFolder("./test/src_examples/haskell/several_files3", "Haskell-HSpec", function (data) {
      // get the tests files
      utilTest.generateTestFilesFromFolder("./test/src_examples/haskell/several_files3/Root/Test", "./test/src_examples/haskell/several_files3", data, function (data) {
        request(app)
          .post('/haskell')
          .send(data)
          .expect('Content-Type', /json/)
          .expect(200)
          .end(function (error, reply) {
            reply.body.compilationError.should.equal(false);
            reply.body.id.should.not.equal(undefined);
            reply.body.numTestsPassing.should.equal(12);
            reply.body.numTestsFailing.should.equal(0);

            done();
          });
      });
    });
  });


  it('Kali Haskell: sending several files in folders (1 successful)', function (done) {
    // get the source files
    utilTest.generateSrcFilesFromFolder("./test/src_examples/haskell/several_files_folders", "Haskell-HSpec", function (data) {
      // get the tests files
      utilTest.generateTestFilesFromFolder("./test/src_examples/haskell/several_files_folders/Root/Test", "./test/src_examples/haskell/several_files_folders", data, function (data) {
        request(app)
          .post('/haskell')
          .send(data)
          .expect('Content-Type', /json/)
          .expect(200)
          .end(function (error, reply) {

            reply.body.compilationError.should.equal(false);
            reply.body.id.should.not.equal(undefined);

            reply.body.numTestsPassing.should.equal(1);
            reply.body.numTestsFailing.should.equal(0);

            done();
          });
      });
    });
  });


  it('Kali Haskell: sending several files in folders (2 successful; 1 error)', function (done) {
    // get the source files
    utilTest.generateSrcFilesFromFolder("./test/src_examples/haskell/several_files_folders2", "Haskell-HSpec", function (data) {
      // get the tests files
      utilTest.generateTestFilesFromFolder("./test/src_examples/haskell/several_files_folders2/Root/Test", "./test/src_examples/haskell/several_files_folders2", data, function (data) {
        request(app)
          .post('/haskell')
          .send(data)
          .expect('Content-Type', /json/)
          .expect(200)
          .end(function (error, reply) {
            reply.body.compilationError.should.equal(false);
            reply.body.id.should.not.equal(undefined);

            reply.body.numTestsPassing.should.equal(2);
            reply.body.numTestsFailing.should.equal(1);

            done();
          });
      });
    });
  });

  it('Kali Haskell: template project - user tests (3 successful; 0 error)', function (done) {
    // get the source files
    utilTest.generateSrcFilesFromFolder("./test/src_examples/haskell/template_project", "Haskell-HSpec", function (data) {
      // get the tests files
      utilTest.generateTestFilesFromFolder("./test/src_examples/haskell/template_project/Root/Test", "./test/src_examples/haskell/template_project", data, function (data) {
        request(app)
          .post('/haskell')
          .send(data)
          .expect('Content-Type', /json/)
          .expect(200)
          .end(function (error, reply) {
            reply.body.compilationError.should.equal(false);
            reply.body.id.should.not.equal(undefined);

            reply.body.numTestsPassing.should.equal(3);
            reply.body.numTestsFailing.should.equal(0);

            done();
          });
      });
    });
  });

  it('Kali Haskell: template project - grading tests (4 successful; 0 error)', function (done) {
    // get the source files
    utilTest.generateSrcFilesFromFolder("./test/src_examples/haskell/template_project", "Haskell-HSpec", function (data) {
      // get the tests files
      utilTest.generateTestFilesFromFolder("./test/src_examples/haskell/template_project/Root/TestSubmission", "./test/src_examples/haskell/template_project", data, function (data) {
        request(app)
          .post('/haskell')
          .send(data)
          .expect('Content-Type', /json/)
          .expect(200)
          .end(function (error, reply) {
            reply.body.compilationError.should.equal(false);
            reply.body.id.should.not.equal(undefined);

            reply.body.numTestsPassing.should.equal(4);
            reply.body.numTestsFailing.should.equal(0);

            done();
          });
      });
    });
  });
});

var app = require('../server/server.js'),
  request = require('supertest'),
  express = require('express'),
  should = require('should'),
  bodyParser = require('body-parser'),
  fs = require('fs'),
  utilTest = require('./util/utilForTesting.js'),
  util = require('../server/util.js');

app.use(bodyParser());

describe('Test Kali with Python ', function () {

  this.timeout(1000 * 50);

  it('Kali Python: error running test (0 run, 0 failure, 1 error)', function (done) {
    // get the source files
    var folder = "./test/src_examples/python/py_error_one_file";
    var testFolder = folder + "/Root/test";
    utilTest.generateSrcFilesFromFolder(folder, "Python", function (data) {
      // get the tests files
      utilTest.generateTestFilesFromFolder(testFolder, folder, data, function (data) {
        request(app)
          .post('/python')
          .send(data)
          .expect('Content-Type', /json/)
          .expect(200)
          .end(function (error, reply) {
            reply.body.id.should.not.equal(undefined);
            reply.body.output.should.not.equal(undefined);
            reply.body.numTestsPassing.should.equal(0);
            reply.body.numTestsFailing.should.equal(0);
            reply.body.numTestsErrors.should.equal(1);
            done();
          });
      });
    });
  });

  it('Kali Python: error running test (0 run, 0 failure, 11 errors)', function (done) {
    // get the source files
    var folder = "./test/src_examples/python/py_error_one_file_manyErrors";
    var testFolder = folder + "/Root/test";
    utilTest.generateSrcFilesFromFolder(folder, "Python-UnitTest", function (data) {
      // get the tests files
      utilTest.generateTestFilesFromFolder(testFolder, folder, data, function (data) {
        request(app)
          .post('/python')
          .send(data)
          .expect('Content-Type', /json/)
          .expect(200)
          .end(function (error, reply) {
            reply.body.id.should.not.equal(undefined);
            reply.body.output.should.not.equal(undefined);
            reply.body.numTestsPassing.should.equal(0);
            reply.body.numTestsFailing.should.equal(0);
            reply.body.numTestsErrors.should.equal(11);
            done();
          });
      });
    });
  });

  it('Kali Python: error running test (0 run, 1 failure, 1 error)', function (done) {
    var folder = "./test/src_examples/python/py_error_and_fail";
    var testFolder = folder + "/Root/test";
    utilTest.generateSrcFilesFromFolder(folder, "Python-UnitTest", function (data) {
      // get the tests files
      utilTest.generateTestFilesFromFolder(testFolder, folder, data, function (data) {
        request(app)
          .post('/python')
          .send(data)
          .expect('Content-Type', /json/)
          .expect(200)
          .end(function (error, reply) {
            reply.body.id.should.not.equal(undefined);
            reply.body.output.should.not.equal(undefined);
            reply.body.numTestsPassing.should.equal(0);
            reply.body.numTestsFailing.should.equal(1);
            reply.body.numTestsErrors.should.equal(1);
            done();
          });
      });
    });
  });


  it('Kali Python: error running test (0 run, 12 failure, 13 error)', function (done) {
    var folder = "./test/src_examples/python/py_error_and_fail_manyErrors";
    var testFolder = folder + "/Root/test";
    utilTest.generateSrcFilesFromFolder(folder, "Python-UnitTest", function (data) {
      // get the tests files
      utilTest.generateTestFilesFromFolder(testFolder, folder, data, function (data) {
        request(app)
          .post('/python')
          .send(data)
          .expect('Content-Type', /json/)
          .expect(200)
          .end(function (error, reply) {
            reply.body.id.should.not.equal(undefined);
            reply.body.output.should.not.equal(undefined);
            reply.body.numTestsPassing.should.equal(0);
            reply.body.numTestsFailing.should.equal(12);
            reply.body.numTestsErrors.should.equal(11);
            done();
          });
      });
    });
  });

  it('Kali Python: error running test (1 run, 1 failure, 1 error)', function (done) {
    var folder = "./test/src_examples/python/py_error_and_fail_and_run";
    var testFolder = folder + "/Root/test";
    utilTest.generateSrcFilesFromFolder(folder, "Python-UnitTest", function (data) {
      // get the tests files
      utilTest.generateTestFilesFromFolder(testFolder, folder, data, function (data) {
        request(app)
          .post('/python')
          .send(data)
          .expect('Content-Type', /json/)
          .expect(200)
          .end(function (error, reply) {
            reply.body.id.should.not.equal(undefined);
            reply.body.output.should.not.equal(undefined);
            reply.body.numTestsPassing.should.equal(1);
            reply.body.numTestsFailing.should.equal(1);
            reply.body.numTestsErrors.should.equal(1);
            done();
          });
      });
    });
  });

  it('Kali Python: error running test in several files(0 run, 0 failure, 1 error)', function (done) {
    var folder = "./test/src_examples/python/py_error_several_files";
    var testFolder = folder + "/Root/test";
    utilTest.generateSrcFilesFromFolder(folder, "Python-UnitTest", function (data) {
      // get the tests files
      utilTest.generateTestFilesFromFolder(testFolder, folder, data, function (data) {
        request(app)
          .post('/python')
          .send(data)
          .expect('Content-Type', /json/)
          .expect(200)
          .end(function (error, reply) {
            reply.body.id.should.not.equal(undefined);
            reply.body.output.should.not.equal(undefined);
            reply.body.numTestsPassing.should.equal(0);
            reply.body.numTestsFailing.should.equal(0);
            reply.body.numTestsErrors.should.equal(1);
            done();
          });
      });
    });
  });

  it('Kali Python: successful test execution (one file, one test)', function (done) {
    var folder = "./test/src_examples/python/py_one_file";
    var testFolder = folder + "/Root/test";
    utilTest.generateSrcFilesFromFolder(folder, "Python-UnitTest", function (data) {
      // get the tests files
      utilTest.generateTestFilesFromFolder(testFolder, folder, data, function (data) {
        request(app)
          .post('/python')
          .send(data)
          .expect('Content-Type', /json/)
          .expect(200)
          .end(function (error, reply) {
            reply.body.id.should.not.equal(undefined);
            reply.body.output.should.not.equal(undefined);
            reply.body.numTestsPassing.should.equal(3);
            reply.body.numTestsFailing.should.equal(0);
            reply.body.numTestsErrors.should.equal(0);
            done();
          })
      });
    });
  });

  it('Kali Python: successful test execution (one file, several tests)', function (done) {
    var folder = "./test/src_examples/python/py_one_file_severalTests";
    var testFolder = folder + "/Root/test";
    utilTest.generateSrcFilesFromFolder(folder, "Python-UnitTest", function (data) {
      // get the tests files
      utilTest.generateTestFilesFromFolder(testFolder, folder, data, function (data) {
        request(app)
          .post('/python')
          .send(data)
          .expect('Content-Type', /json/)
          .expect(200)
          .end(function (error, reply) {
            reply.body.id.should.not.equal(undefined);
            reply.body.output.should.not.equal(undefined);
            reply.body.numTestsPassing.should.equal(4);
            reply.body.numTestsFailing.should.equal(2);
            reply.body.numTestsErrors.should.equal(0);
            done();
          });
      });
    });
  });

  it('Kali Python: successful test execution (one file, several tests running several times)', function (done) {
    var folder = "./test/src_examples/python/py_one_file_severalTests";
    var testFolder = folder + "/Root/test";
    utilTest.generateSrcFilesFromFolder(folder, "Python-UnitTest", function (data) {
      // get the tests files
      utilTest.generateTestFilesFromFolder(testFolder, folder, data, function (data) {
        // this is a special case of the previous test
        data.testArgs = {};
        data.testArgs.testsToRun = "Root.test.test1 Root.test.test2 Root.test.test1 Root.test.test2 " +
          "Root.test.test1 Root.test.test2 Root.test.test1 Root.test.test2 Root.test.test1 " +
          "Root.test.test2 Root.test.test1 Root.test.test2";
        request(app)
          .post('/python')
          .send(data)
          .expect('Content-Type', /json/)
          .expect(200)
          .end(function (error, reply) {
            reply.body.id.should.not.equal(undefined);
            reply.body.output.should.not.equal(undefined);
            reply.body.numTestsPassing.should.equal(24);
            reply.body.numTestsFailing.should.equal(12);
            reply.body.numTestsErrors.should.equal(0);
            done();
          });
      });
    });
  });


  it('Kali Python: sending several files (4 tests run, 2 fail)', function (done) {
    var folder = "./test/src_examples/python/py_several_files";
    var testFolder = folder + "/Root/test";
    utilTest.generateSrcFilesFromFolder(folder, "Python-UnitTest", function (data) {
      // get the tests files
      utilTest.generateTestFilesFromFolder(testFolder, folder, data, function (data) {
        request(app)
          .post('/python')
          .send(data)
          .expect('Content-Type', /json/)
          .expect(200)
          .end(function (error, reply) {
            reply.body.id.should.not.equal(undefined);
            reply.body.output.should.not.equal(undefined);
            reply.body.numTestsPassing.should.equal(4);
            reply.body.numTestsFailing.should.equal(2);
            reply.body.numTestsErrors.should.equal(0);

            done();
          });
      });
    });
  });


  it('Kali Python: sending several files (18 test run, 13 fails)', function (done) {
    var folder = "./test/src_examples/python/py_several_files2";
    var testFolder = folder + "/Root/test";
    utilTest.generateSrcFilesFromFolder(folder, "Python-UnitTest", function (data) {
      // get the tests files
      utilTest.generateTestFilesFromFolder(testFolder, folder, data, function (data) {
        request(app)
          .post('/python')
          .send(data)
          .expect('Content-Type', /json/)
          .expect(200)
          .end(function (error, reply) {
            reply.body.id.should.not.equal(undefined);
            reply.body.output.should.not.equal(undefined);
            reply.body.numTestsPassing.should.equal(18);
            reply.body.numTestsFailing.should.equal(13);
            reply.body.numTestsErrors.should.equal(0);
            done();
          });
      });
    });
  });


  it('Kali Python: sending several files (10 test run, 7 fails) setting arguments testsToRun and testsToCompile ', function (done) {
    var folder = "./test/src_examples/python/py_several_files2";
    var testFolder = folder + "/Root/test";
    utilTest.generateSrcFilesFromFolder(folder, "Python-UnitTest", function (data) {
      // get the tests files
      utilTest.generateTestFilesFromFolder(testFolder, folder, data, function (data) {
        data.testArgs = {};
        data.testArgs.testsToRun = "Root.test.test1 Root.test.test5 Root.test.test6";
        request(app)
          .post('/python')
          .send(data)
          .expect('Content-Type', /json/)
          .expect(200)
          .end(function (error, reply) {
            reply.body.id.should.not.equal(undefined);
            reply.body.output.should.not.equal(undefined);
            reply.body.numTestsPassing.should.equal(10);
            reply.body.numTestsFailing.should.equal(7);
            reply.body.numTestsErrors.should.equal(0);

            done();
          });
      });
    });
  });

  it('Kali Python: sending several files (4 test run, 0 fails) setting arguments testsToRun and testsToCompile ', function (done) {
    var folder = "./test/src_examples/python/py_several_files2";
    var testFolder = folder + "/Root/test";
    utilTest.generateSrcFilesFromFolder(folder, "Python-UnitTest", function (data) {
      // get the tests files
      utilTest.generateTestFilesFromFolder(testFolder, folder, data, function (data) {
        data.testArgs = {};
        data.testArgs.testsToRun = "Root.test.test3 Root.test.test3";
        request(app)
          .post('/python')
          .send(data)
          .expect('Content-Type', /json/)
          .expect(200)
          .end(function (error, reply) {
            reply.body.id.should.not.equal(undefined);
            reply.body.output.should.not.equal(undefined);
            reply.body.numTestsPassing.should.equal(4);
            reply.body.numTestsFailing.should.equal(0);
            reply.body.numTestsErrors.should.equal(0);

            done();
          });
      });
    });
  });

  it('Kali Python: sending several files (14 test run, 10 fails) setting arguments testsToRun and testsToCompile ', function (done) {
    var folder = "./test/src_examples/python/py_several_files2";
    var testFolder = folder + "/Root/test";
    utilTest.generateSrcFilesFromFolder(folder, "Python-UnitTest", function (data) {
      // get the tests files
      utilTest.generateTestFilesFromFolder(testFolder, folder, data, function (data) {
        data.testArgs = {};
        data.testArgs.testsToRun = "Root.test.test1 Root.test.test2 Root.test.test3 " +
          "Root.test.test5 Root.test.test6";
        request(app)
          .post('/python')
          .send(data)
          .expect('Content-Type', /json/)
          .expect(200)
          .end(function (error, reply) {
            reply.body.id.should.not.equal(undefined);
            reply.body.output.should.not.equal(undefined);
            reply.body.numTestsPassing.should.equal(14);
            reply.body.numTestsFailing.should.equal(10);
            reply.body.numTestsErrors.should.equal(0);

            done();
          });
      });
    });
  });

  it('Kali Python: sending several files (2 test run, 0 fails) setting arguments testsToRun and testsToCompile ', function (done) {
    var folder = "./test/src_examples/python/py_several_files2";
    var testFolder = folder + "/Root/test";
    utilTest.generateSrcFilesFromFolder(folder, "Python-UnitTest", function (data) {
      // get the tests files
      utilTest.generateTestFilesFromFolder(testFolder, folder, data, function (data) {
        data.testArgs = {};
        data.testArgs.testsToRun = "Root.test.test3";
        request(app)
          .post('/python')
          .send(data)
          .expect('Content-Type', /json/)
          .expect(200)
          .end(function (error, reply) {
            reply.body.id.should.not.equal(undefined);
            reply.body.output.should.not.equal(undefined);
            reply.body.numTestsPassing.should.equal(2);
            reply.body.numTestsFailing.should.equal(0);
            reply.body.numTestsErrors.should.equal(0);

            done();
          });
      });
    });
  });

  it('Kali Python: sending several files in folders (1 successful)', function (done) {
    var folder = "./test/src_examples/python/py_several_files_folder";
    var testFolder = folder + "/Root/test";
    utilTest.generateSrcFilesFromFolder(folder, "Python-UnitTest", function (data) {
      // get the tests files
      utilTest.generateTestFilesFromFolder(testFolder, folder, data, function (data) {
        request(app)
          .post('/python')
          .send(data)
          .expect('Content-Type', /json/)
          .expect(200)
          .end(function (error, reply) {
            reply.body.id.should.not.equal(undefined);
            reply.body.output.should.not.equal(undefined);
            reply.body.numTestsPassing.should.equal(1);
            reply.body.numTestsFailing.should.equal(0);
            reply.body.numTestsErrors.should.equal(0);

            done();
          });
      });
    });
  });

  it('Kali Python: sending several files in folders (1 successful)', function (done) {
    var folder = "./test/src_examples/python/py_several_files_folder2";
    var testFolder = folder + "/Root/test";
    utilTest.generateSrcFilesFromFolder(folder, "Python-UnitTest", function (data) {
      // get the tests files
      utilTest.generateTestFilesFromFolder(testFolder, folder, data, function (data) {
        request(app)
          .post('/python')
          .send(data)
          .expect('Content-Type', /json/)
          .expect(200)
          .end(function (error, reply) {
            reply.body.id.should.not.equal(undefined);
            reply.body.output.should.not.equal(undefined);
            reply.body.numTestsPassing.should.equal(3);
            reply.body.numTestsFailing.should.equal(0);
            reply.body.numTestsErrors.should.equal(0);

            done();
          });
      });
    });
  });

  it('Kali Python: template project', function (done) {
    var folder = "./test/src_examples/python/template";
    var testFolder = folder + "/Root/test";
    utilTest.generateSrcFilesFromFolder(folder, "Python-UnitTest", function (data) {
      // get the tests files
      utilTest.generateTestFilesFromFolder(testFolder, folder, data, function (data) {
        request(app)
          .post('/python')
          .send(data)
          .expect('Content-Type', /json/)
          .expect(200)
          .end(function (error, reply) {
            reply.body.id.should.not.equal(undefined);
            reply.body.output.should.not.equal(undefined);
            reply.body.numTestsPassing.should.equal(1);
            reply.body.numTestsFailing.should.equal(3);
            reply.body.numTestsErrors.should.equal(0);

            done();
          });
      });
    });
  });
});

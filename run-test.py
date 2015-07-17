#!/usr/bin/python

import os
import sys
import subprocess

devnull = open('/dev/null')
verbose = False
testbuild = os.path.isfile('./testbuild')

class Failure(Exception):
    pass

def runTest(filename):
    if verbose:
        print "Testing " + filename + "...",
    sys.stdout.flush()

    exIn = "# INPUT #"
    exOut = "# EXPECTED OUTPUT #"
    exCmpErr = "# EXPECTED COMPILATION ERROR #"
    exRunErr = "# EXPECTED RUNTIME ERROR #"
    expectations = {
        exIn: False,
        exOut: False,
        exCmpErr: False,
        exRunErr: False,
    }
    current_section = False
    with open(filename) as f:
        for line in f:
            def process_line():
                for key in expectations:
                    if line.strip() == key:
                        if expectations[key] != False:
                            raise Exception(key + ' redeclared!')
                        expectations[key] = []
                        return key
                if current_section:
                    if line.startswith('# '):
                        expectations[current_section].append(line[2:])
                        return current_section
                    elif line == '#\n':
                        expectations[current_section].append('\n')
                        return current_section
                    else:
                        return False
            current_section = process_line()
    if expectations[exCmpErr] and expectations[exOut]:
        raise Exception("Requesting both compilation and any output is insane!")
    if expectations[exCmpErr] and expectations[exRunErr]:
        raise Exception("Requesting both compilation and runtime error is insane!")
    expectFail = bool(expectations[exCmpErr] != False or expectations[exRunErr] != False)

    expectedOut = ''.join(expectations[exOut] or [])
    expectedErr = ''.join((expectations[exCmpErr] or []) or (expectations[exRunErr] or []))

    cmd = ['./interpreter', filename]
    #if testbuild:
        #cmd += ['+RTS', '-xc']
    test = subprocess.Popen(cmd, stdin=subprocess.PIPE, stdout=subprocess.PIPE, stderr=subprocess.PIPE)
    (out, errout) = test.communicate(input=''.join(expectations[exIn] or []))
    test.wait()
    try:
        if expectFail:
            if test.returncode == 0:
                    raise Failure('Unexpected compilation and run success')
            elif test.returncode == 2:
                if expectations[exCmpErr] != False:
                    raise Failure('Unexpected compilation success, but runtime error')
            elif test.returncode == 3:
                if expectations[exRunErr] != False:
                    raise Failure('Unexpected compilation error:\n' + errout)
            else:
                raise Failure('Unknown failure: return code ' +
                                str(test.returncode) + '\n' +
                                out + '\n' + errout)
            if out.rstrip('\n') != expectedOut.rstrip('\n'):
                raise Failure('Incorrect output\n' +
                              'Expected output:\n' +
                              expectedOut +
                              '\nActual output:\n' +
                              out)
            if (expectedErr.rstrip('\n') and
                    (errout.rstrip('\n') != expectedErr.rstrip('\n'))):
                raise Failure('Incorrect error output\n' +
                              'Expected error:\n' +
                              expectedErr +
                              '\nActual error:\n' +
                              errout)
        else:
            if test.returncode != 0:
                if test.returncode == 2:
                    raise Failure('Unexpected runtime error:\n' + errout)
                elif test.returncode == 3:
                    raise Failure('Unexpected compilation error:\n' + errout)
                else:
                    raise Failure('Unknown failure: return code ' +
                                  str(test.returncode) + '\n' +
                                  out + '\n' + errout)
            if out.rstrip('\n') != expectedOut.rstrip('\n'):
                raise Failure('Incorrect output\n' +
                              'Expected output:\n' +
                              expectedOut +
                              '\nActual output:\n' +
                              out)
    except Failure, f:
        if verbose:
            print "Failed"
        sys.stdout.flush()
        sys.stderr.write(40*'=' + '\n')
        sys.stderr.write("" + filename + ":0:0: TEST FAILED\n")
        sys.stderr.write(40*'-' + '\n')
        sys.stderr.write(f.message)
        sys.stderr.write('\n'+ 40*'=' + '\n')
        sys.stderr.flush()
        return False
    if verbose:
        print "OK"
    return True

if len(sys.argv) > 1 and sys.argv[1] == "--verbose":
    verbose = True
    testfiles = sys.argv[2:]
else:
    testfiles = sys.argv[1:]

testcount = len(testfiles)

if not testcount:
    sys.stderr.write("No tests specified!\n")
    sys.exit(1)

successcount = 0
for testfile in testfiles:
    if runTest(testfile):
        successcount += 1

testratio = str(successcount) + '/' + str(testcount)

if successcount == testcount:
    print "All tests passed (" + testratio + ")\nOK"
else:
    sys.stderr.write("tests passed: " + testratio + "\n");
    sys.stderr.write(str(testcount - successcount) + " tests failed\nFAILED\n");
    sys.exit(1)

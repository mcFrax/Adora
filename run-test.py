#!/usr/bin/python

import sys
import subprocess

devnull = open('/dev/null')

class Failure(Exception):
    pass

def runTest(filename):
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

    test = subprocess.Popen(['./interpreter', filename], stdin=subprocess.PIPE, stdout=subprocess.PIPE, stderr=subprocess.PIPE)
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
                raise Failure('Unknown failure: return code ' + test.returncode)
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
                    raise Failure('Unknown failure: return code ' + test.returncode)
            if out.rstrip('\n') != expectedOut.rstrip('\n'):
                raise Failure('Incorrect output\n' +
                              'Expected output:\n' +
                              expectedOut +
                              '\nActual output:\n' +
                              out)
    except Failure, f:
        print "Failed"
        print f.message
        return False
    print "OK"
    return True

assert len(sys.argv) == 2

if not runTest(sys.argv[1]):
    sys.exit(1)

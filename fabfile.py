import os
from fabric.api import run, env, local


def compile_and_test(testfiles=[]):
  """
  testfiles: filename of the files in tests/ to run.  Don't include the .coffee ext
  """
  if not isinstance(testfiles, list):
    testfiles = [testfiles]

  peg()
  coffee()
  for fname in testfiles:
    local(
      """coffee -c tests/%s.coffee;
      mv tests/%s.js ./libs/test-%s.js;
      node libs/test-%s.js;""" % (fname, fname, fname, fname) 
    )

def peg():
  local("""
    mkdir -p ./libs
    pegjs ./pegs/sql.pegjs
    cat ./src/sqlheader.headerjs ./pegs/sql.js  > ./libs/sqlfull.js
    rm ./pegs/sql.js
  """)

def coffee():
  local("""
    mkdir -p ./libs;
    coffee -c src/*.coffee;
    mv src/*.js libs/;
  """)

def clean(): 
  local("rm ./libs/*")

fnames = os.listdir('./tests')
fnames = [n for n in fnames if n.endswith('coffee')]
testfiles = [n[:-7] for n in fnames]
# testfiles = ["time", "igraph", "cycles", "sm", "dq", "jsfunc", "event"]
for fname in testfiles:
  exec( "def %s(): compile_and_test(\"%s\")" % (fname, fname) )


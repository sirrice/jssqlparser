ast = require "./ast.js"
parser = require "./sqlfull.js"
_ = require "underscore"

_.extend @, ast

parse = (str, DEBUG=no) ->

  prepareString = (str) ->
    q = str
    q = q.trim()
    q = q.replace /[\t\n\r]/g, " "
    q = q.replace /(\s)+/g, " "
    q = q.replace /\(\s+/g, "("
    q = q.replace /\s+\)/g, ")"
    q

 
  console.log str if DEBUG
  parser.parse prepareString str

_.extend parse, ast
parse.one = (args...) ->
  res = parse args...
  res.queries[0]

module.exports = parse

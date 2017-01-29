parser = require './jssqlparser.js'
ast = require "./ast.js"
_ = require "underscore"
fs = require "fs"
vows = require "vows"
assert = require "assert"


suite = vows.describe("Parser Tests")
  .addBatch(
    "Project":
      topic: () -> 
        parser "SELECT :$a|a: FROM b;"
      'swap nay' : (q) ->
        q.descendents("ParamVar")[0].val = new ast.ColExpr("nay")
        assert.equal q.toPrettySQL(), "SELECT nay FROM (b) AS b WHERE 1 = 1;"
  )
  .addBatch(
    "Where1":
      topic: -> parser "SELECT a, b FROM c WHERE :$p|true:"
      "swap WHERE": (q) ->
        pe = q.descendents("ParamExpr")[0]
        pe.setParams { "p": parser.one("t > $x AND t < 100")}
        pe.setParams { "x": parser.one("10") }
        assert.equal q.toPrettySQL(), "SELECT a AS a, b AS b FROM (c) AS c WHERE ((t > (10.00)) AND ((t < 100.00)));"

    "Where2":
      topic: -> parser "SELECT a, b FROM c WHERE :$p > $min AND $p < $max|true:"
      "swap WHERE": (q) ->
        q.descendents("ParamExpr")[0].setParams 
          p: parser.one("d")
          min: parser.one("0")
          max: parser.one("100")
        assert.equal q.toPrettySQL(), "SELECT a AS a, b AS b FROM (c) AS c WHERE ((d > (0.00)) AND ((d < (100.00))));"
  )
  .addBatch(
    "Group":
      topic: -> parser "SELECT 1.0 GROUP BY a, :$p|b:, :$q|1:, :$r|NULL:"
      "swap": (q) ->
        assert.equal q.toPrettySQL(), "SELECT 1.00 WHERE 1 = 1  GROUP BY a, b, 1.00;"

        q.descendents("ParamExpr")[0].setParams
          p: parser.one("c")
        assert.equal q.toPrettySQL(), "SELECT 1.00 WHERE 1 = 1  GROUP BY a, c, 1.00;"

        q.descendents("ParamExpr")[1].setParams
          q: parser.one("dd")
        assert.equal q.toPrettySQL(), "SELECT 1.00 WHERE 1 = 1  GROUP BY a, c, dd;"

        q.descendents("ParamExpr")[2].setParams
          r: parser.one("e")
        assert.equal q.toPrettySQL(), "SELECT 1.00 WHERE 1 = 1  GROUP BY a, c, dd, e;"
  )
  .addBatch(
    "parse Null expr":
      topic: -> parser.one "null"
      "is special expr": (q) ->
        assert.equal "SpecialExpr", q.nodeType()
  )
  .addBatch(
    "multiple params": 
      topic: -> parser ":$p1 >= $p2 AND $p3 < $p4|null:", no
      "swap multiple params": (q) ->
        q.descendents("ParamExpr")[0].setParams {
          p1: parser("a", no).queries[0]
          p2: parser("1", no).queries[0]
          p3: parser("a", no).queries[0]
          p4: parser("99", no).queries[0]
        }
        assert.equal q.toPrettySQL(), "((a >= (1.00)) AND ((a < (99.00))));" 
  )
  .addBatch(
    "Cloning": 
      topic: -> parser(":$p = 1:").queries[0]
      "is properly cloned": (q) ->
        q1 = q.clone()
        pv = q.descendents("ParamVar")[0]
        pv1 = q1.descendents("ParamVar")[0]
        assert.notEqual pv, pv1
        pv.val = new ast.ValExpr(999)
        assert.notEqual pv.toSQL(), pv1.toSQL()
  )

saveDot = (q) ->
  fs.writeFile("./graph.dot", q.toDot(), (err) ->
    console.log err if err?
  )


suite.run()

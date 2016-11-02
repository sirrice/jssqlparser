parser = require './sqlast.js'
ast = require "./ast.js"
_ = require "underscore"
fs = require "fs"

ss = [
  "
    SELECT x FROM a;
    SELECT x FROM b, c WHERE b.x = 1;
  "
]


for s in ss
  q = parser s
  fs.writeFile("./graph.dot", q.toDot(), (err) ->
    console.log err if err?
  )


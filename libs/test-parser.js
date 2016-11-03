// Generated by CoffeeScript 1.10.0
(function() {
  var _, ast, fs, i, len, parser, q, s, ss;

  parser = require('./jssqlparser.js');

  ast = require("./ast.js");

  _ = require("underscore");

  fs = require("fs");

  ss = ["SELECT x FROM a; SELECT x FROM b, c WHERE b.x = 1; SELECT :param: FROM b, c WHERE :p2: = 2;"];

  for (i = 0, len = ss.length; i < len; i++) {
    s = ss[i];
    q = parser(s);
    fs.writeFile("./graph.dot", q.toDot(), function(err) {
      if (err != null) {
        return console.log(err);
      }
    });
  }

}).call(this);

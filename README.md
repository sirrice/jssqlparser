# Setup and Running

This folder contains code related to the core interaction language parser, AST, and execution engine.

Files

* `pegs/sql.pegjs` is the grammar file for the language.
   Running `pegjs ./pegs/sql.pegjs` will generate a javascript parser file as `./pegs/sql.js`
* `src/sqlast.coffee` defines a function that wraps the parser generated from the peg grammar file.
* `src/ast.coffee` defines the AST nodes for the language.  It's a rough subset of SQL and interaction statements.

Getting started

* install stuff

        npm install .
        cp node_modules/pegjs/bin/pegjs .
        pip install fabric


* `fab` commands

        fab -l

* compile parser

        fab peg

* compile modules

        fab coffee

* run a test in `./tests/`

        fab parser


# TODOs

evaluate [chevrotrain](http://sap.github.io/chevrotain/playground/)

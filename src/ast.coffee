_ = require "underscore"
_.append = (arr, v) ->
  arr = _.clone(arr)
  arr.push v
  arr

DEBUG = no




class Node
  @id: 1
  constructor: (@type) ->
    @id = Node.id++

  isType:  -> @nodeType() in arguments
  isExpr: -> @nodeType() in [ "Expr", "SpecialExpr", "ColExpr", "ValExpr",
                              "BetweenExpr", "UnaryExpr", "FuncExpr", "TableExpr"]
  isTable: -> @nodeType() in ["Table", "QueryTable"]

  clone: -> throw Error "not implemented"

  nodeType: -> @constructor.name

  descendents: (types...) ->
    @allNodes (n) -> n.isType types...

  children: -> []

  # returns names of tables that Node depends on
  sources: -> @descendents "Table"

  variables: -> @descendents "ColExpr"

  # @param f given a node, returns true if it should be returned
  allNodes: (f) ->
    ret = []
    func = (node, path) ->
      ret.push node if f node

    @traverse func
    _.uniq ret

  traverse: (f, path=[]) -> 
    f @, path
    newpath = _.append path, @
    for child in @children()
      continue unless child?
      unless _.isFunction child.traverse
        console.log "ERR: child node doesn't support traverse().  Printing child, this and path"
        console.log child
        console.log @
        console.log path
      child.traverse f, newpath

  # what is run by a dbms
  toSQL: -> ""

  # readable formatting
  toPrettySQL: -> @toSQL()

  # in the DEVIL syntax
  toString: -> @toSQL()

  # subset of AST nodes can be translated into JS
  toJSString: ->
    throw new Error "toJSString() not implemented by #{@toSQL()}"

  toDot: ->
    output = []
    output.push "\ndigraph DepGraph#{@nprintcalls} {"
    output.push "  labelloc=\"t\";"
    output.push "  label=\"graph#{@nprintcalls}\";"

    f = (node, path) ->
      if path.length > 0
        parent = _.last(path)
        pname = parent.nodeType()
        name = node.nodeType()
        output.push "  #{pname}#{parent.id} -> #{name}#{node.id}"
    @traverse f

    output.push "}\n"
    s = output.join "\n"
    s




class Queries extends Node
  constructor: (@queries) ->
    super

  clone: ->
    new Queries(_.map @queries, (q) -> q.clone())

  children: -> @queries

  toSQL: ->
    sqls = _.map @queries, (q) -> q.toSQL()
    "#{sqls.join(";")};"

  toPrettySQL: ->
    sqls = _.map @queries, (q) -> q.toSQL()
    "#{sqls.join(";\n")};"


class SelectCore extends Node
  constructor: (@project=null, @from=null, @where=null, @groupby=null) ->
    @where = new Where unless @where?
    super

  clone: ->
    project = from = where = set = groupby = limit = orderby = null
    project = @project.clone() if @project?
    from = @from.clone() if @from?
    where = @where.clone() if @where?
    groupby = @groupby.clone() if @groupby?

    new SelectCore project, from, where, groupby, orderby, limit

  children: -> 
    _.compact [@project , @from , @where , @groupby , @orderby , @limit]

  toSQL: ->
    ret = [
      "SELECT #{@project.toSQL()}"
    ]
    if @from? and @from.tables? and @from.tables.length > 0
      ret.push "FROM #{@from.toSQL()}"
    if @where?
      ret.push "WHERE #{@where.toSQL()}"
    if @groupby?
      ret.push " GROUP BY #{@groupby.toSQL()}"
    ret.join " "

  schema: -> @project.schema()


class Query extends Node
  constructor: (@selectCores=[], @orderby=null, @limit=null) ->
    @selectCores = _.compact _.flatten [@selectCores]
    super

    # for insert queries (which we'll support never?)
    @set = null

  clone: ->
    cores = @selectCores.map (sc) -> sc.clone()
    orderby = @orderby.clone() if @orderby?
    limit = @limit.clone() if @limit?
    new Query cores, orderby, limit

  children: -> _.union @selectCores, _.compact([@orderby, @limit])

  toSQL: ->
    cores = @selectCores.map (sc) ->
      sc.toSQL()
    ret = [ cores.join " UNION " ]

    if @orderby?
      ret.push " ORDER BY #{@orderby.toSQL()}"
    if @limit?
      ret.push " LIMIT #{@limit.toSQL()}"
    ret.join " "

  # Doesn't deal with UNIONs of * clauses
  schema: ->
    schema = null
    for core in @selectCores
      if schema?
        isConsistent = _.chain(schema)
          .zip(core.schema())
          .all((pair) -> pair[0].type == pair[1].type)
          .value()
        unless isConsistent
          throw new Error "Inconsistent schemas: #{JSON.stringify schema} ::: #{JSON.stringify core.schema()}"
      else
        schema = core.schema()

    schema = [] unless schema?
    schema

class Project extends Node
  constructor: (@clauses) ->
    @clauses = _.compact _.flatten [@clauses]
    super

  canonicalize: ->
    _.each @clauses, (clause, idx) ->
      unless clause.alias?
        clause.alias = "col_#{idx}"

  addClause: (clause) ->
    @clauses.push clause

  # @param aliasOrIdx is either
  # 1) a number that indexs into clauses or
  # 2) name of the aliased clause
  getByAlias: (aliasOrIdx) ->
    if _.isNumber aliasOrIdx
      return @clauses[aliasOrIdx]
    else
      for c in @clauses
        if c.alias == aliasOrIdx
          return c


  clone: ->
    clauses = _.compact _.map @clauses, (c) ->
      c.clone() if c?
    new Project clauses

  children: -> @clauses

  toSQL: ->
    sqls = _.compact _.map @clauses, (clause) ->
      clause.toSQL() if clause?
    sqls.join ", "

  schema: (tableAliases) ->
    for clause, aliasIdx in @clauses
      if clause.expr.isType "SpecialExpr"
        {
          alias: null
          tableName: clause.expr.table.name
          type: "star"
        }
      else
        clause.alias = "col#{aliasIdx}" unless clause.alias?
        {
          alias: clause.alias
          type: clause.exprType()
        }


class ProjectClause extends Node
  constructor: (@expr, @alias=null) ->
    if @expr.isType "ColExpr"
      @alias = @expr.col unless @alias?
    super

  clone: ->
    proj = new ProjectClause(@expr.clone(), @alias)
    proj

  children: -> [@expr]

  toSQL: ->
    if @alias?
      "#{@expr.toSQL()} AS #{@alias}"
    else
      @expr.toSQL()

  exprType: ->
    type = null
    f = (node) ->
      return if type?
      if node.isType "Expr"
        if node.op in ["+", "-", "/", "*"]
          type = "numeric"
      else if node.isType "ValExpr"
        if _.isString node.v
          type = "text"
        else
          type = "numeric"
      else if node.isType "FuncExpr"
        if node.isSQLFunc
          type = "numeric"

    @expr.traverse f
    if @alias == "id" and type == null
      type = "numeric"
    unless type?
      console.error "WARN: col #{@alias} couldn't infer type for #{@toSQL()}.  "
      console.error "WARN: defaulting to int"
      type = "numeric"

    type



class From extends Node
  constructor: (@tables) ->
    @tables = _.compact _.flatten [@tables]
    super

  addTable: (table) ->
    @queries.push table

  # @param aliasOrIdx is either
  # 1) a number that indexs into table or
  # 2) name of the aliased clause
  getByAlias: (aliasOrIdx) ->
    if _.isNumber aliasOrIdx
      return @tables[aliasOrIdx]
    else
      for t in @tables
        if t.alias == aliasOrIdx
          return t

  clone: ->
    new From(_.map @tables, (t) -> t.clone())

  children: -> @tables

  toSQL: ->
    sqls = _.map @tables, (t) ->
      if t.alias?
        "(#{t.toSQL()}) AS #{t.alias}"
      else
        "#{t.toSQL()}"
    sqls = _.without sqls, _.isEmpty
    sqls.join ", "


class Table extends Node
  constructor: (@name, @alias=null) ->
    @isDefaultAlias = no
    unless @alias?
      @alias = @name unless @alias?
      @isDefaultAlias = yes
    super

  clone: ->
    new Table @name, @alias

  isExternalTable: -> no

  toString: (printAlias=yes) ->
    return @name 


  toSQL: (printAlias=yes) ->
    return @name 

class ExternalTable extends Node
  constructor: (@interactionName, @name) ->
    super

  clone: -> new ExternalTable @interactionName, @name

  isExternalTable: -> yes

  toString: -> @toSQL()

  toSQL: ->
    "#{@interactionName}.#{@name}"


class QueryTable extends Node
  constructor: (@query, @alias=null) ->
    unless @alias?
      throw new Error "subquery needs to have an alias!  #{@query}"
    super

  clone: ->
    new QueryTable @query.clone(), @alias

  children: -> [@query]

  # the FROM clause will add the "AS alias" clause
  toSQL: ->
    @query.toSQL()


class TableUDF extends Node
  constructor: (@fname, @exprs, @alias=null) ->
    unless @alias?
      throw new Error "UDF in FROM clause needs to have an alias! #{@fname}"
    unless @fname not in ["abs", "min", "max"]
      throw new Error "SQL func should not be in FROM clause"
    @type = "TableUDF"
    @exprs = _.compact _.flatten [@exprs]
    super

  clone: ->
    new TableUDF @fname, _.map(@exprs, (e)->e.clone()), @alias

  traverse: (f, path=[]) ->
    f @, path
    newpath = _.append path, @
    _.each @exprs, (e) -> e.traverse f, newpath

  toSQL: ->
    args = @exprs.map((e)->e.toSQL()).join ","
    "#{@fname}(#{args})"

  toJSString: ->
    f = @fname
    args = @exprs.map((e)->e.toJSString()).join ","
    "#{f}(#{args})"


class LetUDF extends Node
  constructor: (@fname, @args, @input, @render_or_compute, @source) ->
     @type = "LetUDF"
     @args = _.compact _.flatten [@args]
     super

  clone: ->
    new LetUDF @fname, _.map(@exprs, (e)->e.clone()), @alias

  traverse: (f, path=[]) ->
    f @, path
    newpath = _.append path, @
    for arg in @args
      arg.traverse f, newpath

  clone: ->
    args = @args.map (arg) -> arg.clone()
    new LetUDF @fname, args, @exists


  toSQL: -> ""

  schema: ->
    @args.map (arg) -> arg.schema()

class LetUDFArg extends Node
  constructor: (@name, @type) ->
    super
  clone: -> new LetUDFArg @name, @type
  toSQL: -> "#{@name} #{@type}"
  schema: ->
    alias: @name
    type: @type

class Where extends Node
  constructor: (@exprs, @conj=" AND ") ->
    @exprs = _.compact _.flatten [@exprs]
    super

  addExpr: (expr) ->
    @exprs.push expr


  # @param aliasOrIdx is either
  # 1) a number that indexs into table
  #
  # NOTE: NO NAMED ALIASES in where clauses
  getByAlias: (aliasOrIdx) ->
    if _.isNumber aliasOrIdx
      return @tables[aliasOrIdx]
    null

  clone: ->
    exprs = _.map @exprs, (e) -> e.clone()
    new Where exprs, @con

  children: -> @exprs

  toSQL: ->
    strs = _.compact _.map @exprs, (expr) -> expr.toSQL()

    if strs.length == 0
      "1 = 1"
    else
      strs.join " AND "

  toJSString: ->
    if @exprs.length == 0
      "true"
    else
      join = " && "
      join = " || " if /OR/.test @conj
      strs = _.chain(@exprs)
        .map((expr) -> "(#{expr.toJSString()})")
        .compact()
        .value()
      strs.join " && "




class Expr extends Node
  constructor: (@l, @op=null, @r=null) ->
    super

  clone: ->
    l = @l.clone()
    op = @op
    r = null
    r = @r.clone() if @r?
    new Expr l, op, r

  children: -> _.compact [@l, @r]

  toSQL: ->
    if @op?
      if @r? and @r.isType("ColExpr", "ValExpr", "FuncExpr")
        "(#{@l.toSQL()} #{@op} #{@r.toSQL()})"
      else
        "(#{@l.toSQL()} #{@op} (#{@r.toSQL()}))"
    else
      @l.toSQL()

  toJSString: ->
    if @op?
      op = @op
      op = "==" if op == "="
      op = "&&" if op == "AND"
      "(#{@l.toJSString()} #{op} (#{@r.toJSString()}))"
    else
      @l.toJSString()






# for strings such as * that should not be
# treated as literals
class SpecialExpr extends Expr
  constructor: (@v, @table=null) ->
    @tableName = null
    @tableName = @table.name if @table?
    super
  clone: -> new SpecialExpr @v, @table
  toSQL: ->
    prefix = ""
    prefix = "#{@tableName}." if @tableName?
    "#{prefix}#{@v}"
  toJSString: ->
    if @tableName?
      "#{@tableName}"
    else
      throw new Error("SpecialExpr doesn't support toJSString: #{@v}")

# For Event queries, checking if value is between two expressions
class BetweenExpr extends Expr
  constructor: (@v, @op, @minv, @maxv) ->
    super

  clone: -> 
    v = @v
    v = @v.clone() if @v.clone?
    minv = @minv
    minv = @minv.clone() if @minv.clone?
    maxv = @maxv
    maxv = @maxv.clone() if @maxv.clone?
    new BetweenExpr v, @op, minv, maxv

  children: -> _.compact [@v, @minv, @maxv]

  toSQL: ->
    "#{@v.toSQL()} #{@op} #{@minv.toSQL()} AND #{@maxv.toSQL()}"

  toJSString: ->
    ret = [
      "(#{@v.toJSString()} >= #{@minv.toJSString()})"
      "(#{@v.toJSString()} < #{@maxv.toJSString()})"
    ]
    ret = ret.join " && "
    if @op == "NOT BETWEEN"
      ret = "!(#{ret})"
    ret

class UnaryExpr extends Expr
  constructor: (@op, @expr) ->
    super
  clone: -> new UnaryExpr @op, @expr.clone()
  children: -> [@expr]
  toSQL: ->
    "#{@op} #{@expr.toSQL()}"
  toJSString: ->
    if @op == "NOT"
      "!(#{@expr.toJSString()})"
    else if @op == "NOT EXISTS"
      "!_.isEmpty(#{@expr.toJSString()})"
    else
      "#{@op}#{@expr.toJSString()}"

class FuncExpr extends Expr
  constructor: (@fname, @exprs) ->
    @exprs = _.compact _.flatten [@exprs]
    super

  children: -> @exprs

  clone: -> new FuncExpr @fname, _.map(@exprs, (e)->e.clone())

  isSQLFunc: -> @fname in ["abs", "max", "min"]

  toSQL: ->
    args = @exprs.map((e)->e.toSQL()).join ","
    "#{@fname}(#{args})"

  toJSString: ->
    f = switch @fname
      when "abs" then "Math.abs"
      when "max" then "Math.max"
      when "min" then "Math.min"
      else @fname
    args = @exprs.map((e)->e.toJSString()).join ","
    "#{f}(#{args})"



class ColExpr extends Expr
  constructor: (@col, @table=null) ->
    @tableName = null
    @tableName = @table.name if @table?
    super

  children: -> _.compact [@table]

  clone: -> new ColExpr @col, @table
  toSQL: ->
    prefix = ""
    prefix = "#{@tableName}." if @tableName?
    "#{prefix}#{@col}"

  toJSString: -> @toSQL()

class TableExpr extends Expr
  constructor: (@table) ->
    unless @table?
      throw new Error "TableExpr got null table"
    @tableName = null
    @tableName = @table.name if @table?
    super

  clone: -> new TableExpr @table.clone()
  toSQL: -> @table.toSQL()
  toJSString: -> @tableName

class ParamVar extends Node
  constructor: (@name, @val=null) ->

  children: -> _.compact [@val]
  clone: -> new ParamVar @name
  toSQL: -> 
    return @val.toSQL() if @val?
    "$#{@name}"

class ParamExpr extends Expr
  constructor: (@expr, @default=null, @params={}) ->
    super

  getVars: -> @expr.descendents "ParamVar"
  areParamsFixed: -> _.all(@getVars(), (v) -> v.val?)
  getParams: -> @getVars()
  getParamNames: -> _.pluck @getVars(), "name"
  setParams: (@params) ->
    for pv in @getVars()
      if pv.name of @params
        pv.val = @params[pv.name]

  children: -> _.compact [@default, @expr]

  clone: -> 
    args = _.map [@expr, @default], (v) -> v? and v.clone() or null
    args.push _.clone(@params)
    new ParamExpr args...

  toSQL: -> 
    return @expr.toSQL() if @areParamsFixed()
    if @default?
      if @default.isType("SpecialExpr") and @default.v is null
        return null
      return @default.toSQL() 
    return null

class ValExpr extends Expr
  constructor: (@v) ->
    super

  children: -> []
  clone: -> new ValExpr @v
  toSQL: ->
    return "'#{@v}'" if _.isString @v
    return @v.toFixed(2) if @v? and _.isNumber @v
    "#{@v}"
  toJSString: -> @toSQL()




##############################################
#
# Operators that don't really get any love
#
##############################################



class Group extends Node
  # @param gorupinglist list of Expr
  # @param having a single HAVING expression Expr object (not a list)
  #
  constructor: (@groupinglist, @having) ->
    super

  clone: ->
    having = null
    having = @having.clone() if @having?
    new Group(
      @groupinglist.map (g) -> g.clone(),
      having
    )

  children: -> _.union @groupinglist, _.compact([@having])

  toSQL: ->
    grouping = _.compact(@groupinglist.map (g) -> g.toSQL())
    if @having? and @having.children().length > 0
      "#{grouping.join ", "} HAVING #{@having.toSQL()}"
    else
      grouping.join ", "


class Having extends Node
  constructor: (@exprs=[]) ->
    @exprs = _.compact _.flatten [@exprs]
    super

  children: -> @exprs

  clone: ->
    exprs = _.map @exprs, (e) -> e.clone()
    new Having exprs

  toSQL: ->
    _.map(@exprs, (e) -> e.toSQL()).join ", "

class OrderBy extends Node
  constructor: (@exprs=[]) ->
    @exprs = _.compact _.flatten [@exprs]
    super

  children: -> @exprs

  clone: -> new OrderBy( @exprs.map (e) -> e.clone())

  toSQL: -> @exprs.map((e) -> e.toSQL()).join ", "

class OrderByClause extends Node
  constructor: (@expr, @asc=true) ->
    super


  clone: -> new OrderByClause @expr.clone(), @asc

  children: -> [@expr]

  toSQL: ->
    asc = if @asc then "ASC" else "DESC"
    "#{@expr.toSQL()} #{asc}"

class Limit extends Expr
  constructor: (@expr, @offset) ->
    super

  clone: ->
    offset = null
    offset = @offset.clone() if @offset?
    new Limit @limit.clone(), offset

  # old traverse() impl used to have the following line:
  #
  #     @expr.traverse f, path
  #
  # instead of 
  #
  #     @expr.traverse f, _.append(path, @)
  #
  # Not sure if that was a bug or not...
  children: -> _.compact [@expr, @offset]

  toSQL: ->
    if @offset?
      "#{@expr.toSQL()} OFFSET #{@offset.toSQL()}"
    else
      @expr.toSQL()


##############################################
#
# Function Query
#
##############################################

class FunctionQuery extends Node
  constructor: (@fname, @tableOrQuery) ->
    super
    unless @tableOrQuery?
      throw new Error "FunctionQuery cannot have empty argument"

  clone: ->
    new FunctionQuery @fname, @tableOrQuery.clone()

  children: -> [@tableOrQuery]

  toSQL: ->
    "#{@fname}(#{@tableOrQuery.toSQL()})"



#
# compute query's schema by recursively expanding all * clauses
# XXX: passes over queries to expand STAR project clauses
#
# @param queryName name of query to compute schema for
# @param nameToQueries dictionary mapping query names (e.g., in FROM clauses) to the Query AST objects
#
schema = (() ->
  get_schema = (queryName, nameToQueries, seen={}) ->
    return [] if queryName of seen
    seen[queryName] = yes

    query = nameToQueries[queryName]
    schema = query.schema()
    stars = _.filter schema, (s) -> s.type == "star"
    rest = _.reject schema, (s) -> s.type == "star"
    return rest unless stars.length

    # expand the schemas for SELECT * clauses
    starSchemas = for star in stars
      if star.table?
        get_schema star.table.name, nameToQueries, seen
      else
        sources = query.sources()
        unless sources.length == 1
          throw new Error("* project clause must be qualified with table name if >1 table source")
        get_schema sources[0].name, nameToQueries, seen

    tmp = null
    for starSchema in starSchemas
      tmp = starSchema unless tmp?
      isConsistent = _.chain(tmp)
        .zip(starSchema)
        .all((p) -> p[0].type == p[1].type)
        .value()
      unless isConsistent
        throw new Error "Inconsistent schemas: #{JSON.stringify tmp}
          ::: #{JSON.stringify starSchema}"

    rest.concat tmp
)()



module.exports =
  Queries           : Queries
  SelectCore        : SelectCore
  Query             : Query
  Project           : Project
  ProjectClause     : ProjectClause
  From              : From
  Table             : Table
  ExternalTable     : ExternalTable
  QueryTable        : QueryTable
  Where             : Where
  Expr              : Expr
  SpecialExpr       : SpecialExpr
  BetweenExpr       : BetweenExpr
  UnaryExpr         : UnaryExpr
  FuncExpr          : FuncExpr
  ColExpr           : ColExpr
  TableExpr         : TableExpr
  ParamVar          : ParamVar
  ParamExpr         : ParamExpr
  ValExpr           : ValExpr
  Group             : Group
  Having            : Having
  OrderBy           : OrderBy
  OrderByClause     : OrderByClause
  Limit             : Limit
  FunctionQuery     : FunctionQuery
  schema            : schema

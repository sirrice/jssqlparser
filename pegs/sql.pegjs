
start = stmt_list

stmt_list = 
  ( c: (whitespace general_stmt? whitespace (semicolon whitespace general_stmt? whitespace)* semicolon?))
  { 
    var qs = _.compact(_.without(_.flatten(c), ";", " ", "\t"));
    return new ast.Queries(qs);
  }


general_stmt = 
  ( g: ( select_stmt / func_stmt))
  { return g; }

func_stmt = 
  cf: ( function_name
    whitespace lparen whitespace
    ( select_stmt / table_name )
    whitespace rparen )
  { 
    return new ast.FunctionQuery(cf[0], cf[4]); 
  }


select_stmt = 
  ( 
    s: ( ss: ( select_core (compound_operator select_core)* )
       { 
         return _.chain(ss)
           .flatten()
           .filter(function(s, idx) { if (idx % 2 == 0) return s; })
           .value();
       })
    o: ( oo: ( ORDER BY ordering_term (whitespace comma ordering_term)* )?
        {
          if (_.isNull(oo)) return null;
          var exprs = _.chain(oo)
            .flatten()
            .without(",", " ", "\n", "\t")
            .reject(function(o) { return _.isString(o); })
            .value();
          return new ast.OrderBy(exprs);
        } )
    l:  ( ll: ( LIMIT expr ( OFFSET expr )? )? 
        {
          if (_.isNull(ll)) return null;
          var offset = null;
          if (!_.isNull(ll[2])) offset = ll[2][1]
          return new ast.Limit(ll[1], offset);
        } )
  )
  { 
    return new ast.Query("select", s,o,l);
  }


select_core =
  ( SELECT 
    d: ( ( DISTINCT / ALL )? )
    c: ( cc: ( select_result ( whitespace comma select_result )* )
         { 
           var clauses = _.without(_.flatten(cc), ",", " ", "\n", "\t");
           return new ast.Project(clauses);
         } )
    f: ( ff: ( FROM join_source )? 
         { return ff ? new ast.From(ff[1]) : null; } )
    w: ( e: ( WHERE where )? 
         { return e ? e[1] : null; } ) 
    g: ( gb: ( GROUP BY 
                ( ot: ( grouping_term ( comma grouping_term )* )
                    { 
                      return _.chain(ot)
                        .flatten()
                        .without(",", " ", ",", null)
                        .reject(_.isEmpty)
                    }
                )
                ( HAVING expr )? )? 
              { 
                // only support a single having expression for now
                if (_.isEmpty(gb) || _.isNull(gb)) return null;
                var groups = gb[2];
                var having = null;
                if (!_.isNull(gb[3])) having = gb[3][1];
                return new ast.Group(groups, new ast.Having(having));
              }) 
  )
  { return new ast.SelectCore(c, f, w, g); }

select_result =
  r: ( whitespace
       ( ( c: ( name dot star )
              { 
                return new ast.ProjectClause(
                  new ast.SpecialExpr("*", c[0])); } )
       / ( c: ( value ( AS whitespace name)? )
              { 
                var alias = null;
                if (!_.isNull(c[1])) alias = c[1][2];
                return new ast.ProjectClause(c[0], alias )
              } )
       / ( c: ( column_ref ( AS whitespace column_alias )? )
              {
                var alias = null;
                if (!_.isNull(c[1])) alias = c[1][2];
                return new ast.ProjectClause(c[1], alias);
              } )
       / ( star
           { 
            return new ast.ProjectClause(
              new ast.SpecialExpr("*"));
           } ) ) 
  )
  { return r[1]; }

join_source =
  s: ( whitespace single_source
       ( whitespace comma whitespace single_source )* )
  { 
    return _.without(_.flatten(s), " ", "\t", "\n", ",");
  }


single_source =
  ( ( x: ( table_name AS whitespace1 table_alias )
      {
      return new ast.Table(x[0].name, x[3]); } )
  / ( p: ( lparen select_stmt rparen
           ( a: ( AS whitespace table_alias )
             { return a[2] } )? )
      {
        return new ast.QueryTable(p[1], p[3]); 
      } )
  / ( u: ( function_name 
          whitespace lparen whitespace ( select_stmt / table_name / select_result )?
          whitespace rparen  
          ( a: ( AS whitespace udftable_alias )
             { return a[2] } )? ) )
    {
      return new ast.TableUDF(u[0], u[4], u[7]);
    }
   / ( x: table_name
      {
      return x; 
      } )
  )


letudf_stmt = 
  (
    'LET' whitespace1
    input:  ('TABLEUDF' / 'ROWUDF')?
    whitespace1
    name: name
    render_or_compute: ('RENDER' / 'COMPUTE')?    
    whitespace1 'RETURN' whitespace lparen
    args: letudf_arg_list?
    rparen
    whitespace1 'SOURCE' whitespace
    source: js_string_literal
   )
  {
    input = _.isNull(input) ? "TABLEUDF" : input;
    render_or_compute = _.isNull(render_or_compute) ? "RENDER" : render_or_compute;
    return new ast.LetUDF(name, args, input, render_or_compute, source.value);
  }

letudf_arg_list = 
  args: (letudf_arg (whitespace comma letudf_arg)*)
  {
    args = _.without(_.flatten(args), ',', ' ', '\t', '\n');
    return args;
  }

letudf_arg = 
  arg: (whitespace name whitespace1 type)
  { 
    arg = _.without(_.flatten(arg), ' ', '\t', '\n');
    return new ast.LetUDFArg(arg[0], arg[1]);
  }


where = 
  c: ( whitespace1 expr (whitespace 'AND' expr)* )
  {
    c = _.flatten(c);
    c = _.without(c, ' ', '\t', 'AND');
    return new ast.Where(c);
  }



value =
  v: ( whitespace
       ( call_function
       / ( x: literal_value
           { return new ast.ValExpr(x); } )
       / ( t: ( table_name dot column_name )
           { return new ast.ColExpr(t[2], t[0]); } )
       / ( c: column_name
           { return new ast.ColExpr(c); } )
       / ( u: ( unary_operator expr )
           { return new ast.UnaryExpr(u[0], u[1]); } )
       / ( p: ( lparen expr whitespace rparen )
           { return p[1]; } )
//       / ( CAST lparen expr AS type_name rparen )
       / ( e: ( ( NOT ? EXISTS )? lparen select_stmt rparen ) 
           { 
             if (_.isNull(e[0])) return e[2];
             if (_.isNull(e[0][0]))
               return new ast.UnaryExpr("NOT EXISTS", e[2]);
             return new ast.UnaryExpr("EXISTS", e[2]);
           } ) ) )
  { return v[1] }

expr =
  e: ( whitespace
       ( ( i: ( value NOT ? IN 
              ( j: ( ( s: ( lparen select_stmt rparen ) 
                       { return s[1]; } )
                   / ( t: ( table_name )
                       { return new ast.TableExpr(t); } ) ) 
                { return _.chain(j)
                          .flatten()
                          .without(",", " ", "\t", null, "(", ")")
                          .value()
                } ))
              { 
                var op = "IN";
                if (!_.isNull(i[1])) op = "NOT IN";
                return new ast.Expr(i[0], op, i[3]);
              } )
       / ( a: ( value binary_op_wout_and expr ) 
              { return new ast.Expr(a[0], a[1], a[2]); } )
       / ( b: ( e_value ( ISNULL / NOTNULL / ( NOT NULL ) ) )
              { return new ast.Expr(b[0], b[1]); } )
       / ( c: ( e_value IS NOT ? e_expr )
              { return new ast.Expr(c[0], "IS NOT", c[c.length-1]) } )
       / ( f: ( e_value NOT ? BETWEEN e_expr AND e_expr )
              { 
                if (f.length == 5) {
                  return new ast.BetweenExpr(f[0], "NOT BETWEEN", f[3], f[5]);
                } return new ast.BetweenExpr(f[0], "BETWEEN", f[3], f[5]);
              } )
       / value ) )
  { return e[1]; }






e_where = 
  c: ( whitespace e_expr (whitespace 'AND' e_expr)* )
  {
    c = _.flatten(c);
    c = _.without(c, ' ', '\t', 'AND');
    return new ast.Where(c);
  }

e_value =
  v: ( whitespace
       ( e_call_function
       / ( x: literal_value
           { return new ast.ValExpr(x); } )
       / ( t: ( table_name dot column_name )
           { return new ast.ColExpr(t[2], t[0]); } )
       / ( v: (column_name)
           { return new ast.ColExpr(v, null); })
//       / ( unary_operator e_expr )
       / ( y: ( lparen e_where whitespace rparen )
           { return y[1]; } ) ) )
  { return v[1] }

e_expr =
  e: ( whitespace
       ( ( a: ( e_value binary_op_wout_and e_expr ) 
              { return new ast.Expr(a[0], a[1], a[2]); } )
       / ( b: ( e_value ( ISNULL / NOTNULL / ( NOT NULL ) ) )
              { return new ast.Expr(b[0], b[1]); } )
       / ( c: ( e_value IS NOT ? e_expr )
              { return new ast.Expr(c[0], "IS NOT", c[c.length-1]) } )
       / ( d: ( "EXISTS" whitespace1 name IN whitespace1 e_table_ref whitespace1 e_where  )
              { return new ast.QuantExpr("exists", d[2], d[5], d[7]); } )
       / ( e: ( "FORALL" whitespace1 name IN whitespace1 e_table_ref whitespace1 e_where )
              { return new ast.QuantExpr("all", e[2], e[5], e[7]); } )
       / ( f: ( e_value NOT ? BETWEEN e_expr AND e_expr )
              { 
                if (f.length == 5) {
                  return new ast.BetweenExpr(f[0], "NOT BETWEEN", f[3], f[5]);
                } return new ast.BetweenExpr(f[0], "BETWEEN", f[3], f[5]);
              } )
       / ( g: ( e_value )
              { return g; } ) ) )
  { return e[1]; }


e_call_function =
  cf: ( function_name
    whitespace lparen
               ( a: ( DISTINCT ? ( e_expr (whitespace comma e_expr)* ) )
                 { 
                  var distinct = !_.isNull(a[0]);
                  var args = _.chain(a[1])
                    .flatten()
                    .without(",", " ", "\t", "\n")
                    .compact()
                    .value();
                  return {
                    distinct: distinct,
                    args: args
                  }
                 }
               / ( whitespace star) 
                 { 
                   return { 
                    distinct: false,
                    args: [new ast.SpecialExpr("*")]
                   }; 
                  }
               )?
    whitespace rparen )
  { 
    var args = cf[3];
    return new ast.FuncExpr(cf[0], args.args);
  }

e_column_ref =
  r: ( table_name dot column_name )
  { return new ast.ColExpr(r[2], r[0]); }

e_table_ref = 
  r: ( table_name )
  { return new ast.TableExpr(r); }



type_name =
  ( name )+
  ( ( lparen signed_number rparen )
  / ( lparen signed_number comma signed_number rparen ) )?


signed_number =
  ( ( plus / minus )? numeric_literal )

literal_value =
  ( numeric_literal / string_literal / blob_literal
  / NULL / CURRENT_TIME / CURRENT_DATE / CURRENT_TIMESTAMP )

numeric_literal =
  digits:( (plus/minus)?
            ( ( ( digit )+ ( decimal_point ( digit )+ )? )
            / ( decimal_point ( digit )+ ) )
            ( E ( plus / minus )? ( digit )+ )? )
  { 
    var x = flatstr(digits);
    if (x.indexOf('.') >= 0) {
      return parseFloat(x);
    }
    return parseInt(x);
  }

call_function =
  cf: ( function_name
        whitespace lparen
          ( 
            ( whitespace
              t: (table_name dot)?
              star ) 
            { if (!_.isNull(t) && !_.isNull(t[0])) t = t[0];
              return new ast.SpecialExpr("*", t); }
            / a: ( DISTINCT ? ( expr (whitespace comma expr)* ) )
              { 
                var distinct = !_.isNull(a[0]);
                var args = _.chain(a[1])
                  .flatten()
                  .without(",", " ", "\t", null)
                  .value();
                // ignores distinct
                return args;
              }
          )?
        whitespace rparen )
  { 
    return new ast.FuncExpr(cf[0], cf[3]);
  }

column_ref =
  r: ( (table_name dot)? column_name )
  {
    var t = null;
    if (!_.isNull(r[0]) && !_.isNull(r[0][0])) t = r[0][0];
    return new ast.ColExpr(r[1], t);
  }


grouping_term = 
  gt: ( whitespace expr )
  { return gt[1]; }  


ordering_term =
  ot: ( whitespace
    ( expr ( ASC / DESC )? ) )
  { 
    var asc = true; 
    if (ot[1][1] == "DESC") asc = false;
    return new ast.OrderByClause(ot[1][0], asc);
  }

compound_operator =
  o: ( ( UNION ALL ? )
     / INTERSECT
     / EXCEPT )
  { return { op: flatstr(o) } }

unary_operator =
  x: ( whitespace
       ( '-' / '+' / '~' / 'NOT') )
  { return x[1] }

binary_op_wout_and = 
  x: ( whitespace
       ('||'
        / '*' / '/' / '%'
        / '+' / '-'
        / '<<' / '>>' / '&' / '|'
        / '<=' / '>='
        / '<' / '>'
        / '=' / '==' / '!=' / '<>'
        / 'IS' / 'IS NOT' / 'IN' / 'NOT IN' / 'LIKE' / 'GLOB' / 'MATCH' / 'REGEXP'
        / 'OR') )
  { return x[1] }


binary_operator =
  x: ( whitespace
       ('||'
        / '*' / '/' / '%'
        / '+' / '-'
        / '<<' / '>>' / '&' / '|'
        / '<=' / '>='
        / '<' / '>'
        / '=' / '==' / '!=' / '<>'
        / 'IS' / 'IS NOT' / 'IN' / 'NOT IN' / 'LIKE' / 'GLOB' / 'MATCH' / 'REGEXP'
        / 'AND'
        / 'OR') )
  { return x[1] }


name =
  str:[A-Za-z0-9_]+
  { return str.join('') }

type = 
    'int'
  / 'numeric'
  / 'float'
  / 'text'
  / 'varchar'
  / 'char'

  

table_name = 
  (
    n: name 
  )
  { return new ast.Table(n, null); }


query_frag = name
function_name = name
column_name = name
table_alias = name
column_alias = name
udftable_alias = name

digit = [0-9]
decimal_point = dot
equal = '='
dot = '.'
comma = ','
semicolon = ';'
minusminus = '--'
minus = '-'
plus = '+'
lparen = '('
rparen = ')'
star = '*'
newline = '\n'
anything_except_newline = [^\n]*
comment_beg = '/*'
comment_end = '*/'
anything_except_comment_end = .* & '*/'
string_literal = 
  s: ('\'' (escape_char / [^"'])* '\'')
  { return s[1].join(''); }
blob_literal = string_literal
escape_char = '\\' .
nil = ''
CURRENT_TIME = 'now'
CURRENT_DATE = 'now'
CURRENT_TIMESTAMP = 'now'
whitespace = [ \t\n\r]*
whitespace1 = [ \t\n\r]+


ADD = whitespace1 "ADD"
ALL = whitespace1 "ALL"
ALTER = whitespace1 "ALTER"
AND = whitespace1 "AND"
AS = whitespace1 "AS"
ASC = whitespace1 "ASC"
BETWEEN = whitespace1 "BETWEEN"
BY = whitespace1 "BY"
CAST = whitespace1 "CAST"
COLUMN = whitespace1 "COLUMN"
DESC = whitespace1 "DESC"
DISTINCT = whitespace1 "DISTINCT"
E = "E"
ESCAPE = whitespace1 "ESCAPE"
EXCEPT = whitespace1 "EXCEPT"
EXISTS = whitespace1 "EXISTS"
EXPLAIN = whitespace "EXPLAIN"
EVENT = whitespace "EVENT"
FORALL = whitespace1 "FORALL"
FROM = whitespace1 "FROM"
GLOB = whitespace1 "GLOB"
GROUP = whitespace1 "GROUP"
HAVING = whitespace1 "HAVING"
IN = whitespace1 "IN"
INNER = whitespace1 "INNER"
INSERT = whitespace "INSERT"
INTERSECT = whitespace1 "INTERSECT"
INTO = whitespace1 "INTO"
IS = whitespace1 "IS"
ISNULL = whitespace1 "ISNULL"
JOIN = whitespace1 "JOIN"
KEY = whitespace1 "KEY"
LEFT = whitespace1 "LEFT"
LIKE = whitespace1 "LIKE"
LIMIT = whitespace1 "LIMIT"
MATCH = whitespace1 "MATCH"
NO = whitespace1 "NO"
NOT = whitespace1 "NOT"
NOTNULL = whitespace1 "NOTNULL"
NULL = whitespace1 "NULL"
OF = whitespace1 "OF"
OFFSET = whitespace1 "OFFSET"
ON = whitespace1 "ON"
OR = whitespace1 "OR"
ORDER = whitespace1 "ORDER"
OUTER = whitespace1 "OUTER"
PRIMARY = whitespace1 "PRIMARY"
QUERY = whitespace1 "QUERY"
RAISE = whitespace1 "RAISE"
REFERENCES = whitespace1 "REFERENCES"
REGEXP = whitespace1 "REGEXP"
RENAME = whitespace1 "RENAME"
REPLACE = whitespace "REPLACE"
RETURN = whitespace1 "RETURN"
ROW = whitespace1 "ROW"
SAVEPOINT = whitespace1 "SAVEPOINT"
SELECT = whitespace "SELECT"
SET = whitespace1 "SET"
TABLE = whitespace1 "TABLE"
TEMP = whitespace1 "TEMP"
TEMPORARY = whitespace1 "TEMPORARY"
THEN = whitespace1 "THEN"
TO = whitespace1 "TO"
UNION = whitespace1 "UNION"
USING = whitespace1 "USING"
vALUES = whitespace1 "VALUES"
VIRTUAL = whitespace1 "VIRTUAL"
WITH = whitespace1 "WITH"
WHERE = whitespace1 "WHERE"












/***************************************/

// copy paste from pegjs github StringLiteral parser
// functionality: 
//   parse source javacript code (i.e., UDF) as string literal

/***************************************/

js_string_literal "string"
  = '"' chars:js_double_string_character* '"' {
      return { type: "Literal", value: chars.join("") };
    }
  / "'" chars:js_single_string_character* "'" {
      return { type: "Literal", value: chars.join("") };
    }

js_double_string_character
  = !('"' / "\\" / js_line_terminator) js_source_character { return text(); }
  / "\\" sequence:js_escape_sequence { return sequence; }
  / js_line_continuation

js_single_string_character
  = !("'" / "\\" / js_line_terminator) js_source_character { return text(); }
  / "\\" sequence:js_escape_sequence { return sequence; }
  / js_line_continuation

js_line_continuation
  = "\\" js_line_terminator_sequence { return ""; }


js_escape_sequence
  = js_character_escape_sequence
  / "0" !js_decimal_digit { return "\0"; }
  / js_hex_escape_sequence
  / js_unicode_escape_sequence

js_line_terminator
  = [\n\r\u2028\u2029]

js_source_character
  = .

js_character_escape_sequence
  = js_single_escape_character
  / js_non_escape_character

js_single_escape_character
  = "'"
  / '"'
  / "\\"
  / "b"  { return "\b";   }
  / "f"  { return "\f";   }
  / "n"  { return "\n";   }
  / "r"  { return "\r";   }
  / "t"  { return "\t";   }
  / "v"  { return "\x0B"; }   // IE does not recognize "\v".

js_line_terminator_sequence "end of line"
  = "\n"
  / "\r\n"
  / "\r"
  / "\u2028"
  / "\u2029"

js_non_escape_character
  = !(js_escape_character / js_line_terminator) js_source_character { return text(); }

js_escape_character
  = js_single_escape_character
  / js_decimal_digit
  / "x"
  / "u"


js_decimal_digit
  = [0-9]


js_hex_escape_sequence
  = "x" digits:$(js_hex_digit js_hex_digit) {
      return String.fromCharCode(parseInt(digits, 16));
    }

js_hex_digit
  = [0-9a-f]i


js_unicode_escape_sequence
  = "u" digits:$(js_hex_digit js_hex_digit js_hex_digit js_hex_digit) {
      return String.fromCharCode(parseInt(digits, 16));
    }

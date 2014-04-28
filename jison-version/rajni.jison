/* description: Parses end executes mathematical expressions. */

/* lexical grammar */
%lex
digit                       [0-9]
id                          [a-zA-Z][a-zA-Z0-9]*

%%
"//".*                      /* ignore comment */
"OruvanOruvanMudharali"     return 'MAIN';
"class"                     return 'CLASS';
"extends"                   return 'EXTENDS';
"nat"                       return 'NATTYPE';
"yen_vazhi"                 return 'IF';
"thani_vazhi"               return 'ELSE';
"for"                       return 'FOR';
"function"                  return 'FUNCTION';
":"                         return 'PARAMSEP';
"andavan_solran_arunachalam_mudikran"                  return 'PRINTNAT';
"readNat"                   return 'READNAT';
"this"                      return 'THIS';
"new"                       return 'NEW';
"style"                     return 'VAR';
"null"                      return 'NUL';
{digit}+                    return 'NATLITERAL';
{id}                        return 'ID';
"=="                        return 'EQUALITY';
"="                         return 'ASSIGN';
"+"                         return 'PLUS';
"-"                         return 'MINUS';
"*"                         return 'TIMES';
">"                         return 'GREATER';
"<"                         return 'LESSER';
"/"                         return 'DIVIDES';
"||"                        return 'OR';
"!"                         return 'NOT';
"."                         return 'DOT';
"{"                         return 'LBRACE';
"}"                         return 'RBRACE';
"("                         return 'LPAREN';
")"                         return 'RPAREN';
"cha"                       return 'SAMPLE';
";"                         return 'SEMICOLON';
"~"                         return 'VALASSIGN';
","                         return 'COMMA';
"nil"                       return  'NIL';
"#"                         return 'LOOPLIMIT';
\s+                         /* skip whitespace */
"."                         throw 'Illegal character';
<<EOF>>                     return 'ENDOFFILE';


/lex

%right ASSIGN
%left OR
%nonassoc EQUALITY GREATER
%nonassoc LESSER
%nonassoc VALASSIGN
%nonassoc LOOPLIMIT
%left PLUS MINUS
%left TIMES
%right NOT
%left DOT
%left DIVIDES


%start pgm
%%

pgm
    : MAIN LBRACE vdl el RBRACE ENDOFFILE
    ;

cdl
    : c cdl
    |
    ;

c
    : CLASS id EXTENDS id LBRACE vdl mdl RBRACE
    ;

vdl
    : VAR t SEMICOLON vdl
    |
    ;

mdl
    : t LPAREN t  RPAREN LBRACE vdl el RBRACE mdl
    |
    ;

t
    : NATTYPE
    | id
    ;

id
    : ID
    ;

el
    : e SEMICOLON el
    | e SEMICOLON
    ;
e
    : NATLITERAL
    | NUL
    | id
    | NEW id
    | THIS
    | IF LPAREN e RPAREN LBRACE el RBRACE ELSE LBRACE el RBRACE
    {  $$ = ifblock($3,$6,$10) }
    | FOR LPAREN e SEMICOLON e SEMICOLON e RPAREN LBRACE el RBRACE
    { $$ = forloop($3,$5,$7,$10)}
    | READNAT LPAREN RPAREN
    | PRINTNAT LPAREN e RPAREN
    { $$ = printF($3)}
    | e PLUS e
    {  $$ = (add($1,$3))}
    | e MINUS e
    {  $$ = (subtract($1,$3))}
    | e TIMES e
    {  $$ = (multiply($1,$3))}
    | e EQUALITY e
    {  $$ = console.log(parseInt($1) == parseInt($3))}
    | e GREATER e
    {  $$ = parseInt($1) > parseInt($3) }
    | NOT e
    | e OR e
    | e DOT id
    | id ASSIGN e
    | e DOT id ASSIGN e
    | id LPAREN e RPAREN
    | e DOT id LPAREN e RPAREN
    | LPAREN e RPAREN
    | e LESSER e
    {  $$ = parseInt($1) < parseInt($3) }
    | e DIVIDES e
    {  $$ = divide($1,$3) }
    | e VALASSIGN e
    {
      $$ = assign($1,parseInt($3))
    }
    | e LOOPLIMIT e
    { $$ = setLimitLoop(parseInt($3))}
;

%%

var global;
var looplimit;
var variableTable = [];

var mainFunction = {};

var printF = function(a)
{
  variableTable.forEach(function(obj) {
    if(obj.id === a)
    {
      return console.log(obj.value);
    }
  });
}

var assign = function(a,b)
{
  variableTable.push({id:a,value:b});
  global = b;
}

var setLimitLoop = function(b)
{
    looplimit = b;
    return looplimit;
}

var multiply = function (a,b) {
  return parseInt(a) * parseInt(b);
}

var divide = function (a,b) {
  return parseInt(a) / parseInt(b);
}

var subtract = function (a,b) {

    return parseInt(a) - parseInt(b);
}

var add = function (a,b) {
	return parseInt(a) + parseInt(b);
}

var ifblock = function(a,b,c) {
  if(a)
  {
    console.log(b);
    return b;
  }else
  {
    console.log(c);
    return c;
  }
}

var forloop = function(a,b,c,d)
{
  for(global;global<=looplimit;global++)
  {
    console.log(global);
    return console.log("SuperStar");
  }
}

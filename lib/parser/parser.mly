%{
  open Ast
%}

%token PARENS_OPEN
%token PARENS_CLOSE
%token <int> NUMBER
%token <bool> BOOL
%token <string> HEX
%token <Ptime.t> DATE
%token <string> STRING
%token <string> NAME
%token <string> VARIABLE
%token CHECK
%token IF
%token ALL
%token BRACKET_OPEN
%token BRACKET_CLOSE
%token SEMICOLON
%token COMMA
%token EQUAL
%token GREATER_THAN
%token GREATER_EQUAL_THAN
%token LOWER_THAN
%token LOWER_EQUAL_THAN
%token MULT
%token DIV
%token PLUS
%token MINUS
%token BW_OR
%token BW_AND
%token BW_XOR
%token BOOL_OR
%token BOOL_AND
%token AND
%token OR
%token ALLOW
%token DENY
%token LEFT_ARROW
%token EXCLAMATION_MARK
%token EOI
%left MULT DIV
%left PLUS MINUS
%left BW_OR BW_AND BW_XOR
%nonassoc EQUAL GREATER_THAN GREATER_EQUAL_THAN LOWER_THAN LOWER_EQUAL_THAN
%left BOOL_OR BOOL_AND
%start <Ast.Block.t> block
%start <Ast.Authorizer.t> authorizer
%%

block:
  | EOI { [] }
  | block_element block { $1 :: $2 }

authorizer:
  | EOI { [] }
  | authorizer_element authorizer { $1 :: $2 }

block_element:
  | rule SEMICOLON { Block.Rule $1 }
  | fact SEMICOLON { Block.Fact $1 }
  | check SEMICOLON { Block.Check $1 }

authorizer_element:
  | policy SEMICOLON { Authorizer.Policy $1 }
  | check SEMICOLON { Authorizer.Check $1 }

policy:
  | policy_action IF separated_nonempty_list(OR, rule_body) { Policy.{ action=$1; queries=$3 } }

policy_action:
  | ALLOW { Policy.Allow }
  | DENY { Policy.Deny }

check:
  | CHECK IF separated_nonempty_list(OR, rule_body) { Check.{ kind=One; queries=$3 } }
  | CHECK ALL separated_nonempty_list(OR, rule_body) { Check.{ kind=All; queries=$3 } }

rule_body:
  | separated_nonempty_list(COMMA, rule_body_element) { Rule.Body.{ elements=$1 } }

rule_body_element:
  | predicate { Rule.Body.Predicate $1 }
  | expression { Rule.Body.Expression $1 }

predicate:
  NAME delimited(PARENS_OPEN, separated_nonempty_list(COMMA, term), PARENS_CLOSE) { Predicate.{ name=$1; terms=$2 } }

fact_terms:
  | fact_term { [$1] }
  | fact_term COMMA fact_terms { $1 :: $3 }

fact:
  NAME delimited(PARENS_OPEN, fact_terms, PARENS_CLOSE) { Fact.{ name=$1; terms=$2 } }

term:
  | fact_term { Term.Fact $1 }
  | VARIABLE { Term.Variable $1 }

expression:
  | expression_element { Expression.Leaf $1 }
  | expression_element operator expression_element { Expression.Node ($1, $2, $3) }

operator:
  | EQUAL { Operator.Equal }
  | GREATER_THAN { Operator.GreaterThan }
  | GREATER_EQUAL_THAN { Operator.GreaterEqualThan }
  | LOWER_THAN { Operator.LowerThan }
  | LOWER_EQUAL_THAN { Operator.LowerEqualThan }
  | MULT { Operator.Mult }
  | DIV { Operator.Div }
  | PLUS { Operator.Plus }
  | MINUS { Operator.Minus }
  | BW_OR { Operator.BwOr }
  | BW_AND { Operator.BwAnd }
  | BW_XOR { Operator.BwXor }
  | BOOL_OR { Operator.Or }
  | BOOL_AND { Operator.And }

expression_element:
  | expression_unary { Expression.Unary $1 }
  | expression_term { Expression.ElementTerm { term=$1; method_call=None } }

expression_unary:
  EXCLAMATION_MARK expression { $2 }

expression_term:
  | term { Expression.Term $1 }
  | PARENS_OPEN expression PARENS_CLOSE { Expression.Expression $2 }

rule:
  predicate LEFT_ARROW rule_body { Rule.{ head=$1; body=$3 } }

fact_term:
  | STRING { Fact.Term.String $1 }
  | NUMBER { Fact.Term.Number $1 }
  | BOOL { Fact.Term.Bool $1 }
  | HEX { Fact.Term.Hex $1 }
  | DATE { Fact.Term.DateTime $1 }
  | set { Fact.Term.Set $1 }

set:
  BRACKET_OPEN separated_list(COMMA, set_term) BRACKET_CLOSE { $2 }

set_term:
  | STRING { Set.Term.String $1 }
  | NUMBER { Set.Term.Number $1 }
  | BOOL { Set.Term.Bool $1 }
  | HEX { Set.Term.Hex $1 }
  | DATE { Set.Term.DateTime $1 }

type algorithm = Ed25519
type public_key = { algorithm : algorithm; key : bytes }

type term_set = { set : term_v2 list }

and term_v2 =
  | Variable of int
  | Integer of int
  | String of string
  | Date of int
  | Bytes of bytes
  | Bool of bool
  | Set of term_set

type op_unary = Negate | Parens | Length

type op_binary =
  | LessThan
  | GreaterThan
  | LessOrEqual
  | GreaterOrEqual
  | Equal
  | Contains
  | Prefix
  | Suffix
  | Regex
  | Add
  | Sub
  | Mul
  | Div
  | And
  | Or
  | Intersection
  | Union
  | BitwiseAnd
  | BitwiseOr
  | BitwiseXor
  | NotEqual

type op = Value of term_v2 | Unary of op_unary | Binary of op_binary
type expression_v2 = { ops : op list }
type predicate_v2 = { name : int; terms : term_v2 list }
type fact_v2 = { predicate : predicate_v2 }
type scope_type = Authority | Previous
type scope = ScopeType of scope_type | PublicKey of int

type rule_v2 = {
  head : predicate_v2;
  body : predicate_v2 list;
  expressions : expression_v2 list;
  scope : scope list;
}

type check_v2_kind = One | All
type check_v2 = { kind : check_v2_kind; queries : rule_v2 list }

type block = {
  symbols : string list;
  context : string option;
  version : int option;
  facts_v2 : fact_v2 list;
  rules_v2 : rule_v2 list;
  checks_v2 : check_v2 list;
  scope : scope;
  public_keys : public_key list;
}

type external_signature = { signature : bytes; public_key : public_key }
type proof = NextSecret of bytes | FinalSignature of bytes

type signed_block = {
  bytes : bytes;
  next_Key : public_key;
  signature : bytes;
  external_signature : external_signature option;
}

type biscuit = {
  root_key_id : int option;
  authority : signed_block;
  blocks : signed_block list;
  proof : proof;
}

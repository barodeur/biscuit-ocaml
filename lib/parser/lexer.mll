{
open Core
open Parser
exception LexicalError of string * Lexing.position * Lexing.position
exception SyntaxError of string
}

let digit = ['0'-'9']
let letter = ['a'-'z']
let number = '-'? digit+
let sp = [' ' '\t' '\n']+
let date = digit+ '-' digit digit '-' digit digit 'T' digit digit ':' digit digit ':' digit digit ('Z' | (('+' | '-') digit digit ':' digit digit))
let bytes = (letter | digit)+
let name = letter (letter | '_')*
let hex = 'h' 'e' 'x' ':' bytes+
let variable = '$' (letter | digit | '_' | ':')+

rule token =
  parse
  | sp { token lexbuf }
  | ',' { COMMA }
  | ';' { SEMICOLON }
  | '(' { PARENS_OPEN }
  | ')' { PARENS_CLOSE }
  | '[' { BRACKET_OPEN }
  | ']' { BRACKET_CLOSE }
  | '"' { read_string (Buffer.create 17) lexbuf }
  | '!' { EXCLAMATION_MARK }
  | '=' { EQUAL }
  | '<' { LOWER_THAN }
  | "<=" { LOWER_EQUAL_THAN }
  | '>' { GREATER_THAN }
  | "=>" { GREATER_EQUAL_THAN }
  | "allow" { ALLOW }
  | "deny" { DENY }
  | "check" { CHECK }
  | "if" { IF }
  | "all" { ALL }
  | "true" { BOOL true }
  | "false" { BOOL false }
  | "or" { OR } 
  | "<-" { LEFT_ARROW }
  | number { NUMBER (int_of_string (Lexing.lexeme lexbuf)) }
  | hex { HEX (Lexing.lexeme lexbuf |> String.chop_prefix_exn ~prefix:"hex:") }
  | name { NAME (Lexing.lexeme lexbuf) }
  | variable { VARIABLE (String.slice (Lexing.lexeme lexbuf) 1 0) }
  | date {
    let str = Lexing.lexeme lexbuf in
    DATE (Ptime.of_rfc3339 str |> Result.ok |> Option.value_exn |> Tuple3.get1)
  }
  | eof { EOI }
  | _ {
    let string = Lexing.lexeme lexbuf
    and pos_start = Lexing.lexeme_start_p lexbuf
    and pos_end = Lexing.lexeme_end_p lexbuf in
    raise (LexicalError (string, pos_start, pos_end))
  }
and read_string buf =
  parse
  | '"'       { STRING (Buffer.contents buf) }
  | '\\' '/'  { Buffer.add_char buf '/'; read_string buf lexbuf }
  | '\\' '\\' { Buffer.add_char buf '\\'; read_string buf lexbuf }
  | '\\' 'b'  { Buffer.add_char buf '\b'; read_string buf lexbuf }
  | '\\' 'f'  { Buffer.add_char buf '\012'; read_string buf lexbuf }
  | '\\' 'n'  { Buffer.add_char buf '\n'; read_string buf lexbuf }
  | '\\' 'r'  { Buffer.add_char buf '\r'; read_string buf lexbuf }
  | '\\' 't'  { Buffer.add_char buf '\t'; read_string buf lexbuf }
  | [^ '"' '\\']+
    { Buffer.add_string buf (Lexing.lexeme lexbuf);
      read_string buf lexbuf
    }
  | _ { raise (SyntaxError ("Illegal string character: " ^ Lexing.lexeme lexbuf)) }
  | eof { raise (SyntaxError ("String is not terminated")) }

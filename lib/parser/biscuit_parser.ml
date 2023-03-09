open Core
open Lexing

module Error = struct
  type t = ParserError of string

  let to_string = function ParserError msg -> msg
end

let position_to_string lexbuf =
  let pos = lexbuf.lex_curr_p in
  sprintf "%s:%d:%d" pos.pos_fname pos.pos_lnum (pos.pos_cnum - pos.pos_bol + 1)

let parse_lexbuf lexbuf =
  try Ok (Parser.block Lexer.token lexbuf)
  with Parser.Error ->
    let msg = sprintf "%s: syntax error\n" (position_to_string lexbuf) in
    Error (Error.ParserError msg)

let ( >> ) f g x = g (f x)
let parse_string = Lexing.from_string >> parse_lexbuf
let parse_channel = Lexing.from_channel >> parse_lexbuf

module Authorizer = struct
  let parse_lexbuf lexbuf =
    try Ok (Parser.authorizer Lexer.token lexbuf)
    with Parser.Error ->
      let msg = sprintf "%s: syntax error\n" (position_to_string lexbuf) in
      Error (Error.ParserError msg)

  let parse_string = Lexing.from_string >> parse_lexbuf
  let parse_channel = Lexing.from_channel >> parse_lexbuf
end

let%test_module "Tests" =
  (module struct
    let parse_and_print =
      parse_string
      >> Result.map_error ~f:(function Error.ParserError msg -> msg)
      >> Result.ok_or_failwith >> Ast.Block.print

    let%expect_test _ =
      parse_and_print "";
      [%expect {| () |}]

    let%expect_test _ =
      parse_and_print {| user("123"); |};
      [%expect {| ((Fact((name user)(terms((String 123)))))) |}]

    let%expect_test _ =
      parse_and_print {| user(123); |};
      [%expect {| ((Fact((name user)(terms((Number 123)))))) |}]

    let%expect_test _ =
      parse_and_print {| user(true); |};
      [%expect {| ((Fact((name user)(terms((Bool true)))))) |}]

    let%expect_test _ =
      parse_and_print {| user(hex:abc1234); |};
      [%expect {| ((Fact((name user)(terms((Hex abc1234)))))) |}]

    let%expect_test _ =
      parse_and_print {| user(1989-12-21T12:00:00Z); |};
      [%expect
        {| ((Fact((name user)(terms((DateTime 1989-12-21T12:00:00-00:00)))))) |}]

    let%expect_test _ =
      parse_and_print {| user("test", 1989-12-21T12:00:00Z); |};
      [%expect
        {| ((Fact((name user)(terms((String test)(DateTime 1989-12-21T12:00:00-00:00)))))) |}]

    let%expect_test _ =
      parse_and_print {| user(["test", -123, 1989-12-21T12:00:00Z]); |};
      [%expect
        {| ((Fact((name user)(terms((Set((String test)(Number -123)(DateTime 1989-12-21T12:00:00-00:00)))))))) |}]

    let%expect_test _ =
      parse_and_print {| check if a(42) or b("abc"), c(-2); |};
      [%expect
        {| ((Check((kind One)(queries(((elements((Predicate((name a)(terms((Fact(Number 42)))))))))((elements((Predicate((name b)(terms((Fact(String abc))))))(Predicate((name c)(terms((Fact(Number -2)))))))))))))) |}]

    let%expect_test _ =
      parse_and_print {| check if time($time), $time <= 2018-12-20T00:00:00Z; |};
      [%expect
        {| ((Check((kind One)(queries(((elements((Predicate((name time)(terms((Variable time)))))(Expression((ElementTerm(term(Term(Variable time)))(method_call()))((LowerEqualThan(ElementTerm(term(Term(Fact(DateTime 2018-12-20T00:00:00-00:00))))(method_call())))))))))))))) |}]

    let%expect_test _ =
      parse_and_print {| right($0, "read") <- resource($0), user_id($1); |};
      [%expect
        {| ((Rule((head((name right)(terms((Variable 0)(Fact(String read))))))(body((elements((Predicate((name resource)(terms((Variable 0)))))(Predicate((name user_id)(terms((Variable 1)))))))))))) |}]

    let%expect_test _ =
      parse_and_print {| allow if true; |};
      [%expect
        {| ((Policy((action Allow)(queries(((elements((Expression((ElementTerm(term(Term(Fact(Bool true))))(method_call()))())))))))))) |}]

    let%expect_test _ =
      parse_and_print {| deny if query(1, 2); |};
      [%expect
        {| ((Policy((action Deny)(queries(((elements((Predicate((name query)(terms((Fact(Number 1))(Fact(Number 2)))))))))))))) |}]
  end)

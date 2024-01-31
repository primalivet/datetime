open Base
open Datetime
open Alcotest

let reason_testable =
  testable (Fmt.of_to_string Parser.ParserResult.to_string) Stdlib.( = )
;;

let presult_testable (type a) (a_testable : a testable) : a Parser.ParserResult.t testable
  =
  result (triple a_testable string int) reason_testable
;;

let test_pchar () =
  let open Parser.ParserResult in
  let p = Parser.pchar 'a' in
  check (presult_testable char) "parser 'a'" (Ok ('a', "abc", 1)) (Parser.run p "abc" 0);
  check
    (presult_testable char)
    "parser 'a' (failure, no match)"
    (Error (ExpectedButGot ("a", "c")))
    (Parser.run p "cba" 0);
  check
    (presult_testable char)
    "parser 'a' (failure, empty input)"
    (Error NoMoreInput)
    (Parser.run p "" 0);
  let p = Parser.pchar '1' in
  check
    (presult_testable char)
    "parser '1' (number)"
    (Ok ('1', "123", 1))
    (Parser.run p "123" 0)
;;

let test_parse_lowercase_char () =
  let p = Parser.parse_lowercase_char in
  check
    (presult_testable char)
    "parse_lowercase_char"
    (Ok ('a', "abc", 1))
    (Parser.run p "abc" 0);
  let char_after_z = Char.to_int 'z' + 1 |> Char.of_int_exn |> Char.to_string in
  check
    (presult_testable char)
    "parse_lowercase_char (failuer, after 'z')"
    (Error (ExpectedButGot ("z", char_after_z)))
    (Parser.run p char_after_z 0);
  let char_before_a = Char.to_int 'a' - 1 |> Char.of_int_exn |> Char.to_string in
  check
    (presult_testable char)
    "parse_lowercase_char (failuer, after 'z')"
    (Error (ExpectedButGot ("z", char_before_a)))
    (Parser.run p char_before_a 0)
;;

(* let test_pdigit () = *)
(*   let open Parser.ParserResult in *)
(*   let p = Parser.pdigit 0 in *)
(*   check (presult_testable int) "pdigit 0" (Ok (0, "0123", 1)) (Parser.run p "0123" 0); *)
(*   let p = Parser.pdigit 9 in *)
(*   check (presult_testable int) "pdigit 9" (Ok (9, "9876", 1)) (Parser.run p "9876" 0); *)
(*   check *)
(*     (presult_testable int) *)
(*     "pdigit (failure, empty input)" *)
(*     (Error NoMoreInput) *)
(*     (Parser.run p "" 0); *)
(*   let p = Parser.pdigit 10 in *)
(*   check *)
(*     (presult_testable int) *)
(*     "pdigit (failure, invalid input)" *)
(*     (Error (InvalidInput "10")) *)
(*     (Parser.run p "10" 0) *)
(* ;; *)

(* let test_pdigit_any () = *)
(*   let open Parser.ParserResult in *)
(*   let p = Parser.pdigit_any in *)
(*   check (presult_testable int) "pdigit_any 0" (Ok (0, "0123", 1)) (Parser.run p "0123" 0); *)
(*   check (presult_testable int) "pdigit_any 9" (Ok (9, "9876", 1)) (Parser.run p "9876" 0); *)
(*   check *)
(*     (presult_testable int) *)
(*     "pdigit_any (failure, empty input)" *)
(*     (Error NoMoreInput) *)
(*     (Parser.run p "" 0); *)
(*   check *)
(*     (presult_testable int) *)
(*     "pdigit_any (failure, invalid digit)" *)
(*     (Error (ExpectedButGot ("1", "a"))) *)
(*     (Parser.run p "a" 0) *)
(* ;; *)

let test_and_then () =
  let open Parser.ParserResult in
  let p = Parser.and_then (Parser.pchar 'a') (Parser.pchar 'b') in
  check
    (presult_testable (pair char char))
    "and_then"
    (Ok (('a', 'b'), "ab", 2))
    (Parser.run p "ab" 0);
  check
    (presult_testable (pair char char))
    "and_then (failure, first dont match)"
    (Error (ExpectedButGot ("a", "b")))
    (Parser.run p "bb" 0);
  check
    (presult_testable (pair char char))
    "and_then (failure, second dont match)"
    (Error (ExpectedButGot ("b", "a")))
    (Parser.run p "aa" 0);
  check
    (presult_testable (pair char char))
    "and_then (failure, empty input)"
    (Error NoMoreInput)
    (Parser.run p "" 0);
  check
    (presult_testable (pair char char))
    "and_then (failure, only 1 char input)"
    (Error NoMoreInput)
    (Parser.run p "a" 0)
;;

(* TODO: Test that and_then can take parsers of muliple types, since it's result is a tuple. *)
(* let p = Parser.and_then (Parser.pchar 'a') (Parser.pdigit 'b') *)

let test_or_else () =
  let p = Parser.or_else (Parser.pchar 'a') (Parser.pchar 'b') in
  check
    (presult_testable char)
    "or_else (first match)"
    (Ok ('a', "ab", 1))
    (Parser.run p "ab" 0);
  check
    (presult_testable char)
    "or_else (second match)"
    (Ok ('b', "ba", 1))
    (Parser.run p "ba" 0);
  check
    (presult_testable char)
    "or_else (no match)"
    (Error (ExpectedButGot ("b", "c")))
    (Parser.run p "cde" 0)
;;

let test_parse_uppercase_char () = Alcotest.skip ()
let test_parser_digit () = Alcotest.skip ()
let test_choice () = Alcotest.skip ()

let () =
  run
    "Parser"
    [ "pchar", [ test_case "pchar" `Quick test_pchar ]
    ; ( "parse_lowercase_char"
      , [ test_case "parse_lowercase_char" `Quick test_parse_lowercase_char ] )
    ; ( "parse_uppercase_char"
      , [ test_case "parse_uppercase_char" `Quick test_parse_uppercase_char ] )
    ; "parse_digit", [ test_case "parse_digit" `Quick test_parser_digit ]
    ; "and_then", [ test_case "pdigit" `Quick test_and_then ]
    ; "or_else", [ test_case "or_else" `Quick test_or_else ]
    ; "choice", [ test_case "choice" `Quick test_choice ]
    ]
;;

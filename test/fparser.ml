open Base
open Datetime

let make_testable_parser_result eq_a pp_a =
  let open Stdlib.Format in
  let open Fparser in
  let open Fparser.ParserResult in
  let make_a_pp pp_a =
    let pp fmt = function
      | Ok (value, state) -> fprintf fmt "Ok(%a, %a)" pp_a value Fparser.State.pp state
      | Error (label, reason) ->
        fprintf fmt "Error(%a, %a)" Label.pp label Reason.pp reason
    in
    pp
  in
  let make_a_equal eq_a result_a result_b =
    match result_a, result_b with
    | Ok (a_value, a_state), Ok (b_v, b_s) -> eq_a a_value b_v && State.equal a_state b_s
    | Error (a_label, a_reason), Error (b_label, b_reason) ->
      Label.equal a_label b_label && Reason.equal a_reason b_reason
    | _, _ -> false
  in
  Alcotest.testable (make_a_pp pp_a) (make_a_equal eq_a)
;;

let testable_parser_result_char =
  make_testable_parser_result Char.equal Stdlib.Format.pp_print_char
;;

let testable_parser_result_int =
  make_testable_parser_result Int.equal Stdlib.Format.pp_print_int
;;

let test_pchar () =
  let open Fparser in
  let expected =
    ParserResult.Ok ('a', { State.lines = [ "abc" ]; position = { line = 0; column = 1 } })
  in
  let actual = run (pchar 'a') "abc" in
  Alcotest.check testable_parser_result_char "parser 'a'" expected actual
;;

let test_pint () =
  let open Fparser in
  let expected =
    ParserResult.Ok (123, { State.lines = [ "123" ]; position = { line = 0; column = 2 } })
  in
  let actual = run pint "123" in
  Alcotest.check testable_parser_result_int "parser 'a'" expected actual
;;

let () =
  Alcotest.run
    "FParser"
    [ "pchar", [ Alcotest.test_case "pchar" `Quick test_pchar ]
    ; "pint", [ Alcotest.test_case "pint" `Quick test_pint ]
    ]
;;

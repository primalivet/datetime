let parser_result x = Alcotest.(option (pair x string))

let test_parse_any_char expected input =
  Alcotest.(check (parser_result char))
    "same parser result"
    expected
    (Datetime.Scratch.parse_any_char input)
;;

let test_parse_char expected char input =
  Alcotest.(check (parser_result char))
    "same parser result"
    expected
    (Datetime.Scratch.parse_char char input)
;;

let test_parse_any_digit expected input =
  Alcotest.(check (parser_result int))
    "same parser result"
    expected
    (Datetime.Scratch.parse_any_digit input)
;;

let test_parse_digit expected digit input =
  Alcotest.(check (parser_result int))
    "same parser result"
    expected
    (Datetime.Scratch.parse_digit digit input)
;;

let () =
  let open Alcotest in
  run
    "Scratch"
    [ ( "Any char parser"
      , [ test_case "Any char" `Quick (fun () ->
            test_parse_any_char (Some ('a', "bc")) "abc")
        ; test_case "Any char (empty input)" `Quick (fun () ->
            test_parse_any_char None "")
        ; test_case "Any char (digit input)" `Quick (fun () ->
            test_parse_any_char None "123")
        ; test_case "Any char (single input)" `Quick (fun () ->
            test_parse_any_char (Some ('a', "")) "a")
        ] )
    ; ( "Char parser"
      , [ test_case "Char" `Quick (fun () -> test_parse_char (Some ('a', "bc")) 'a' "abc")
        ; test_case "Char (empty input)" `Quick (fun () -> test_parse_char None 'a' "")
        ; test_case "Char (digit input)" `Quick (fun () -> test_parse_char None 'a' "123")
        ; test_case "Char (single input)" `Quick (fun () ->
            test_parse_char (Some ('a', "")) 'a' "a")
        ; test_case "Char (other input)" `Quick (fun () -> test_parse_char None 'a' "b")
        ] )
    ; ( "Any digit parser"
      , [ test_case "Any digit" `Quick (fun () ->
            test_parse_any_digit (Some (1, "23")) "123")
        ; test_case "Any digit (empty input)" `Quick (fun () ->
            test_parse_any_digit None "")
        ] )
    ; ( "Digit parser"
      , [ test_case "Digit" `Quick (fun () -> test_parse_digit (Some (1, "23")) '1' "123")
        ; test_case "Digit (empty input)" `Quick (fun () -> test_parse_digit None '1' "")
        ] )
    ]
;;

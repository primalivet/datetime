open Base

module ParserResult = struct
  type 'a t = ('a * string * int, reason) Result.t

  and reason =
    | NoMoreInput
    | InvalidInput of string
    | ExpectedButGot of (string * string)

  let to_string = function
    | NoMoreInput -> "No more input"
    | InvalidInput actual -> Printf.sprintf "Invalid input, got: %s" actual
    | ExpectedButGot (expected, actual) ->
      Printf.sprintf "Expected '%s' but got '%s'" expected actual
  ;;

  let ppm pp_a fmt = function
    | Ok (a, s, i) -> Stdlib.Format.fprintf fmt "Ok (%a, %s, %d)" pp_a a s i
    | Error reason -> Stdlib.Format.fprintf fmt "Error (%s)" (to_string reason)
  ;;

  let equal eq_a a b =
    match a, b with
    | Ok (ca, sa, ia), Ok (cb, sb, ib) -> eq_a ca cb && String.equal sa sb && ia = ib
    | Error a, Error b -> String.equal (to_string a) (to_string b)
    | _ -> false
  ;;
end

type 'a t = { parser : string -> int -> 'a ParserResult.t }

let parser fn = { parser = fn }
let run parser input index = parser.parser input index
let return value = { parser = (fun s i -> Ok (value, s, i)) }

let check_out_of_bounds s i =
  if i >= String.length s then Error ParserResult.NoMoreInput else Ok (s.[i], s, i)
;;

let map f p =
  let inner s i =
    let result = run p s i in
    match result with
    | Ok (a, s, i) -> Ok (f a, s, i)
    | Error reason -> Error reason
  in
  parser inner
;;

let is_legal_digit int = int >= 0 && int <= 9

let pchar expected =
  let inner s i =
    if i >= String.length s
    then Error ParserResult.NoMoreInput
    else if Char.equal expected s.[i]
    then Ok (expected, s, i + 1)
    else
      Error (ParserResult.ExpectedButGot (String.of_char expected, String.of_char s.[i]))
  in
  parser inner
;;

(* let pdigit expected = *)
(*   let expected_char = Char.of_int (expected + 48) in *)
(*   let inner s i = *)
(*     let out_of_bounds = i >= String.length s in *)
(*     match is_legal_digit expected, out_of_bounds, expected_char with *)
(*     | false, _, _ | _, _, None -> *)
(*       Error (ParserResult.InvalidInput (Int.to_string expected)) *)
(*     | _, true, Some _ -> Error ParserResult.NoMoreInput *)
(*     | _, false, Some c when Char.equal c s.[i] -> Ok (expected, s, i + 1) *)
(*     | _, false, Some c -> *)
(*       Error (ParserResult.ExpectedButGot (Char.to_string c, String.of_char s.[i])) *)
(*   in *)
(*   parser inner *)
(* ;; *)

(* let pdigit_any = *)
(*   let inner s i = *)
(*     if i >= String.length s *)
(*     then Error ParserResult.NoMoreInput *)
(*     else if Char.(s.[i] >= '0' && s.[i] <= '9') *)
(*     then Ok (Char.to_int s.[i] - Char.to_int '0', s, i + 1) *)
(*     else Error (ParserResult.ExpectedButGot ("1", String.of_char s.[i])) *)
(*   in *)
(*   parser inner *)
(* ;; *)

let and_then parser1 parser2 =
  let inner s i =
    let result1 = run parser1 s i in
    match result1 with
    | Error reason -> Error reason
    | Ok (a1, s, i) ->
      let result2 = run parser2 s i in
      (match result2 with
       | Error reason -> Error reason
       | Ok (a2, s, i) -> Ok ((a1, a2), s, i))
  in
  parser inner
;;

let or_else parser1 parser2 =
  let inner s i =
    let result1 = run parser1 s i in
    match result1 with
    | Ok result -> Ok result
    | Error _ -> run parser2 s i
  in
  parser inner
;;

let choice parsers = List.reduce_exn ~f:or_else parsers
let any_of chars = List.reduce_exn ~f:or_else (List.map chars ~f:pchar)

let char_range start stop =
  List.range ~stop:`inclusive (Char.to_int start) (Char.to_int stop)
  |> List.map ~f:Char.of_int_exn
;;

let parse_lowercase_char = any_of (char_range 'a' 'z')
let parse_uppercase_char = any_of (char_range 'A' 'Z')
let parse_digit = any_of (char_range '0' '9')

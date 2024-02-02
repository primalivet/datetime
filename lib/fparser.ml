open Base

module Position = struct
  type t =
    { line : int
    ; column : int
    }

  let equal pos1 pos2 =
    let same_line = Int.equal pos1.line pos2.line in
    let same_column = Int.equal pos1.column pos2.column in
    same_line && same_column
  ;;

  (** Create a empty intial position of the State or line and column to zero *)
  let empty = { line = 0; column = 0 }

  (** Increment the position in State by one column *)
  let inc_column pos = { pos with column = pos.column + 1 }

  (** Increment the position in State by one line *)
  let inc_line pos = { pos with line = pos.line + 1 }

  let pp fmt pos =
    let open Stdlib in
    Format.fprintf fmt "{ line = %d; column = %d }" pos.line pos.column
  ;;
end

(** State
    Represents the input also looked upon as state, to the parser *)
module State = struct
  type t =
    { lines : string list
    ; position : Position.t
    }

  (** Retrive the current line from the State *)
  let current_line input = List.nth input.lines input.position.line

  let to_position s = s.position

  (** Create a new State from a string *)
  let from_string s =
    if String.is_empty s
    then { lines = []; position = Position.empty }
    else { lines = String.split_lines s; position = Position.empty }
  ;;

  let next_char input : t * char option =
    let curr_line = current_line input in
    match curr_line with
    | Some curr_line ->
      if input.position.column < String.length curr_line
      then (
        let char = curr_line.[input.position.column] in
        let new_pos = Position.inc_column input.position in
        let new_state = { input with position = new_pos } in
        new_state, Some char)
      else (
        let next_line =
          current_line { input with position = Position.inc_line input.position }
        in
        match next_line with
        | Some _ ->
          let char = '\n' in
          let new_pos = Position.inc_line input.position in
          let new_state = { input with position = new_pos } in
          new_state, Some char
        | None -> input, None)
    | None -> input, None
  ;;

  let equal input1 input2 =
    let same_lines = List.equal String.equal input1.lines input2.lines in
    let same_position = Position.equal input1.position input2.position in
    same_lines && same_position
  ;;

  let pp fmt input =
    Stdlib.Format.fprintf
      fmt
      "{ lines = %s; position = %a }"
      (input.lines |> String.concat ~sep:"\n")
      Position.pp
      input.position
  ;;
end

module Label = struct
  type t = string

  let equal = String.equal
  let pp fmt label = Stdlib.Format.fprintf fmt "%s" label
end

module Reason = struct
  type t =
    { message : string
    ; current_line : string
    ; position : Position.t
    }

  let equal reason1 reason2 =
    String.equal reason1.message reason2.message
    && String.equal reason1.current_line reason2.current_line
    && Position.equal reason1.position reason2.position
  ;;

  let pp fmt reason =
    Stdlib.Format.fprintf
      fmt
      "%s at (%d:%d): %s"
      reason.message
      reason.position.line
      reason.position.column
      reason.current_line
  ;;
end

module ParserResult = struct
  type 'a t =
    | Ok of 'a
    | Error of (Label.t * Reason.t)

  let equal (eqa : 'a -> 'a -> bool) result1 result2 =
    match result1, result2 with
    | Ok a1, Ok a2 -> eqa a1 a2
    | Error (label1, reason1), Error (label2, reason2) ->
      String.equal label1 label2 && Reason.equal reason1 reason2
    | _, _ -> false
  ;;

  (** Print the result of a parser as a string *)
  let pp ppa fmt result =
    match result with
    | Ok (value, _) -> Printf.sprintf "%a" ppa value |> Stdlib.print_endline
    | Error (label, reason) ->
      let caret = Printf.sprintf "%*s^%s" reason.position.column "" reason.message in
      let message =
        Printf.sprintf
          "%i:%i Error parsing %s\n%s\n%s"
          reason.position.line
          reason.position.column
          label
          reason.message
          caret
      in
      Stdlib.Format.fprintf fmt "%s" message
  ;;
end

(** Represents a parser *)
type 'a t =
  { fn : State.t -> ('a * State.t) ParserResult.t
  ; label : Label.t
  }

(** Run a parser on a input (State.t) *)
let run_on_input p input = p.fn input

(** Run a parser on a string *)
let run parser s =
  let input = State.from_string s in
  run_on_input parser input
;;

(** Get the label of a parser *)
let get_label p = p.label

(** Set the label of a parser *)
let set_label p new_label =
  let open ParserResult in
  let new_inner input =
    let result = p.fn input in
    match result with
    | Ok (value, input) -> Ok (value, input)
    | Error (_, error) -> Error (new_label, error)
  in
  { fn = new_inner; label = new_label }
;;

(** set_label infix operator, see Parser.set_label *)
let ( <?> ) = set_label

(** Satisfy a predicate on the next character of the input *)
let satisfy pred label =
  let open ParserResult in
  let inner input =
    let remaining_input, char_opt = State.next_char input in
    match char_opt with
    | None ->
      let reason : Reason.t =
        { message = "No more input"
        ; position = State.to_position input
        ; current_line = State.current_line input |> Option.value ~default:"EOF"
        }
      in
      Error (label, reason)
    | Some char ->
      if pred char
      then Ok (char, remaining_input)
      else (
        let reason : Reason.t =
          { message = Printf.sprintf "Unexpected '%c'" char
          ; position = State.to_position input
          ; current_line = State.current_line input |> Option.value ~default:"EOF"
          }
        in
        Error (label, reason))
  in
  { fn = inner; label }
;;

(** Moniadic bind for parser, used to chain parsers *)
let bind f p =
  let open ParserResult in
  let label = "bind unknown" in
  let inner input =
    let result_1 = run_on_input p input in
    match result_1 with
    | Error (label, reason) -> Error (label, reason)
    | Ok (value_1, remaining_input) ->
      let parser2 = f value_1 in
      run_on_input parser2 remaining_input
  in
  { fn = inner; label }
;;

(** bind infix operator, see Parser.bind *)
let ( >>= ) p f = bind f p

let return x =
  let open ParserResult in
  let inner input = Ok (x, input) in
  { fn = inner; label = "return unknown" }
;;

(* Forward function composition, create a new function out of two functions *)
let ( >> ) f g x = g (f x)

(** Map a function overa parser of a value
    Examples (below are the same):
    - let map f = bind (fun x -> return (f x))
    - let map f = bind (f >> return) *)
let map f = bind (f >> return)

(** Map infix operator, see Parser.map **)
let ( <!> ) = map

(** Forward piping operator, arguments are flipped compared to map, see Parser.map **)
let ( |>> ) p f = map f p

(** Apply a parser of a function with a parser of an argument *)
let apply fp xp = fp >>= fun f -> xp >>= fun x -> return (f x)

(** apply infix operator, see Parser.apply **)
let ( <*> ) = apply

(** Apply two seperate parsers of values to a parser of a function *)
let lift2 f xp yp = return f <*> xp <*> yp

(** Run a first parser, if successfull, run the second parser *)
let and_then p1 p2 =
  let label = Printf.sprintf "%s and_then %s" (get_label p1) (get_label p2) in
  p1 >>= (fun r1 -> p2 >>= fun r2 -> return (r1, r2)) <?> label
;;

(** and_then infix operator, see Parser.and_then **)
let ( @>>@ ) = and_then

(** Run a first parser, if failure, run the second parser *)
let or_else p1 p2 =
  let label = Printf.sprintf " %s or_else %s" (get_label p1) (get_label p2) in
  let inner input =
    let r1 = run_on_input p1 input in
    match r1 with
    | Ok _ -> r1
    | Error _ -> run_on_input p2 input
  in
  { fn = inner; label }
;;

(** or_else infix operator, see Parser.or_else **)
let ( <|> ) = or_else

(** Choice any of a list of parsers *)
let choice ps = List.reduce_exn ~f:( <|> ) ps

(** Run a list of parsers in sequence *)
let rec sequence ps =
  let cons h t = h :: t in
  match ps with
  | [] -> return []
  | p :: ps -> lift2 cons p (sequence ps)
;;

(** Helper to collect parsed values, cannot fail as zero will end up as an empty
    list and therefor always succeed *)
let rec zero_or_more p input =
  let fst_result = run_on_input p input in
  match fst_result with
  | Error _ -> [], input
  | Ok (fst_value, input_after_fst) ->
    let subsequent_values, remaining_input = zero_or_more p input_after_fst in
    let values = fst_value :: subsequent_values in
    values, remaining_input
;;

(** Matches a parser zero or more times *)
let many p =
  let open ParserResult in
  let label = Printf.sprintf "many %s" (get_label p) in
  let inner input = Ok (zero_or_more p input) in
  { fn = inner; label }
;;

(** Matches a parser one or more times *)
let many1 p =
  let label = Printf.sprintf "many1 %s" (get_label p) in
  p >>= fun fst -> many p >>= fun rest -> return (fst :: rest) <?> label
;;

(** Matches a optional parser *)
let optional p =
  let label = Printf.sprintf "optional %s" (get_label p) in
  let some = p |>> Option.some in
  let none = return None in
  some <|> none <?> label
;;

(** Keep the left side result, but both must pass *)
let ( >>@ ) p1 p2 = p1 @>>@ p2 |> map (fun (_, b) -> b)

(** Keep the right side result, but both must pass *)
let ( @>> ) p1 p2 = p1 @>>@ p2 |> map (fun (a, _) -> a)

(** Keep only the middle result, but all three must pass *)
let between p1 p2 p3 = p1 >>@ p2 @>> p3

(** Parses one or more occorences of a parser seperated by a seperator *)
let sep_by1 p sep =
  let sep_then = sep >>@ p in
  p @>>@ many sep_then |>> fun (x, xs) -> x :: xs
;;

(** Parses zero or more occorences of a parser seperated by a seperator *)
let sep_by p sep = sep_by1 p sep <|> return []

(** Parse a character *)
let pchar c =
  let label = Printf.sprintf "'%c'" c in
  let pred = Char.equal c in
  satisfy pred label
;;

(** Parse any of a list of characters *)
let char_any_of cs =
  let label = Printf.sprintf "char_any_of %s" (String.of_char_list cs) in
  cs |> List.map ~f:pchar |> choice <?> label
;;

(** Parses a sequence of zero or more characters using a char parser (cp) *)
let pmany_chars cp =
  let label = Printf.sprintf "many_chars %s" (get_label cp) in
  many cp |>> String.of_char_list <?> label
;;

(** Parses a sequence of one or more characters using a char parser (cp) *)
let pmany_chars1 cp =
  let label = Printf.sprintf "many_chars1 %s" (get_label cp) in
  many1 cp |>> String.of_char_list <?> label
;;

(** Parses a string *)
let pstring s =
  let label = Printf.sprintf "string %s" s in
  String.to_list s |> List.map ~f:pchar |> sequence |>> String.of_char_list <?> label
;;

(** Parses a whitespace character *)
let whitespace =
  let label = "whitespace" in
  let pred = Char.is_whitespace in
  satisfy pred label
;;

(** Prases zeror or more whitespace charaters *)
let spaces = many whitespace <?> "spaces"

(** Prases on or more whitespace charaters *)
let spaces1 = many1 whitespace <?> "spaces1"

(** Parses a digit *)
let pdigit =
  let label = "digit" in
  let pred = Char.is_digit in
  satisfy pred label
;;

(** Parses a integer *)
let pint =
  let label = "int" in
  let result_to_int = function
    | Some _, digits -> -Int.of_string digits
    | None, digits -> Int.of_string digits
  in
  let digits = pmany_chars1 pdigit in
  optional (pchar '-') @>>@ digits |> map result_to_int <?> label
;;

(** Parses a float *)
let pfloat =
  let label = "float" in
  let result_to_float (sign_opt, (digits1, (_point, digits2))) =
    let float = Printf.sprintf "%s.%s" digits1 digits2 |> Float.of_string in
    match sign_opt with
    | Some _ -> -.float
    | None -> float
  in
  let digits = pmany_chars1 pdigit in
  optional (pchar '-') @>>@ digits @>>@ pchar '.' @>>@ digits
  |> map result_to_float
  <?> label
;;

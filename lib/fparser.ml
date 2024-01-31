open Base

(** TextInput
    Represents the input also looked upon as state, to the parser *)
module TextInput = struct
  type t =
    { lines : string list
    ; position : position
    }

  and position =
    { line : int
    ; column : int
    }

  (** Set the intial position of the TextInput or line and column to zero *)
  let initial_position = { line = 0; column = 0 }

  (** Increment the position in TextInput by one column *)
  let inc_column pos = { pos with column = pos.column + 1 }

  (** Increment the position in TextInput by one line *)
  let inc_line pos = { pos with line = pos.line + 1 }

  (** Retrive the current line from the TextInput *)
  let current_line input =
    if input.position.line < List.length input.lines
    then Some (List.nth_exn input.lines input.position.line)
    else None
  ;;

  (** Create a new TextInput from a string *)
  let from_string s =
    if String.is_empty s
    then { lines = []; position = initial_position }
    else { lines = String.split_lines s; position = initial_position }
  ;;

  (** Retrive the next character from the TextInput *)
  let next_char input =
    let current_line = current_line input in
    let is_end_of_line =
      current_line
      |> Option.map ~f:(fun line -> input.position.line < String.length line)
      |> Option.value ~default:false
    in
    match current_line, is_end_of_line with
    | None, _ -> input, None
    | _, true ->
      let char = '\n' in
      let new_state = { input with position = inc_line input.position } in
      new_state, Some char
    | Some line, false ->
      let char = line.[input.position.column] in
      let new_state = { input with position = inc_column input.position } in
      new_state, Some char
  ;;
end

(** Type alias for the TextInput.t *)
type input = TextInput.t

(** Represents the label of a parser, used in error messages *)
type parser_label = string

(** Represents the error of a parser, used in error messages *)
type parser_error = string

(** Represents the position of a parser, used in error messages *)
type parser_position =
  { current_line : string
  ; line : int
  ; column : int
  }

(** Represents a result of a parser *)
type 'a parser_result =
  | Success of 'a
  | Failure of (parser_label * parser_error * parser_position)

(** Represents a parser *)
type 'a parser =
  { fn : input -> ('a * input) parser_result
  ; label : parser_label
  }

(** Run a parser on a input (TextInput.t) *)
let run_on_input p input = p.fn input

(** Run a parser on a string *)
let run parser s =
  let input = TextInput.from_string s in
  run_on_input parser input
;;

(** Get a parser_position based of an input (TextInput.t) *)
let parser_position_from_input input =
  match TextInput.current_line input with
  | None ->
    { current_line = ""; line = input.position.line; column = input.position.column }
  | Some line ->
    { current_line = line; line = input.position.line; column = input.position.column }
;;

(** Print the result of a parser as a string *)
let print_result ppa result =
  match result with
  | Success (value, _) -> Printf.sprintf "%a" ppa value |> Stdlib.print_endline
  | Failure (label, error, position) ->
    let caret = Printf.sprintf "%*s^%s" position.column "" error in
    let message =
      Printf.sprintf
        "%i:%i Error parsing %s\n%s\n%s"
        position.line
        position.column
        label
        error
        caret
    in
    message |> Stdlib.print_endline
;;

(** Get the label of a parser *)
let get_label p = p.label

(** Set the label of a parser *)
let set_label p new_label =
  let new_inner input =
    let result = p.fn input in
    match result with
    | Success (value, input) -> Success (value, input)
    | Failure (_, error, position) -> Failure (new_label, error, position)
  in
  { fn = new_inner; label = new_label }
;;

(** set_label infix operator, see Parser.set_label *)
let ( <?> ) = set_label

(** Satisfy a predicate on the next character of the input *)
let satisfy pred label =
  let inner input =
    let remaining_input, char_opt = TextInput.next_char input in
    match char_opt with
    | None ->
      let error = "No more input" in
      let position = parser_position_from_input input in
      Failure (label, error, position)
    | Some char ->
      if pred char
      then Success (char, remaining_input)
      else (
        let err = Printf.sprintf "Unexpected '%c'" char in
        let position = parser_position_from_input input in
        Failure (label, err, position))
  in
  { fn = inner; label }
;;

(** Moniadic bind for parser, used to chain parsers *)
let bind f p =
  let label = "bind unknown" in
  let inner input =
    let result_1 = run_on_input p input in
    match result_1 with
    | Failure (label, error, position) -> Failure (label, error, position)
    | Success (value_1, remaining_input) ->
      let parser2 = f value_1 in
      run_on_input parser2 remaining_input
  in
  { fn = inner; label }
;;

(** bind infix operator, see Parser.bind *)
let ( >>= ) p f = bind f p

let return x =
  let inner input = Success (x, input) in
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
    | Success _ -> r1
    | Failure _ -> run_on_input p2 input
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
  | Failure _ -> [], input
  | Success (fst_value, input_after_fst) ->
    let subsequent_values, remaining_input = zero_or_more p input_after_fst in
    let values = fst_value :: subsequent_values in
    values, remaining_input
;;

(** Matches a parser zero or more times *)
let many p =
  let label = Printf.sprintf "many %s" (get_label p) in
  let inner input = Success (zero_or_more p input) in
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

(** Parse a specific character *)
let char c = 
  let label = Printf.sprintf "'%c'" c in
  let pred = Char.equal c in
  satisfy pred label

(** Parse any of a list of characters *)
let char_any_of cs =
  let label = Printf.sprintf "char_any_of %s" (String.of_char_list cs) in
  cs |> List.map ~f:char |> choice <?> label

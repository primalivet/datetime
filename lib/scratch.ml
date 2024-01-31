open Base

type date =
  { posix : int
  ; zone : string option
  }

let posix_to_date posix = { posix; zone = None }

type 'a parser = string -> ('a * string) option

let digit_to_int c = Char.to_int c - Char.to_int '0'

let parse_any_char : char parser = function
  | "" -> None
  | s ->
    if Char.is_alpha s.[0]
    then Some (s.[0], String.sub s ~pos:1 ~len:(String.length s - 1))
    else None
;;

let parse_char c : char parser = function
  | "" -> None
  | s ->
    if Char.equal s.[0] c
    then Some (c, String.sub s ~pos:1 ~len:(String.length s - 1))
    else None
;;

let parse_any_digit : int parser = function
  | "" -> None
  | s ->
    if Char.is_digit s.[0]
    then Some (digit_to_int s.[0], String.sub s ~pos:1 ~len:(String.length s - 1))
    else None
;;

let parse_digit d : int parser = function
  | "" -> None
  | s ->
    if Char.is_digit s.[0] && Char.equal s.[0] d
    then Some (digit_to_int d, String.sub s ~pos:1 ~len:(String.length s - 1))
    else None
;;

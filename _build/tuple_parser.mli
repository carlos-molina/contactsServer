(* The type of tokens. *)
type token = 
  | VAR of (string)
  | STRING of (string)
  | START
  | RIGHT_SQUARE_BRACE
  | RIGHT_BRACE
  | LEFT_SQUARE_BRACE
  | LEFT_BRACE
  | EOF
  | END
  | COMMA

(* This exception is raised by the monolithic API functions. *)
exception Error

(* The monolithic API. *)
val parse_tuple: (Lexing.lexbuf -> token) -> Lexing.lexbuf -> (Config.tuple option)

val parse_lst: (Lexing.lexbuf -> token) -> Lexing.lexbuf -> (Config.tuple list)

val parse_collection: (Lexing.lexbuf -> token) -> Lexing.lexbuf -> (Config.tuple list list)


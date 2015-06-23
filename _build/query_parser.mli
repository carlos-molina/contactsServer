(* The type of tokens. *)
type token = 
  | VAR of (string)
  | STRING of (string)
  | START
  | MAP
  | EOF
  | END
  | COMMA

(* This exception is raised by the monolithic API functions. *)
exception Error

(* The monolithic API. *)
val parse: (Lexing.lexbuf -> token) -> Lexing.lexbuf -> (Config.tuple list)


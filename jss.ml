(*  
 @Program :   jsonStrLst.ml
   Shows how to build and work with `List of json list and `Assoc of (string * `String string) list.
  
   The program demonstrates that I can convert `List of json list to string with 
   the help of Yojson.Basic.to_string and recover the `List of json list with the help 
   of Yojson.Basic.from_string
  
  Notice that to_string returns "Some string" , thus, to_str is used before
  printing.

  print_string("First person from jsonLi2tst is: " ^ (to_str per_tst)  ^ "\n");
    
 @Programmer: Carlos Molina Jimenez

          Inspired from Real World OCaml, Yaron Minsky and Anil Madhavapeddy
          p 300

 @Programmer: Carlos Molina J
 @Date: 9 Feb 2015, Computer Laboratory University of Cambridge

 Compilation:   
   #!/bin/bash

   eval `opam config env`

   rm *.byte

   ocamlbuild -r -use-ocamlfind -use-menhir -classic-display -package 
              core,async,cohttp.async,lwt,yojson,atdgen,git.unix,irmin,
              irmin.unix,oUnit,sexplib.syntax,comparelib.syntax,bin_prot.syntax 
              -tag thread  jsonStrLsts.byte -cflags -annot


             % jsonStrLst.native 

 Execution: % jsonStrLst.native
*)

(*
exception WrongJsonList of string
exception WrongJsonNameDataTuple of string

*)

(* open Core.Std *)

(*
 let contactData_to_json name_data= 
  let name= fst(name_data) in
  let data= snd(name_data) in
  (name, `String data)
*)

(*
  converts (jon, `String Crowcroft) into (jon, Crowcroft)
 *)

(*let json_to_contactData name_data=  
  let name= fst(name_data) in
  let data= match snd(name_data) with
       | `String d -> d
       |  _        -> raise (WrongJsonNameDataTuple ("Problems with Name data")) in
  (name, data)

let lst_to_jsonStr contactLst=
  let lst= List.map (fun name_data -> contactData_to_json name_data) contactLst in
  Yojson.Basic.to_string (`Assoc lst)

let someToTup someTup = match someTup with
    | Some tup   -> tup 
    | _          -> raise (WrongJsonNameDataTuple ("Problems with Name data"))

let jsonStr_to_lst jsonStr =
  let jsonLst= Yojson.Basic.from_string jsonStr in
  let lstAssoc= match jsonLst with
      | `Assoc lst -> lst 
      | _          -> raise (WrongJsonList("wrong Assoc json list\n")) in
   List.map (fun name_data -> json_to_contactData name_data) lstAssoc 
   
let tupLstToLst tupLst= List.map (fun name_data -> snd name_data) tupLst
*)

let main () =

  let jsonStrLst= Jhelp.lst_to_jsonStr [("Reye", "Loma"); ("Marcelina", "Medano"); ("Luis", "Callejon")] in
  
  
  let tupLst = Jhelp.jsonStr_to_lst jsonStrLst in  

  let fname= fst(List.hd tupLst) in
  let fdata= snd(List.hd tupLst) in

  print_string("The name:" ^ fname ^ " " ^fdata);

  print_string("\nThe data in list is:" ^ List.nth (Jhelp.tupLstToLst tupLst) 0);
  print_string(" " ^ List.nth (Jhelp.tupLstToLst tupLst) 2);
  
  print_newline ();

  exit 0;;
 main ();;

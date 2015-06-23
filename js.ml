(*  
 @Program :   js.ml
   Shows how to convert to json a list
   [("Reye", "Loma"); ("Marcelina", "Medano"); ("Luis", "Callejon")] 
   and recover it back.
   The file jsonString.ml uses local functions only. 
               
 
 @Programmer: Carlos Molina Jimenez

          Inspired from Real World OCaml, Yaron Minsky and Anil Madhavapeddy
          p 300

 @Programmer: Carlos Molina J
 @Date: 25 Feb 2015, Computer Laboratory University of Cambridge

 Compilation:   
   #!/bin/bash

   eval `opam config env`

   rm *.byte

   ocamlbuild -r -use-ocamlfind -use-menhir -classic-display -package 
              core,async,cohttp.async,lwt,yojson,atdgen,git.unix,irmin,
              irmin.unix,oUnit,sexplib.syntax,comparelib.syntax,bin_prot.syntax 
              -tag thread  jsonStrLsts.byte -cflags -annot


 Execution: % js.native
*)


let main () =

  let jsonStrLst= Jsonhelper.lst_to_jsonStr [("Reyes", "Loma"); ("Marcelina", "Medano"); ("Luis", "Callejon")] in
  
  
  let tupLst = Jsonhelper.jsonStr_to_lst jsonStrLst in  

  let fname= fst(List.hd tupLst) in
  let fdata= snd(List.hd tupLst) in

  print_string("The name:" ^ fname ^ " " ^fdata);

  print_string("\nThe data in list is:" ^ List.nth (Jsonhelper.tupLstToLst tupLst) 0);
  print_string(" " ^ List.nth (Jsonhelper.tupLstToLst tupLst) 2);
  
  print_newline ();

  exit 0;;
 main ();;

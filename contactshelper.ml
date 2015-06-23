(*
* Copyright (c) 2014 Carlos Molina Jimenez
*
* Permission to use, copy, modify, and distribute this software for any
* purpose with or without fee is hereby granted, provided that the above
* copyright notice and this permission notice appear in all copies.
*
* THE SOFTWARE IS PROVIDED "AS IS" AND THE AUTHOR DISCLAIMS ALL WARRANTIES
* WITH REGARD TO THIS SOFTWARE INCLUDING ALL IMPLIED WARRANTIES OF
* MERCHANTABILITY AND FITNESS. IN NO EVENT SHALL THE AUTHOR BE LIABLE FOR
* ANY SPECIAL, DIRECT, INDIRECT, OR CONSEQUENTIAL DAMAGES OR ANY DAMAGES
* WHATSOEVER RESULTING FROM LOSS OF USE, DATA OR PROFITS, WHETHER IN AN
* ACTION OF CONTRACT, NEGLIGENCE OR OTHER TORTIOUS ACTION, ARISING OUT OF
* OR IN CONNECTION WITH THE USE OR PERFORMANCE OF THIS SOFTWARE.
*)

open Config
  
(* open Yojson *)

exception TupleListIsEmpty  of string;;
exception WrongJsonRecord   of string;;
exception WrongString       of string;;
  

(**
 @author: Carlos Molina 
 @date:   12 Feb 2015, Computer Laboratory, Univ. of Cambridge
 Converts a query tuple to string 
 *)
(* let qry_to_string =
  function
  | {
      subj     = Variable s;
      pred     = Constant p;
      obj      = Constant o;
      ctxt     = Constant c;
      time_stp = _;
      sign = _ } -> Printf.sprintf "< %s %s %s >" s p o
  | _ -> "Not printing this query."
*)


(**
 @author: Carlos Molina
 @date:   11 Jun 2015, Computer Laboratory, Univ. of Cambridge
 It expects a string that representes a first name (fn), last name
 or email address. 
 Returns a tuple that can be used as part of a query. 
 *)
let make_fn_last_email_qry predVal fn= 
 {
  subj= Variable "?y";
  pred= Constant predVal;
  obj=  Constant fn;
  ctxt= Constant "contacts";
  time_stp= None;
  sign= None;
 }

(**
 @author: Carlos Molina
 @date:   5 Jun 2015, Computer Laboratory, Univ. of Cambridge
 It expects a string that representes a first name (fn), such
 as Jon, Carlos, Anil.
 Returns a tuple that can be used as part of a query. 
 *)
let make_fn_qry fn=   (* deprecated 11 Jun 2015 *) 
 {
  subj= Variable "?y";
  pred= Constant "fn";
  obj=  Constant fn;
  ctxt= Constant "contacts";
  time_stp= None;
  sign= None;
 }

(**
 @author: Carlos Molina
 @date:   9 Jun 2015, Computer Laboratory, Univ. of Cambridge
 It expects a string that representes a last name (last), such
 as Crowcroft. 
 Returns a tuple that can be used as part of a query. 
 *)
let make_last_qry last= (* deprecated 11 Jun 2015 *) 
 {
  subj= Variable "?y";
  pred= Constant "last";
  obj=  Constant last;
  ctxt= Constant "contacts";
  time_stp= None;
  sign= None;
 }

(**
 @author: Carlos Molina
 @date:   11 Jun 2015, Computer Laboratory, Univ. of Cambridge
 It expects a string that representes an email, such
 as cm770@cam.ac.uk 
 Returns a tuple that can be used as part of a query. 
 *)
let make_email_qry email= (* deprecated 11 Jun 2015 *) 
 {
  subj= Variable "?y";
  pred= Constant "email";
  obj=  Constant email;
  ctxt= Constant "contacts";
  time_stp= None;
  sign= None;
 }

  
(**
 @author: Carlos Molina 
 @date:   5 Jan 2015, Computer Laboratory, Univ. of Cambridge
 Converts a single tuple like < c fn Anil> < c last Madhavapeddy> ...
 < c title Lecturer > into a string and returns the resulting string.
 *)
let tupleToStr tuple=
  let rec help str tuple= match tuple with
  | []      -> str^"\n--This is nextContact--\n"
  | h::rest -> help (str^(Helper.to_string h)^"\n") rest 
  in help "" tuple


(**
 @author: Carlos Molina 
 @date:   5 Jan 2015, Computer Laboratory, Univ. of Cambridge
 Returns an integer: the length of the list which contains 
 strings that represent tuples
 *)
 let lengthofTupleLst tupleLst = 
    let lst= List.map (fun t -> tupleToStr t) tupleLst in
    List.length lst

(**
 @author: Carlos Molina 
 @date:   5 Jan 2015, Computer Laboratory, Univ. of Cambridge
          5 Jun 2015, I deprecated this function
 Returns a string: the results of converting a list of strings 
 like [s1;s2;s3] into a single string s= s1^s2^s3 
 *)
 let listofStrToStr_5Jun2015 lst= 
     let rec help str lst= match lst with
     | []      -> str^"\nNo more contacts "
     | h::rest -> help (str^h) rest 
     in help "" lst

(**
 @author: Carlos Molina 
 @date:   5 Jun 2015, Computer Laboratory, Univ. of Cambridge
          I updated the original function.
 Returns a string: the results of converting a list of strings 
 like [s1;s2;s3] into a single string s= s1^s2^s3 
 *)
 let listofStrToStr lst= 
     let rec help str lst= match lst with
     | []      -> str
     | h::rest -> help (str^h) rest 
     in help "" lst
    

(**
 @author: Carlos Molina 
 @date:   5 Jan 2015, Computer Laboratory, Univ. of Cambridge
 Returns a string: the result of converting a list of tuples 
 into a string
 *)
 let listoftuples_to_str tuples= 
    let lst= List.map (fun t -> tupleToStr t) tuples in
    listofStrToStr lst



(**
 @author: Carlos Molina-Jimenez 
 @date:   13 Jan 2015, Computer Laboratory, Univ. of Cambridge
 @updatedate:   27 Jan 2015: this function needs to be generalized.
 Returns a string: a query to be presented to a rete map. 
 *) 
 let _makeQuery subj pred qryvar context=
    "MAP{" ^ subj ^ "," ^ pred ^ "," ^ qryvar ^ "," ^ context ^ "}"

(**
 @author: Carlos Molina-Jimenez 
 @date:   13 Jan 2015, Computer Laboratory, Univ. of Cambridge
 Returns a string: a query to be presented to a rete map. It can handle
          only a single-tuple query.
 *) 
 let makeSimpleQry subj pred qryvar context=
    "MAP{" ^ subj ^ "," ^ pred ^ "," ^ qryvar ^ "," ^ context ^ "}"

(****************)

(**
 @author: Carlos Molina 
 @date:   8 Jan 2015, Computer Laboratory, Univ. of Cambridge
 returns a string: the result of converting
 Constant str into str
 *)
let constStrToStr constStr= match constStr with
    | Constant value -> "\n"^value
    | _              -> raise Wrong_value


(**
 @author: Carlos Molina 
 @date:   8 Jan 2015, Computer Laboratory, Univ. of Cambridge
 returns a list of stringis: the result of converting a tuple 
 (a key value pair) like "simon" [Constant "marco"; Constant "defino"]
 into a list like ["simon: "; "marco"; "delfino"]
 *)
let tupleLstToStrLst var lst= 
    let resLst= List.map constStrToStr lst in
    (var^":")::resLst

(**
 @author: Carlos Molina 
 @date:   8 Jan 2015, Computer Laboratory, Univ. of Cambridge
 returns a string: the result of converting a tuple 
 (a key value pair) like "simon" [Constant "marco"; Constant "defino"]
 into a string like "simon: marco delfino"
 *)
let tupleToStr var lst= 
    let resLst= List.map constStrToStr lst in
    listofStrToStr((var^":")::resLst)


let tupleToString tuple=
    let var= fst(tuple) in
    let lst= snd(tuple) in
    tupleToStr var lst

let mapLstToStr mapLst=  listofStrToStr (List.map tupleToString mapLst)

(*
  @Date: 23 Feb 2015 Computer Laboratory, University of Cambridge.
  returns a string: the value of the tuple field presented. 
 *)
let val_to_string = function
    | Variable x -> x
    | Constant x -> x

(*
  returns a string: the result of converting a query of five tuples
  like
   let q2= {
    subj = Variable "?x";
    pred = Constant "hasColor";
    obj = Constant "Red";
    ctxt = Constant "context";
    time_stp = None;
    sign = None; }
  into "?x, hasColor, Red, context" 
*)      
let qry_to_string qry=
 let sb= (val_to_string qry.subj)      in
 let pr= (val_to_string qry.pred)      in
 let ob= (val_to_string qry.obj)       in
 let ct= (val_to_string qry.ctxt)      in
 sb^", "  ^  pr^", "  ^  ob^", "  ^  ct^"\n"

(*
  returns a string: the result of converting a query of five tuples
  like
   let tup_jo1= {
    subj = Constant "a";
    pred = Constant "fn";
    obj =  Constant "Jon";
    ctxt = Constant "contacts";
    time_stp = None;
    sign = None; }
  into "(a,fn,Jon,contacts)" 

  4 Mar 2015 (Carlos Molina): For some reason the current version 
  of Helper.to_tuple_lst cannot conver correctly the following

 let tup_to_string tup=
  let sb= (val_to_string tup.subj)      in
  let pr= (val_to_string tup.pred)      in
  let ob= (val_to_string tup.obj)       in
  let ct= (val_to_string tup.ctxt)      in
  "("^sb^", "  ^  pr^", "  ^  ob^", "  ^  ct^")\n"

 let makeStrOfContactData tupLst=
    let lst= List.map (fun tup -> tup_to_string tup) tupLst in
    let str= List.fold_left (^) "" lst in
    "\n{"^ str ^ "}"
*)      
let tup_to_string tup=
 let sb= (val_to_string tup.subj)      in
 let pr= (val_to_string tup.pred)      in
 let ob= (val_to_string tup.obj)       in
 let ct= (val_to_string tup.ctxt)      in
 "("^sb^","  ^  pr^","  ^  ob^","  ^  ct^")"^"\n"

let makeStrOfContactData tupLst=
    let lst= List.map (fun tup -> tup_to_string tup) tupLst in
    let str= List.fold_left (^) "" lst in
    "{"^ str ^ "}"

let tup_to_str_long tup=
 let sb= (val_to_string tup.subj)      in
 let pr= (val_to_string tup.pred)      in
 let ob= (val_to_string tup.obj)       in
 let ct= (val_to_string tup.ctxt)      in
 "subj= Constant " ^sb^";\n"^
 "pred= Constant " ^pr^";\n"^
 "obj = Constant " ^ob^";\n"^
 "ctxt= Constant " ^ct^";\n"^
 "time_stp= None" ^";\n"^
 "sign    = None" ^";\n"


let prtQryResponse rspLst= 
 print_string("\n Query response: \n");
 let strLst= List.map (fun tup -> tup_to_str_long tup) rspLst in
 let rec prtTup lst= match lst with
  | []      ->  "\n"
  | h::rest -> print_string h; prtTup rest  in
 prtTup strLst



(*
 returns a string: the result of converting a list of queries like
         [q1;q2;q3] into
 "MAP{ 
       ?x, hasColor, Red, context
       ?x, knows, ?o, contacts
     }"
 *)
let makeMapQry qryLst=
    let lst= List.map (fun qry -> qry_to_string qry) qryLst in
    let str= List.fold_left (^) "" lst in
    "\n MAP { \n" ^ str ^ "}"


    (* let lst= List.map (fun t -> tupleToStr t) tupleLst in *)

(* helper function for printing out (variable, values) pairs *)
(* 
 *var is a string, for example "abc", "?email", etc.
 *x is a string: Anil, Carlos, cm770@cam.ac.uk, a, b ,c
 *)

let valuesToStr_var var values =
  let lst= List.map
     (fun vl ->
        match vl with
        | Constant x ->  x
        | _ -> raise Wrong_value)
     values in 
   var^" ..."^(List.hd lst)  
 
(* let lstResultsToStr lst= *)
  

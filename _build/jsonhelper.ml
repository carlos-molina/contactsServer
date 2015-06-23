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

(* open Core *)
open Config
open Yojson 


exception TupleListIsEmpty of  string
exception WrongJsonRecord  of  string
exception WrongJsonLst     of  string
exception WrongJsonString  of  string
exception WrongQuery       of  string 
exception WrongSubjectType of  string 
exception WrongJsonNameDataTuple of string

let emptyLst= []

let emptyJnLst= `List [] 

(* let to_lst (`List lst)= lst   <-- this is wrong *)

let to_jnLst lst= `List lst

let to_lst jlst= match jlst with
    | `List lst -> lst
    | _        -> raise (WrongJsonLst("Given list is not a json list\n"))


let emptyJsonLst= to_jnLst emptyLst



let len jnLst= 
    let lst= to_lst jnLst in
    List.length lst


let isLstEmpty jnLst= 
    let lst= to_lst jnLst in
    match lst with
    | [] -> true
    | _  -> false


(*
 return a json list
 *)
let add jnTuple jnLst= 
    let lst= to_lst jnLst in
    to_jnLst(jnTuple::lst)

let getHd jnLst=
    let lst= to_lst jnLst in
    List.hd lst

let getTl jnLst=
    let lst= to_lst jnLst in
    List.tl lst

let getHdTl jnLst=
    let lst= to_lst jnLst in
    let hd= List.hd lst   in
    let tl= List.tl lst   in
    (hd,tl)


(*
 
 *)
let optionStr_to_str optStr:string= match optStr with
    | Some str -> str
    | None     -> "NoNameFound"

(*
  
 *)  
let optQry_to_qry optQry = match optQry with
    | Some qry  ->  qry
    | _         ->  raise (WrongQuery ("some_to_qry failed "))

(*
 Compare two queries. I use this function for testing
 queries sent in json format.
 *)
let equal_qry1_qry2 qry1 qry2= match qry1=qry2 with
    | true   -> "\nPass\n"
    | falase -> "\nFail\n"



(**
 @author: Carlos Molina 
 @date:   12 Feb 2015, Computer Laboratory, Univ. of Cambridge
 Converts a query tuple to string 
 *)
let qry_to_string =
  function
  | {
      subj     = Variable s;
      pred     = Constant p;
      obj      = Constant o;
      ctxt     = Constant c;
      time_stp = _;
      sign = _ } -> Printf.sprintf "< %s %s %s >" s p o
  | _ -> "Not printing this query."
 


(* 
 The following four functions help extract the string
 and the type (either Variable or Constant) of the elements
 subj, pred, obj and ctxt of a tuple. They are used in the
 conversion from query to json structure
 *)
let subj_to_str subj= match subj with
    | Variable str -> (str, "Variable")
    | Constant str -> (str, "Constant")

let pred_to_str pred= match pred with
    | Variable str -> (str, "Variable")
    | Constant str -> (str, "Constant")

let obj_to_str obj= match obj with
    | Variable str -> (str, "Variable")
    | Constant str -> (str, "Constant")

let ctxt_to_str ctxt= match ctxt with
    | Variable str -> (str, "Variable")
    | Constant str -> (str, "Constant")


(*
   Carlos Molina Jimenez 19 Feb 2015; modified 12 Mar 2015
   returns a json query.
   The keys Subj_t, Pred_t and Obj_t are used to specify the type of
   the Subject, Predicate and Object, respectively, which is either
   Constant or Variable.
 *)
let qry_to_json qry= match qry with
  | {
      subj = Variable s;
      pred = Constant p;
      obj  = Constant o;
      ctxt = Constant c;
      time_stp = _;
      sign = _ 
     } | 
     {
      subj = Variable s;
      pred = Constant p;
      obj  = Variable o;
      ctxt = Constant c;
      time_stp = _;
      sign = _ 
     } | 
      {
      subj = Variable s;
      pred = Variable p;
      obj  = Variable o;
      ctxt = Constant c;
      time_stp = _;
      sign = _ 
     } |  

     {       (* included to support tuples *)
      subj = Constant s; 
      pred = Constant p;
      obj  = Constant o;
      ctxt = Constant c;
      time_stp = _;
      sign = _ 
     } ->
 
  let subj   = qry.subj     in
  let pred   = qry.pred     in
  let obj    = qry.obj      in
  let ctxt   = qry.ctxt     in
  let ti     = qry.time_stp in
  let sg     = qry.sign     in  

  let json_q : Yojson.Basic.json =  
      `Assoc
        [ ("Subject",   `String (fst(subj_to_str subj)));
          ("Predicate", `String (fst(pred_to_str pred)));
          ("Object",    `String (fst(obj_to_str  obj)));
          ("Subj_t",    `String (snd(subj_to_str subj))); 
          ("Pred_t",    `String (snd(pred_to_str pred))); 
          ("Obj_t",     `String (snd(obj_to_str  obj)))
        ] 
    in json_q

  | _ -> raise(WrongJsonRecord "qry_to_json failed\n") 




(* 
  Carlos Molina Jimenez 10 Feb 2015
  returns a query 
 *)
let json_to_qry1 json_q =
  (* FIX ME: Need to take care of Context, Signature and Timestamp *)
  let open Yojson.Basic.Util
  in
    let json = json_q                    in
    let s = json |> (member "Subject")   in
    let p = json |> (member "Predicate") in
    let o = json |> (member "Object")    in
      {
        subj     = Variable (Basic.Util.to_string s);
        pred     = Constant (Basic.Util.to_string p);
        obj      = Constant (Basic.Util.to_string o);
        ctxt     = Constant "context";
        time_stp = None;
        sign     = None;
      }

(* 
  Carlos Molina Jimenez 16 Feb 2015
  It expects a json structure and returns a query 
 *)
let json_to_qry json_q =
  (* FIX ME: Need to take care of Context, Signature and Timestamp *)
  let open Yojson.Basic.Util
  in
    let json = json_q                       in
    let suj  = json |> (member "Subject")   in
    let pre  = json |> (member "Predicate") in
    let obj  = json |> (member "Object")    in
    
    let subjType= json |> (member "Subj_t") |> to_string   in
    let makeSubj subjType = match subjType with
      | "Variable" -> Variable (Basic.Util.to_string suj)
      | "Constant" -> Constant (Basic.Util.to_string suj)  in

    let predType= json |> (member "Pred_t") |> to_string   in
    let makePred predType = match predType with
      | "Variable" -> Variable (Basic.Util.to_string pre) 
      | "Constant" -> Constant (Basic.Util.to_string pre)  in
      
    let objType= json |> (member "Obj_t") |> to_string     in
    let makeObj objType = match  objType with
      | "Variable" -> Variable (Basic.Util.to_string obj) 
      | "Constant" -> Constant (Basic.Util.to_string obj)  in

      {
        subj     = makeSubj subjType;   
        pred     = makePred predType;
        obj      = makeObj  objType;    
        ctxt     = Constant "context";
        time_stp = None;
        sign     = None;
      }

(*
 Converts a list of query parameters like [q1; q2; q3] into a list
 json list like `List of json like `List [jsonq1; jsonq2; jsonq3]
 *) 
let parLstToJsonParLst lst=
    let li= List.map (fun q -> qry_to_json q) lst in
        Yojson.Basic.to_string (to_jnLst li)


(*
 Recover parameter list from json string and produce
 a list like [q1;q2;q3]
 *)
let getQryParLst jsonStr=
 let open Yojson.Basic.Util                    in
 let jsonqry= Yojson.Basic.from_string jsonStr in
 let lst= to_lst jsonqry                       in
     List.map json_to_qry lst


(*
 converts a single query like q6 to json string
 This function is used to place queries with a single query
 parameter: Uri.add_query_param base_uri (qry, [q6])
 ex.        Uri.add_query_param base_uri (qry, [Jsonhelper.qry_to_jsonStr qryPar])
 *)
let qry_to_jsonStr qry= Yojson.Basic.to_string (qry_to_json qry)


(*
 converts a json string receiver over http into a query like q6
 This function is used to execute a function against Moana that
 take a single query parameter: Rtests.remote_test4 q6
 ex. let qry= Jsonhelper.jsonStr_to_qry jsonStr in
              Rtests.remote_test4 qry) 
 *)
let jsonStr_to_qry jsonStr= 
    let open Yojson.Basic.Util   in    
    json_to_qry(Yojson.Basic.from_string jsonStr)


(*
 Extracts query and tuple lists from tuples composed
 by the server
 *)
 let fst_of_four (fstEle, _, _, _)= fstEle
 let snd_of_four (_, sndEle, _, _)= sndEle
 let thi_of_four (_, _, thiEle, _)= thiEle
 let fou_of_four (_, _, _, fouEle)= fouEle

(*
 Extracts query, policy and tuple lists from tuples composed
 by the server
 *)
 let fst_of_three (fstEle, _, _)= fstEle
 let snd_of_three (_, sndEle, _)= sndEle
 let thi_of_three (_, _, thiEle)= thiEle

(*
 Extracts queries and tuple list from tuples composed
 by the server
 *)
 let fst_qr (fstEle, _, _, _, _)= fstEle
 let snd_qr (_, sndEle, _, _, _)= sndEle
 let thi_qr (_, _, thiEle, _, _)= thiEle
 let fou_qr (_, _, _, fouEle, _)= fouEle
 let fif_tu (_, _, _, _, fifEle)= fifEle


let contactData_to_json name_data=
  let name= fst(name_data) in
  let data= snd(name_data) in
  (name, `String data)

(*
  converts (jon, `String Crowcroft) into (jon, Crowcroft)
 *)

let jsonTocontactData name_data=
  let name= fst(name_data) in
  let data= match snd(name_data) with
       | `String d -> d
       |  _        -> raise (WrongJsonNameDataTuple ("Problems with Name data")) in
  (name, data)

let lst_to_jsonStr contactLst=
  let lst= List.map (fun name_data -> contactData_to_json name_data) contactLst in
  Yojson.Basic.to_string (`Assoc lst)

let jsonStr_to_lst jsonStr =
  let jsonLst= Yojson.Basic.from_string jsonStr in
  let lstAssoc= match jsonLst with
      | `Assoc lst -> lst
      | _          -> raise (WrongJsonLst("wrong Assoc json list\n")) in
   List.map (fun name_data -> jsonTocontactData name_data) lstAssoc

let tupLstToLst tupLst= List.map (fun name_data -> snd name_data) tupLst


(* 
  For resons I dont understand this function produces
  errors when called from within jsonStr_to_strLst but
  works fine when called from jsonLstOfString.
  Carlos Molina 27 Feb 2015
  let jsonStr_to_str (`String s)= s
 *)

let str_to_jsonStr s= `String s

let jsonStr_to_str jstr= match jstr with
    | `String s -> s
    | _         -> raise (WrongJsonString("jsonStr_to_strLst: wrong json string\n"))

(*
 Converts a list of string like ["simon"; "marco"; "defino"]
 to a json string that can be sent over http as a string.
 The original list can be recovered by jsonStr_to_strLst
 *)
let strLst_to_jsonStr strLst = 
  let lst= List.map str_to_jsonStr strLst    in
  Yojson.Basic.to_string (to_jnLst lst)


(*
 Recovers a list of string like ["simon"; "marco"; "defino"]
 from a json string produced by strLst_to_jsonStr.
 *)
let jsonStr_to_strLst jsonStr =
  let jsonLst= Yojson.Basic.from_string jsonStr in
  let lst= to_lst jsonLst               in
  List.map jsonStr_to_str lst 
  

(*
  prints a list of strings
 *)
let rec prtStrLst = function 
    []       -> ()
    | e::lst -> print_string (" "^e) ; prtStrLst lst





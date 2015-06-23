(*
* Copyright (c) 2014 Yan Shvartzshnaider
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
(*** This is an implementation of a sample application - contacts

The main functionality:

* Add contact info
* Query info on the contact
***)

open Config
  

  
(*let Contactsdata.yancontacts = Helper.tuples_from_file "contacts.db"*)
(* sample query *)
let q2 =
  "MAP {
	a,knows,?y,contacts
	?y,fn,?name,contacts
	?y,email,?email,contacts
	}"
  
(* execute policy to bring all the tuples that b can view *)
let q1 =
  "MAP {
	 b, canView,?x, policies
	 ?x, knows, ?o, contacts
	 ?o, email, ?email, contacts	 
	 ?o, fn, ?n, contacts
	}"
  

(* let prnt = Helper.print_tuples_list Contactsdata.yancontacts *)
 
let results = (Rete.exec_qry q1 (List.flatten Contactsdata.yancontacts)) |> (Rete.exec_bm q2) 

(**
 @author: Carlos Molina
 @date:   5 Jan 2015, Computer Laboratory, Univ. of Cambridge
 returns ...
 pquery: policy query
 qry:    query
 *)
let rresults policyQry qry= (Rete.exec_qry policyQry (List.flatten Contactsdata.yancontacts)) |> (Rete.exec_bm qry)

  
let (Rete.Node (_, res_bm, _)) = results
  
(*let p2 =
  Helper.print_tuples (Helper.TupleSet.elements (Rete.get_tuples results))*)
(*let p = Rete.print_bm res_bm*)

let r_map = Rete.get_res_map results [ "?name"; "?y"; "?email" ]

  
(*let _ = Helper.StringMap.iter Helper.print_var r_map *)



(**
 @author: Carlos Molina-Jimenez
 @date:   13 Jan 2015, Computer Laboratory, Univ. of Cambridge
 returns the content of the whole r_map presented as a
 single string.
 For ex. give a map, it produces a list like
 [("k1",[Constant "Ali"; Constant "Bob"]); (k2,[Constant "a"; Constant "b"; Constant "c"])],
 conversts it into a single string and prints it.
 *)
let getWhole_r_map() =
    print_string("\n\n The content of the whole map is: \n\n");
    "\ngetWhole_r_map\n"^ Contactshelper.mapLstToStr (Helper.StringMap.bindings r_map)

let getWhole policyQry qry= 
    let results= (Rete.exec_qry policyQry (List.flatten Contactsdata.yancontacts)) |> (Rete.exec_bm qry) in
    let r_map= Rete.get_res_map results [ "?name"; "?y"; "?email" ]                      in
    print_string("\n\n The result of getWhole map is: \n\n");
    "\n getWhole\n"^ Contactshelper.mapLstToStr (Helper.StringMap.bindings r_map)

let getWholeMap policyQry qry tupLst= 
  let contactsLst= List.map (fun str -> Helper.to_tuple_lst str) tupLst                   in
  let results= (Rete.exec_qry policyQry (List.flatten contactsLst)) |> (Rete.exec_bm qry) in
  let r_map= Rete.get_res_map results [ "?name"; "?y"; "?email" ]                         in
  print_string("\n\n The result of getWhole map is: \n\n");
    "\n getWholeMap::: with varsLst by default: \n"^ Contactshelper.mapLstToStr (Helper.StringMap.bindings r_map)


let get_whole_map policyQry qry tupLst varsLst= 
  let contactsLst= List.map (fun str -> Helper.to_tuple_lst str) tupLst                   in
  let results= (Rete.exec_qry policyQry (List.flatten contactsLst)) |> (Rete.exec_bm qry) in
  let r_map= Rete.get_res_map results varsLst (* [ "?name"; "?y"; "?email" ] *)           in
  print_string("\n\n The result of getWhole map is: \n\n");
  Contactshelper.mapLstToStr (Helper.StringMap.bindings r_map)

(**
 @author: Carlos Molina-Jimenez
 @date:   9 Jun 2015, Computer Laboratory, Univ. of Cambridge
 returns a string in the following format:
 <ul class="vcard">
  <li class="y">cm770@cam.ac.uk< </li>
  <li class="y">carlos.molina@ncl.ac.uk< </li>
 </ul>
 *)
let get_contactDataByName policyQry qry tupLst varsLst= 
  let contactsLst= List.map (fun str -> Helper.to_tuple_lst str) tupLst                   in
  let results= (Rete.exec_qry policyQry (List.flatten contactsLst)) |> (Rete.exec_bm qry) in
  let r_map= Rete.get_res_map results varsLst (* [ "?name"; "?y"; "?email" ] *)           in
  print_string("\n\n The result of getWhole map is: \n\n");
  Htmlhelper.mapLstToHtml_ul_Str (Helper.StringMap.bindings r_map)



(**
 @author: Carlos Molina-Jimenez
 @date:   9 Jun 2015, Computer Laboratory, Univ. of Cambridge
 Retrieves the contact data related to a given firstName. If 
 firstName="" return the contact data of all contacts. The
 string returned is either
 <ul class="vcard">
  <li class="y">cm770@cam.ac.uk< </li>
  <li class="y">carlos.molina@ncl.ac.uk< </li>
 </ul>
  or 
  <ul class="vcard">
  <li class="fn">Carlos< </li>
  <li class="last">Molina-jimenez< </li>
  <li class="image">carlos.jpg< </li>
  <li class="email">cm770@cam.ac.uk< </li>
  <li class="title">Postdoc< </li>
  <li class="twitter">@carlos< </li>
  <li class="knows">a< </li>
  <li class="knows">c< </li>
</ul>
  for each contact.
 *)

let contacts= List.map (fun str -> Helper.to_tuple_lst str) Contactsdata.contacts 

let getContactDataByFirstName firstName= 
  match firstName with
  | ""  -> Htmlhelper.listoftuples_to_html_str contacts 
  | _   ->
    let qry = Contactshelper.makeMapQry [Contactshelper.make_fn_last_email_qry "fn" firstName;
               Contactsdata.qry_fn;     Contactsdata.qry_last;
               Contactsdata.qry_email;  Contactsdata.qry_title;
               Contactsdata.qry_mobile] in
    let results= (Rete.exec_qry Contactsdata.defaultPlcy (List.flatten contacts)) 
                 |> (Rete.exec_bm qry) in
    let r_map= Rete.get_res_map results Contactsdata.varsLstDefault in
    Htmlhelper.mapLstToHtml_ul_Str (Helper.StringMap.bindings r_map)

(**
 @author: Carlos Molina-Jimenez
 @date:   9 Jun 2015, Computer Laboratory, Univ. of Cambridge
 Retrieves the contact data related to a given lastName. If 
 lastName="" return the contact data of all contacts. 
 *)
let getContactDataByLastName lastName= 
  match lastName with
  | ""  -> Htmlhelper.listoftuples_to_html_str contacts 
  | _   ->
    let qry = Contactshelper.makeMapQry [Contactshelper.make_fn_last_email_qry "last" lastName;
               Contactsdata.qry_fn; Contactsdata.qry_last; Contactsdata.qry_email] in 
    let results= (Rete.exec_qry Contactsdata.defaultPlcy (List.flatten Contactsdata.yancontacts)) 
                 |> (Rete.exec_bm qry) in
    let r_map= Rete.get_res_map results Contactsdata.varsLstDefault in
    Htmlhelper.mapLstToHtml_ul_Str (Helper.StringMap.bindings r_map)


let getContactDataByEmail email= 
  match email with
  | ""  -> Htmlhelper.listoftuples_to_html_str contacts 
  | _   ->
    let qry = Contactshelper.makeMapQry [Contactshelper.make_fn_last_email_qry "email" email;
               Contactsdata.qry_fn; Contactsdata.qry_last; Contactsdata.qry_email] in 
    let results= (Rete.exec_qry Contactsdata.defaultPlcy (List.flatten contacts)) 
                 |> (Rete.exec_bm qry) in
    let r_map= Rete.get_res_map results Contactsdata.varsLstDefault in
    Htmlhelper.mapLstToHtml_ul_Str (Helper.StringMap.bindings r_map)


(**
 returns a string: the content of the contact repository.
 @author: Carlos Molina-Jimenez
 @date:   5 Jan 2015, Computer Laboratory, Univ. of Cambridge
 *)
let getStrOfTuples() = Contactshelper.listoftuples_to_str Contactsdata.yancontacts


(**
 returns a string: the content of the contact repository.
 tupLst is [("jon","jondata"), ("carlos","carlosdata");...] sent
 from a client to the server.
 This function and getStrOfTuples produce the same results: string
 that differ on white spaces only.
 @author: Carlos Molina-Jimenez
 @date:   26 Jan 2015, Computer Laboratory, Univ. of Cambridge
 *)
let getStringOfTuples tupLst= 
  let contactsLst= List.map (fun str -> Helper.to_tuple_lst str) tupLst in
  Contactshelper.listoftuples_to_str contactsLst

(**
 returns a string: the content of the contact repository.
 tupLst is [("jon","jondata"), ("carlos","carlosdata");...] sent
 from a client to the server.
 This function and getStrOfTuples produce the same results: string
 that differ on white spaces only.
 @author: Carlos Molina-Jimenez
 @date:   26 May 2015, Computer Laboratory, Univ. of Cambridge
 *)
let getHtmlStringOfTuples tupLst= 
  let contactsLst= List.map (fun str -> Helper.to_tuple_lst str) tupLst in
  Htmlhelper.listoftuples_to_html_str contactsLst

(**
 returns a string: the content of the contact repository.
 tupLst is [("jon","jondata"), ("carlos","carlosdata");...] sent
 from a client to the server.
 This function and getStrOfTuples produce the same results: string
 that differ on white spaces only.
 @author: Carlos Molina-Jimenez
 @date:   8 Jun 2015, Computer Laboratory, Univ. of Cambridge
 *)
(*
  let getAllContacts_inHtml= 
  let tupLst= [Contactsdata.jondata; Contactsdata.amirdata); ("anil", Contactsdata.anildata);
               ("carlos", Contactsdata.carlosdata); ("richard", Contactsdata.richarddata)] in
  let contactsLst= List.map (fun str -> Helper.to_tuple_lst str) tupLst in
  Htmlhelper.listoftuples_to_html_str contactsLst
*)



(*
 Used for testing only
 *)
let bounceStr (s:string):string= s


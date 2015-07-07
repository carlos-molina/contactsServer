(*
* Copyright (c) 2015 Carlos Molina-Jimenez 
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

exception WrongRequestorName of  string   

  
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
  

let contacts= List.map (fun str -> Helper.to_tuple_lst str) Contactsdata.contacts 



(**
 @date: 9 Jun 2015, Computer Laboratory, Univ. of Cambridge
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
 @date:   9 Jun 2015, Computer Laboratory, Univ. of Cambridge
 Retrieves the contact data related to a given lastName. If 
 lastName="" return the contact data of all contacts. 
 *)
let getContactDataByLastName lastName= 
  match lastName with
  | ""  -> Htmlhelper.listoftuples_to_html_str contacts 
  | _   ->
    let qry = Contactshelper.makeMapQry [Contactshelper.make_fn_last_email_qry "last" lastName;
               Contactsdata.qry_fn;     Contactsdata.qry_last;
               Contactsdata.qry_email;  Contactsdata.qry_title;
               Contactsdata.qry_mobile] in
    let results= (Rete.exec_qry Contactsdata.defaultPlcy (List.flatten Contactsdata.yancontacts)) 
                 |> (Rete.exec_bm qry) in
    let r_map= Rete.get_res_map results Contactsdata.varsLstDefault in
    Htmlhelper.mapLstToHtml_ul_Str (Helper.StringMap.bindings r_map)


let getContactDataByFirstNamePlcy requestorName firstName= 
  match firstName with
  | ""  -> Htmlhelper.listoftuples_to_html_str contacts 
  | _   ->
    let qry = Contactshelper.makeMapQry [Contactshelper.make_fn_last_email_qry "fn" firstName;
               Contactsdata.qry_fn;     Contactsdata.qry_last;
               Contactsdata.qry_email;  Contactsdata.qry_title;
               Contactsdata.qry_mobile] in
    let applyPolicy= match requestorName with
     | "Jon" -> Contactsdata.jonPlcy   
     | "Amir" | "Anil" | "Carlos" | "Richard" -> Contactsdata.defaultPlcy 
     | _     ->  raise (WrongRequestorName ("getContactDataByFirstNamePlcy failed: wrong requestor name"))
     in
    let results= (Rete.exec_qry applyPolicy (List.flatten contacts)) 
                 |> (Rete.exec_bm qry) in
    let r_map= Rete.get_res_map results Contactsdata.varsLstDefault in
    Htmlhelper.mapLstToHtml_ul_Str (Helper.StringMap.bindings r_map)


let getContactDataByEmail email= 
  match email with
  | ""  -> Htmlhelper.listoftuples_to_html_str contacts 
  | _   ->
    let qry = Contactshelper.makeMapQry [Contactshelper.make_fn_last_email_qry "email" email;
               Contactsdata.qry_fn;     Contactsdata.qry_last;
               Contactsdata.qry_email;  Contactsdata.qry_title;
               Contactsdata.qry_mobile] in
    let results= (Rete.exec_qry Contactsdata.defaultPlcy (List.flatten contacts)) 
                 |> (Rete.exec_bm qry) in
    let r_map= Rete.get_res_map results Contactsdata.varsLstDefault in
    Htmlhelper.mapLstToHtml_ul_Str (Helper.StringMap.bindings r_map)



(*
 Used for testing only
 *)
let bounceStr (s:string):string= s


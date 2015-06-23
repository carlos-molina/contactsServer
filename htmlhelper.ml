(*
* Copyright (c) 2015 Carlos Molina Jimenez
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
 @date:   26 May 2015, Computer Laboratory, Univ. of Cambridge
 Converts a single tuple like < c fn Anil> < c last Madhavapeddy> ...
 < c title Lecturer > into a html string and returns the resulting string.
 *)
let tupleToHtmlStr tuple=
  let rec help str tuple= match tuple with
  | []      -> str^"</ul>\n\n"
  | h::rest -> help (str^(Helper.to_htmlstring h)^"\n") rest 
  in help "<ul class=\"vcard\">\n" tuple


(**
 @author: Carlos Molina 
 @date:   26 May 2015, Computer Laboratory, Univ. of Cambridge
 Returns a string: the result of converting a list of tuples 
 into a html string
 *)
 let listoftuples_to_html_str tuples= 
    let lst= List.map (fun t -> tupleToHtmlStr t) tuples in
    Contactshelper.listofStrToStr lst


(**
 @author: Carlos Molina 
 @date:   5 Jun 2015, Computer Laboratory, Univ. of Cambridge
 returns a string: the result of converting
 Constant str into str in html language
 *)
let constStrToStr_in_html constStr= match constStr with
    | Constant value -> "\n"^value^"</li>"
    | _              -> raise Wrong_value


(**
 @author: Carlos Molina 
 @date:   5 Jun 2015, Computer Laboratory, Univ. of Cambridge
 Given var= "?y" and lst= ["cm770@cam.ac.uk", "carlos.molina@ncl.ac.uk"]
 returns a string.
 *)
let make_html_ul var lst=
    let leftstr= "  <li class=\""^Str.string_after var 1^"\">" in (* exclude ? from 1st pos of var *)
    let rightstr= "< </li>\n"                    in
    let resLst= List.map Contactshelper.constStrToStr lst       in
    let lst_of_html= List.map (fun str -> leftstr ^ String.trim(str) ^ rightstr) resLst in
    Contactshelper.listofStrToStr lst_of_html   
     

 
(**
 @author: Carlos Molina 
 @date:   5 Jun 2015, Computer Laboratory, Univ. of Cambridge
 Given [("?y", ["cm770@cam.ac.uk", "carlos.molina@ncl.ac.uk")]
 returns a string in the format shown in function make_html_ul
 *)
let mapLstTo_html_ul tupleLst=
    let var= fst(tupleLst) in
    let lst= snd(tupleLst) in
    make_html_ul var lst



(**
 @author: Carlos Molina 
 @date:   5 Jun 2015, Computer Laboratory, Univ. of Cambridge
 Given [("?y", ["cm770@cam.ac.uk", "car@ncl.ac.uk"); ("?fn", ["carlos"])]
 returns an html ul list
 <ul class="vcard">
  <li class="y">cm770@cam.ac.uk< </li>
  <li class="y">carlos.molina@ncl.ac.uk< </li>
 </ul> 
 *)
let mapLstToHtml_ul_Str mapLst= "<ul class=\"vcard\">\n" ^ 
                                 Contactshelper.listofStrToStr (List.map mapLstTo_html_ul mapLst) ^ 
                                "</ul>"




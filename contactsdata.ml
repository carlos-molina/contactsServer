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

(*
 Contacts data used by Yan Shvartzshnaider in his
 example
 *)
let jon =
  Helper.to_tuple_lst
    "{(a,fn,Jon,contacts)
          (a,last,Crowcroft,contacts)    
   (a,email,jon.crowcroft@cl.cam.ac.uk,contacts)
         (a,twitter,@tforcworc,contacts)
         (a,mobile, 617-000-0001,contacts)
   (a,title,Professor,contacts) 
         (a,image,jon.jpg, contacts)
         (a,knows,c,contacts)
         (a,knows,d,contacts)
         (a,knows,e,contacts)
        }"
  
let amir =
  Helper.to_tuple_lst
    "{(b,fn,Amir, contacts)
                (b,last,Chaudhry,contacts)    
   (b,email,amir.chaudhry@cl.cam.ac.uk,contacts)
         (b,twitter,@amirmc,contacts)
         (b,image,amir.jpg, contacts)
   (b,title,Postdoc,contacts)
   (b,knows,a,contacts)
   (b,knows,c,contacts)
         (b,knows,d,contacts)
        }"
  
let anil =
  Helper.to_tuple_lst
    "{(c,fn,Anil, contacts)
          (c,last,Madhavapeddy, contacts)
   (c,email,anil@recoil.org,contacts)    
         (c,image,anil.jpg, contacts)
         (c,twitter,@avsm,contacts) 
         (c,email,anil.madhavapeddy@recoil.org,contacts)
   (c,title,Lecturer,contacts)}"

let carlos =
  Helper.to_tuple_lst
    "{(d,fn,Carlos, contacts)
         (d,last,Molina-jimenez, contacts)
         (d,image,carlos.jpg, contacts)    
   (d,email,cm770@cam.ac.uk,contacts)        
   (d,title,Postdoc,contacts)
         (d,twitter,@carlos,contacts) 
         (d,knows,a,contacts)
         (d,knows,c,contacts)}"
  
let richard =
  Helper.to_tuple_lst
    "{(e,fn,Richard,contacts)    
                (e,last,Mortier,contacts)
   (e,email,richard.mortier@nottingham.ac.uk,contacts)
         (e,image,mort.png, contacts)
         (e,twitter,@mort__,contacts)        
   (e,title,Lecturer,contacts)
         (e,knows,c,contacts)
         (e,knows,d,contacts)
        }"
  
let policies =
  Helper.to_tuple_lst "{
        (b,canView,a,policies)
        (c,canView,d,policies)  
        }"


let yancontacts = [ jon; amir; anil; carlos; richard ]
  


(*
 Tuples used by Carlos Molina to send and receive
 in Json format
 *)
let t1 =
  {
    subj = Constant "a";
    pred = Constant "type";
    obj = Constant "Car";
    ctxt = Constant "context";
    time_stp = None;
    sign = None;
  }

let t2 =
  {
    subj = Constant "a";
    pred = Constant "hasColor";
    obj = Constant "c";
    ctxt = Constant "context";
    time_stp = None;
    sign = None;
  }

let t3 =
  {
    subj = Constant "b";
    pred = Constant "type";
    obj = Constant "Chair";
    ctxt = Constant "context";
    time_stp = None;
    sign = None;
  }

let t4 =
  {
    subj = Constant "b";
    pred = Constant "hasColor";
    obj = Constant "c";
    ctxt = Constant "context";
    time_stp = None;
    sign = None;
  }

let t5 =
  {
    subj = Constant "c";
    pred = Constant "type";
    obj = Constant "Color";
    ctxt = Constant "context";
    time_stp = None;
    sign = None;
  }

let t6 =
  {
    subj = Constant "c1";
    pred = Constant "type";
    obj = Constant "Color";
    ctxt = Constant "context";
    time_stp = None;
    sign = None;
  }

let t7 =
  {
    subj = Constant "c";
    pred = Constant "rgbValue";
    obj = Constant "White";
    ctxt = Constant "context";
    time_stp = None;
    sign = None;
  }

let t8 =
  {
    subj = Constant "c1";
    pred = Constant "rgbValue";
    obj = Constant "Red";
    ctxt = Constant "context";
    time_stp = None;
    sign = None;
  }

let t9 =
  {
    subj = Constant "a";
    pred = Constant "hasColor";
    obj = Constant "Red";
    ctxt = Constant "context";
    time_stp = None;
    sign = None;
  }

let t10 =
  {
    subj = Constant "d";
    pred = Constant "relatesTo";
    obj = Constant "c";
    ctxt = Constant "context";
    time_stp = None;
    sign = None;
  }

let t11 =
  {
    subj = Constant "c";
    pred = Constant "relatesTo";
    obj = Constant "d";
    ctxt = Constant "context";
    time_stp = None;
    sign = None;
  }

let t12 =
  {
    subj = Constant "d";
    pred = Constant "type";
    obj = Constant "Car";
    ctxt = Constant "context";
    time_stp = None;
    sign = None;
  }

let t13 =
  {
    subj = Constant "d";
    pred = Constant "hasColor";
    obj = Constant "c2";
    ctxt = Constant "context";
    time_stp = None;
    sign = None;
  }


let t14 =
  {
    subj = Constant "c2";
    pred = Constant "type";
    obj = Constant "Color";
    ctxt = Constant "context";
    time_stp = None;
    sign = None;
  }

let t15 =
  {
    subj = Constant "c2";
    pred = Constant "rgbValue";
    obj = Constant "White";
    ctxt = Constant "context";
    time_stp = None;
    sign = None;
  }

let q_get_all =
{
    subj = Variable "?s";
    pred = Variable "?p";
    obj  = Variable "?o";
    ctxt = Constant "context";
    time_stp = None;
    sign = None;
  }

let q_get_all_type =
{
    subj = Variable "?s";
    pred = Constant "type";
    obj  = Variable "?o";
    ctxt = Constant "context";
    time_stp = None;
    sign = None;
  }

let q_get_all_hasColor =
{
    subj = Variable "?s";
    pred = Constant "hasColor";
    obj  = Variable "?o";
    ctxt = Constant "context";
    time_stp = None;
    sign = None;
  }

let q_get_all_rgbValue =
{
    subj = Variable "?s";
    pred = Constant "rgbValue";
    obj  = Variable "?o";
    ctxt = Constant "context";
    time_stp = None;
    sign = None;
  }

let q_get_all_relatesTo=
{
    subj = Variable "?s";
    pred = Constant "relatesTo";
    obj  = Variable "?o";
    ctxt = Constant "context";
    time_stp = None;
    sign = None;
  }

let q1 =
{
    subj = Variable "?x";
    pred = Constant "type";
    obj  = Constant "Car";
    ctxt = Constant "context";
    time_stp = None;
    sign = None;
  }

let q2 =
  {
    subj = Variable "?x";
    pred = Constant "hasColor";
    obj  = Constant "Red";
    ctxt = Constant "context";
    time_stp = None;
    sign = None;
  }

let q3 =
  {
    subj = Variable "?x";
    pred = Constant "hasColor";
    obj = Variable "?y";
    ctxt = Constant "context";
    time_stp = None;
    sign = None;
  }

let q4 =
  {
    subj = Variable "?y";
    pred = Constant "type";
    obj = Constant "Color";
    ctxt = Constant "context";
    time_stp = None;
    sign = None;
  }


let q5 =
  {
    subj = Variable "?y";
    pred = Constant "rgbValue";
    obj = Constant "White";
    ctxt = Constant "context";
    time_stp = None;
    sign = None;
  }

let q6 =
  {
    subj = Variable "?y";
    pred = Constant "relatesTo";
    obj  = Variable "?x";
    ctxt = Constant "context";
    time_stp = None;
    sign = None;
  }

let q7 =
  {
    subj = Variable "?x";
    pred = Constant "relatesTo";
    obj  = Variable "?y";
    ctxt = Constant "context";
    time_stp = None;
    sign = None;
  }


  


(* 
 Contacts data used by carlos Molina to send ansd receive
 in Json format.
 It is free from tab characters
 *)
(*
  data about Jon
 *)
let tup_jo1= 
 {
  subj=  Constant "a";
  pred=  Constant "fn";
  obj=   Constant "Jon";
  ctxt=  Constant "contacts";
  time_stp= None;
  sign= None;
 }

let tup_jo2= 
 {
  subj=  Constant "a";
  pred=  Constant "last";
  obj=   Constant "Crowcroft";
  ctxt=  Constant "contacts";
  time_stp= None;
  sign= None;
 }

let tup_jo3=
 {
 subj= Constant "a";
 pred= Constant "email";
 obj=  Constant "jon.crowcroft@cl.cam.ac.uk";
 ctxt= Constant "contacts";
 time_stp= None;
 sign= None;
 }


let tup_jo4=
 {
  subj= Constant "a"; 
  pred= Constant "twitter";
  obj=  Constant "@tforcworc";
  ctxt= Constant "contacts";
  time_stp= None;
  sign= None;
 }


let tup_jo5=
 {
  subj= Constant "a";
  pred= Constant  "mobile"; 
  obj=  Constant  "617-000-0001";
  ctxt= Constant  "contacts";
  time_stp= None;
  sign= None;
 }

let tup_jo6=
 {
  subj= Constant  "a";
  pred= Constant  "title";
  obj=  Constant  "Professor";
  ctxt= Constant  "contacts";
  time_stp= None;
  sign= None;
 }	

let tup_jo7=
 {
  subj= Constant "a";
  pred= Constant "image";
  obj=  Constant "jon.jpg"; 
  ctxt= Constant "contacts";
  time_stp= None;
  sign= None;
 }


let tup_jo8= 
 {
  subj= Constant  "a";
  pred= Constant  "knows";
  obj=  Constant  "c";
  ctxt= Constant  "contacts";
  time_stp= None;
  sign= None;
 }

let tup_jo9=
 {
  subj= Constant  "a";
  pred= Constant  "knows";
  obj=  Constant  "d";
  ctxt= Constant  "contacts";
  time_stp= None;
  sign= None;
 }

let tup_jo10=
 {
  subj= Constant "a";
  pred= Constant "knows";
  obj=  Constant "e";
  ctxt= Constant "contacts";
  time_stp= None;
  sign= None;
 }


(*
 data about Amir
 *)
let tup_am1=
 {
   subj= Constant "b";
   pred= Constant "fn";
   obj=  Constant "Amir"; 
   ctxt= Constant "contacts";
   time_stp= None;
   sign= None;
 }
		
let tup_am2=
 {
  subj= Constant  "b";
  pred= Constant  "last";
  obj=  Constant  "Chaudhry";
  ctxt= Constant  "contacts"; 
  time_stp= None;
  sign= None;
 }   

   
let tup_am3=
 {
  subj= Constant "b";
  pred= Constant "email";
  obj=  Constant "amir.chaudhry@cl.cam.ac.uk";
  ctxt= Constant "contacts";
  time_stp= None;
  sign= None;
 }

let tup_am4=
 {
  subj= Constant "b";
  pred= Constant "twitter";
  obj=  Constant "@amirmc";
  ctxt= Constant "contacts";
  time_stp= None;
  sign= None;
 }

	 
let tup_am5=
 {
  subj= Constant  "b";
  pred= Constant  "image";
  obj=  Constant  "amir.jpg"; 
  ctxt= Constant  "contacts";
  time_stp= None;
  sign= None;
 }

let tup_am6=
 {
  subj= Constant "b";
  pred= Constant "title";
  obj=  Constant "Postdoc";
  ctxt= Constant "contacts";
  time_stp= None;
  sign= None;
 }

let tup_am7=
 {   
  subj= Constant  "b";
  pred= Constant  "knows";
  obj=  Constant  "a";
  ctxt= Constant  "contacts";
  time_stp= None;
  sign= None;
 }

let tup_am8=
 {
  subj= Constant "b";
  pred= Constant "knows";
  obj=  Constant "c";
  ctxt= Constant "contacts";
  time_stp= None;
  sign= None;
 }

let tup_am9=
 {
  subj= Constant "b";
  pred= Constant "knows";
  obj=  Constant "d";
  ctxt= Constant "contacts";
  time_stp= None;
  sign= None;
 } 
 

(*
  data about Anil
 *)
let tup_an1=
 { 
  subj= Constant "c";
  pred= Constant "fn";
  obj=  Constant "Anil"; 
  ctxt= Constant "contacts";
  time_stp= None;
  sign= None;
 }

let tup_an2=
 {	  
  subj= Constant  "c";
  pred= Constant  "last";
  obj=  Constant  "Madhavapeddy"; 
  ctxt= Constant  "contacts";
  time_stp= None;
  sign= None;
 }

let tup_an3=
 {
  subj= Constant  "c";
  pred= Constant  "email";
  obj=  Constant  "anil@recoil.org";
  ctxt= Constant  "contacts"; 
  time_stp= None;
  sign= None;
 }

let tup_an4=
 {   
  subj= Constant  "c";
  pred= Constant  "image";
  obj=  Constant  "anil.jpg"; 
  ctxt= Constant  "contacts";
  time_stp= None;
  sign= None;
 }

let tup_an5=
 {
  subj= Constant  "c";
  pred= Constant  "twitter";
  obj=  Constant  "@avsm";
  ctxt= Constant  "contacts"; 
  time_stp= None;
  sign= None;
 }

let tup_an6=
 {	
  subj= Constant  "c";
  pred= Constant  "email";
  obj=  Constant  "anil.madhavapeddy@recoil.org";
  ctxt= Constant  "contacts";
  time_stp= None;
  sign= None;
 }

let tup_an7=
 {
  subj= Constant  "c";
  pred= Constant  "title";
  obj=  Constant  "Lecturer";
  ctxt= Constant  "contacts";
  time_stp= None;
  sign= None;
 }


(*
  data about carlos
 *)
 
let tup_ca1=
 { 
  subj= Constant "d";
  pred= Constant "fn";
  obj=  Constant "Carlos"; 
  ctxt= Constant "contacts";
  time_stp= None;
  sign= None;
 }

let tup_ca2=
 {
  subj= Constant  "d";
  pred= Constant  "last";
  obj=  Constant  "Molina-jimenez"; 
  ctxt= Constant  "contacts";
  time_stp= None;
  sign= None;
 }

let tup_ca3=
 {
  subj= Constant  "d";
  pred= Constant  "image";
  obj=  Constant  "carlos.jpg"; 
  ctxt= Constant  "contacts"; 
  time_stp= None;
  sign= None;
 }   
  

let tup_ca4=
 {
  subj= Constant  "d";
  pred= Constant  "email";
  obj=  Constant  "cm770@cam.ac.uk";
  ctxt= Constant  "contacts";
  time_stp= None;
  sign= None;
 }

let tup_ca5=
 {        
  subj= Constant  "d";
  pred= Constant  "title";
  obj=  Constant  "Postdoc";
  ctxt= Constant  "contacts";
  time_stp= None;
  sign= None;
 }

let tup_ca6=
 {
  subj= Constant  "d";
  pred= Constant  "twitter";
  obj=  Constant  "@carlos";
  ctxt= Constant  "contacts"; 
  time_stp= None;
  sign= None;
 }

let tup_ca7=
 {
  subj= Constant  "d";
  pred= Constant  "knows";
  obj=  Constant  "a";
  ctxt= Constant  "contacts";
  time_stp= None;
  sign= None;
 }

let tup_ca8=
 {
  subj= Constant  "d";
  pred= Constant  "knows";
  obj=  Constant  "c";
  ctxt= Constant  "contacts";
  time_stp= None;
  sign= None;
 }

  
let tup_ri1=
 {
  subj= Constant "e";
  pred= Constant "fn";
  obj=  Constant "Richard";
  ctxt= Constant "contacts";
  time_stp= None;
  sign= None;
 }
    
let tup_ri2=
 {
  subj= Constant  "e";
  pred= Constant  "last";
  obj=  Constant  "Mortier";
  ctxt= Constant  "contacts";
  time_stp= None;
  sign= None;
 }

let tup_ri3=
 {
  subj= Constant  "e";
  pred= Constant  "email";
  obj=  Constant  "richard.mortier@nottingham.ac.uk";
  ctxt= Constant  "contacts";
  time_stp= None;
  sign= None;
 }

let tup_ri4=
 {
  subj= Constant "e";
  pred= Constant "image";
  obj=  Constant "mort.png"; 
  ctxt= Constant "contacts";
  time_stp= None;
  sign= None;
 }

let tup_ri5=
 {
  subj= Constant "e";
  pred= Constant "twitter";
  obj=  Constant "@mort__";
  ctxt= Constant "contacts";
  time_stp= None;
  sign= None;
 }
        
let tup_ri6=
 {
  subj= Constant  "e";
  pred= Constant  "title";
  obj=  Constant  "Lecturer";
  ctxt= Constant  "contacts";
  time_stp= None;
  sign= None;
 }

let tup_ri7=
 {
  subj= Constant  "e";
  pred= Constant  "knows";
  obj=  Constant  "c";
  ctxt= Constant  "contacts";
  time_stp= None;
  sign= None;
 }

let tup_ri8=
 {
  subj= Constant  "e";
  pred= Constant  "knows";
  obj=  Constant  "d";
  ctxt= Constant  "contacts";
  time_stp= None;
  sign= None;
 }


(*
  Jon's data ready for Json manipulation
 *)
let jondata= Contactshelper.makeStrOfContactData [tup_jo1; tup_jo2;
                                                  tup_jo3; tup_jo4;
                                                  tup_jo5; tup_jo6;
                                                  tup_jo7; tup_jo8;
                                                  tup_jo9; tup_jo10]
(*
  Amir's data ready for Json manipulation
 *)
let amirdata= Contactshelper.makeStrOfContactData [tup_am1; tup_am2;
                                                   tup_am3; tup_am4;
                                                   tup_am5; tup_am6;
                                                   tup_am7; tup_am8;
                                                   tup_am9]
(*
  Anil's data ready for Json manipulation
 *)
let anildata= Contactshelper.makeStrOfContactData [tup_an1; tup_an2;
                                                   tup_an3; tup_an4;
                                                   tup_an5; tup_an6;
                                                   tup_an7]
(*
  Carlos' data ready for Json manipulation
 *)
let carlosdata= Contactshelper.makeStrOfContactData [tup_ca1; tup_ca2;
                                                     tup_ca3; tup_ca4;
                                                     tup_ca5; tup_ca6;
                                                     tup_ca7; tup_ca8]
(*
  Richard's data ready for Json manipulation
 *)
let richarddata= Contactshelper.makeStrOfContactData [tup_ri1; tup_ri2;
                                                      tup_ri3; tup_ri4;
                                                      tup_ri5; tup_ri6;
                                                      tup_ri7; tup_ri8]



(*
 query qry 
 let q2 =
  "MAP {
        a,knows,?y,contacts
        ?y,fn,?name,contacts
        ?y,email,?email,contacts
        }"
*)

let qt1 =
 {
  subj= Variable "a";
  pred= Constant "knows";
  obj=  Constant "?y";
  ctxt= Constant "contacts";
  time_stp= None;
  sign= None;
 }

let qt1jon =
 {
  subj= Variable "a";
  pred= Constant "knows";
  obj=  Constant "?y";
  ctxt= Constant "contacts";
  time_stp= None;
  sign= None;
 }

let qt1amir =
 {
  subj= Variable "b";
  pred= Constant "knows";
  obj=  Constant "?y";
  ctxt= Constant "contacts";
  time_stp= None;
  sign= None;
 }

let qt1carlosemail =
 {
  subj= Variable "d";
  pred= Constant "email";
  obj=  Constant "?y";
  ctxt= Constant "contacts";
  time_stp= None;
  sign= None;
 }

let qt1richardemail =
 {
  subj= Variable "e";
  pred= Constant "email";
  obj=  Constant "?y";
  ctxt= Constant "contacts";
  time_stp= None;
  sign= None;
 }

let qt1carloslast =
 {
  subj= Variable "d";
  pred= Constant "last";
  obj=  Constant "?y";
  ctxt= Constant "contacts";
  time_stp= None;
  sign= None;
 }

let qt1anillast =
 {
  subj= Variable "c";
  pred= Constant "last";
  obj=  Constant "?y";
  ctxt= Constant "contacts";
  time_stp= None;
  sign= None;
 }

let qt1richardlast =
 {
  subj= Variable "e";
  pred= Constant "last";
  obj=  Constant "?y";
  ctxt= Constant "contacts";
  time_stp= None;
  sign= None;
 }

let qt2 =
 {
  subj= Variable "?y";
  pred= Constant "fn";
  obj=  Constant "?name";
  ctxt= Constant "contacts";
  time_stp= None;
  sign= None;
 }

let qt3 =
 {
  subj= Variable "?y";
  pred= Constant "email";
  obj=  Constant "?email";
  ctxt= Constant "contacts";
  time_stp= None;
  sign= None;
 }


(*
  policy: execute policy to bring all the tuples that b can view. 
let q1 =
  "MAP {
         b, canView,?x, policies
         ?x, knows, ?o, contacts
         ?o, email, ?email, contacts     
         ?o, fn, ?n, contacts
        }"
 *)

let plcy_tu1 =
 {
  subj= Variable "b";
  pred= Constant "canView";
  obj=  Constant "?x";
  ctxt= Constant "policies";
  time_stp= None;
  sign= None;
 }
let plcy_tu2 =
 {
  subj= Variable "?x";
  pred= Constant "knows";
  obj=  Constant "?o";
  ctxt= Constant "contacts";
  time_stp= None;
  sign= None;
 }


let plcy_tu3 =
 {
  subj= Variable "?o";
  pred= Constant "email";
  obj=  Constant "?email";
  ctxt= Constant "contacts";
  time_stp= None;
  sign= None;
 }

let plcy_tu4 =
 {
  subj= Variable "?o";
  pred= Constant "fn";
  obj=  Constant "?n";
  ctxt= Constant "contacts";
  time_stp= None;
  sign= None;
 }

let plcy_tu5_last =
 {
  subj= Variable "?o";
  pred= Constant "last";
  obj=  Constant "?last";
  ctxt= Constant "contacts";
  time_stp= None;
  sign= None;
 }



(*
  default policy: the user can view everything.
  I used this policy to get contacts data of a contact
  given a first name.
*)

let defaultPlcy =
  "MAP {
         ?xbody,  canView,         ?ybody,      policies
         ?ybody,  ?anypred,        ?anyobj,     contacts
        }"


let _jonPlcy =
  "MAP {
         a,       canView,   ?ybody,      policies
         ?ybody,  knows,     ?Jon,        contacts 
         ?ybody,  email,     ?email,      contacts
        }"

let jonPlcy__ =
  "MAP {
         b, canView,?x, policies
         ?x, knows, ?o, contacts
         ?o, email, ?email, contacts     
         ?o, fn, ?n, contacts
        }"

let jonPlcy =
  "MAP {
         a, canView,?x, policies
         ?x, knows, Jon, contacts
         ?o, email, ?email, contacts     
         ?o, fn, ?n, contacts
        }"
  


let defaultplcy_tu1 =
 {
  subj= Variable "?xbody";
  pred= Constant "canView";
  obj=  Constant "?ybody";
  ctxt= Constant "policies";
  time_stp= None;
  sign= None;
 }

let defaultplcy_tu2 =
 {
  subj= Variable "?ybody";
  pred= Constant "?anypred";
  obj=  Constant "?anyobj";
  ctxt= Constant "contacts";
  time_stp= None;
  sign= None;
}

(*
 tuples to make query to retrive contacts data by first name
 9 Jun 2015
 *)
let qry_ybody=
 {
  subj= Variable "?y";
  pred= Constant "fn";
  obj=  Constant "Jon";
  ctxt= Constant "contacts";
  time_stp= None;
  sign= None;
 }

let qry_fn =
 {
  subj= Variable "?y";
  pred= Constant "fn";
  obj=  Constant "?fn";
  ctxt= Constant "contacts";
  time_stp= None;
  sign= None;
 }

let qry_last =
 {
  subj= Variable "?y";
  pred= Constant "last";
  obj=  Constant "?last";
  ctxt= Constant "contacts";
  time_stp= None;
  sign= None;
 }

let qry_email =
 {
  subj= Variable "?y";
  pred= Constant "email";
  obj=  Constant "?email";
  ctxt= Constant "contacts";
  time_stp= None;
  sign= None;
 }

let qry_title =
 {
  subj= Variable "?y";
  pred= Constant "title";
  obj=  Constant "?title";
  ctxt= Constant "contacts";
  time_stp= None;
  sign= None;
 }

let qry_mobile =
 {
  subj= Variable "?y";
  pred= Constant "mobile";
  obj=  Constant "?mobile";
  ctxt= Constant "contacts";
  time_stp= None;
  sign= None;
 }

let qry_image = (* 17 Jun 2015: does not work-wrong output *)
 {
  subj= Variable "?y";
  pred= Constant "image";
  obj=  Constant "?image";
  ctxt= Constant "contacts";
  time_stp= None;
  sign= None;
 }

let qry_twitter = (* 17 Jun 2015: does not work-wrong output *)
 {
  subj= Variable "?y";
  pred= Constant "twitter";
  obj=  Constant "?twitter";
  ctxt= Constant "contacts";
  time_stp= None;
  sign= None;
 }

let qry_knows = (* 17 Jun 2015: does not work-wrong output *)
 {
  subj= Variable "?y";
  pred= Constant "knows";
  obj=  Constant "?knows";
  ctxt= Constant "contacts";
  time_stp= None;
  sign= None;
 }

let varsLstDefault= ["?fn"; "?last"; "?email"; "?title"; "?mobile"]

(*
 17 Jun 2015
 This is a temporary approach to produce contacts and a way
 around the original strings from Yan's example that contain
 invisible chars that the parser cannot handle. I will use this
 till the bug is fixed.
 *)
let contacts=  
               
    let str= Jsonhelper.lst_to_jsonStr 
                [("jon",     jondata);
                 ("amir",    amirdata);
                 ("anil",    anildata);
                 ("carlos",  carlosdata);
                 ("richard", richarddata)] in
     let lis= Jsonhelper.jsonStr_to_lst str    in
     Jsonhelper.tupLstToLst lis
                


 

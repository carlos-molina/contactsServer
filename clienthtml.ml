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


(*
 @Program: client_rcontacts.ml
 @author: Carlos Molina-Jimenez
 @date: 17 Feb 2015, Computer Laboratory, Univ. of Cambridge

 @sources: 
  1) I copied the server from
     https://github.com/mirage/ocaml-cohttp/blob/master/examples/async/hello_world.ml
  2) I copied the moana files from 
     https://github.com/yansh/MoanaML/blob/master/moana_irmin.ml
  3) I copied irmin from
     https://github.com/mirage/irmin
            
 @compilation: build.sh

 @Execution: % client.byte

 
 *)

exception WrongQuery of string

open Core.Std 
open Async.Std
open Config


(*
 correspondance:
 strPath to "/remote_test1"
 qry     to "qry"
 jsonParLst to lst
 
 | "/remote_test1" -> Uri.get_query_param uri "qry"
 ... ... ...
                  Rtests.remote_test1 lst) 
 *) 



(*
 place query against contact_server
 *)
let jsonStrQry strPath qry qryPar=
  let base_uri = Uri.of_string ("http://localhost:8080/"^strPath) in
   Uri.add_query_param base_uri (qry, [qryPar]) (* qry is the uri qry *) 
                                (* string * string list *)



(* Execute querySer  *)
let getContactData testAim strPath qry qryPar=
  Cohttp_async.Client.get (jsonStrQry strPath qry qryPar)  
  >>= fun (_, body) ->
  Cohttp_async.Body.to_string body 
  >>| fun jsonStr -> print_string(testAim ^"\nQuery response: \n" ^ jsonStr ^ "\n");
      print_newline()


 
let () =
(*
 client.ml Retrieve and display all the contacts in the repository
*)

  getContactData "\n\n\n\n\nTest aim: ping ping" 
                  "ping_contactsServer"
                  "key"
                  "string to bounced back!";

  getContactData "\n\n\n\n\nTest aim: get contacts details of all contacts" 
                  "getContactDataByfirstName"
                  "firstName"
                  "";

  getContactData "\n\n\n\n\nTest aim: get contacts details of Carlos" 
                  "getContactDataByfirstName"
                  "firstName"
                  "Carlos";

  getContactData "\n\n\n\n\nTest aim: get contacts details of Crowcroft" 
                  "getContactDataBylastName"
                  "lastName"
                  "Crowcroft";

(* 11 Jun 2015: the @ in the email crashes the server
  getContactData "\n\n\n\n\nTest aim: get contacts details of cm770@cam.ac.uk" 
                  "getContactDataByemail"
                  "email"
                  "cm770@cam.ac.uk";
*)

  never_returns (Scheduler.go())



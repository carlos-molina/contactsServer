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

 @Execution: % client_rcontacts.byte

 
 *)

exception WrongQuery of string

open Core.Std 
open Async.Std
open Config


let jondata= Contactshelper.makeStrOfContactData [Contactsdata.tup_jo1; Contactsdata.tup_jo2;
                                                  Contactsdata.tup_jo3;  Contactsdata.tup_jo4;
                                                  Contactsdata.tup_jo5;  Contactsdata.tup_jo6;
                                                  Contactsdata.tup_jo7;  Contactsdata.tup_jo8;
                                                  Contactsdata.tup_jo9;  Contactsdata.tup_jo10]

let amirdata= Contactshelper.makeStrOfContactData [Contactsdata.tup_am1; Contactsdata.tup_am2;
                                                   Contactsdata.tup_am3; Contactsdata.tup_am4;
                                                   Contactsdata.tup_am5; Contactsdata.tup_am6;
                                                   Contactsdata.tup_am7; Contactsdata.tup_am8;
                                                   Contactsdata.tup_am9]

let anildata= Contactshelper.makeStrOfContactData [Contactsdata.tup_an1; Contactsdata.tup_an2;
                                                   Contactsdata.tup_an3; Contactsdata.tup_an4;
                                                   Contactsdata.tup_an5; Contactsdata.tup_an6;
                                                   Contactsdata.tup_an7] 

let carlosdata= Contactshelper.makeStrOfContactData [Contactsdata.tup_ca1; Contactsdata.tup_ca2;
                                                     Contactsdata.tup_ca3; Contactsdata.tup_ca4;
                                                     Contactsdata.tup_ca5; Contactsdata.tup_ca6;
                                                     Contactsdata.tup_ca7; Contactsdata.tup_ca8]

let richarddata= Contactshelper.makeStrOfContactData [Contactsdata.tup_ri1; Contactsdata.tup_ri2;
                                                      Contactsdata.tup_ri3; Contactsdata.tup_ri4;
                                                      Contactsdata.tup_ri5; Contactsdata.tup_ri6;
                                                      Contactsdata.tup_ri7; Contactsdata.tup_ri8]


(*
 correspondance:
 strPath to "/remote_test1"
 qry     to "qry"
 jsonParLst to lst
 
 | "/remote_test1" -> Uri.get_query_param uri "qry"
 ... ... ...
                  Rtests.remote_test1 lst) 
 *) 

let jsonStrQry strPath qry qryPar=
  let base_uri = Uri.of_string ("http://localhost:8080/"^strPath) in
   Uri.add_query_param base_uri (qry, [qryPar]) (* qry is the uri qry *) 
                                (* string * string list *)

let placeJsonQ strPath qry jsonParLst= (* placeJsonQry qry qrylst *)
  let base_uri = Uri.of_string ("http://localhost:8080/"^strPath) in
   Uri.add_query_param base_uri (qry, [Jsonhelper.parLstToJsonParLst jsonParLst]) (* qry is the uri qry *) 
      
let placeJsonQrySinglePar strPath qry qryPar= (* placeJsonQry qry qrylst *)
  let base_uri = Uri.of_string ("http://localhost:8080/"^strPath) in
   Uri.add_query_param base_uri (qry, [Jsonhelper.qry_to_jsonStr qryPar]) (* qry is the uri qry *) 

                                (* string * string list *)
let placeJsonQryParTupLst strPath qry qryPar tupK tupLst=
  let base_uri = Uri.of_string ("http://localhost:8080/"^strPath) in
   Uri.add_query_params base_uri [(qry,  [Jsonhelper.qry_to_jsonStr qryPar]); 
                                  (tupK, [Jsonhelper.parLstToJsonParLst tupLst])]

let placeJsonQryTup strPath qryK qryLst tupK tupLst= (* placeJsonQry qry qrylst *)
  let base_uri = Uri.of_string ("http://localhost:8080/"^strPath) in
   Uri.add_query_params base_uri [(qryK, [Jsonhelper.parLstToJsonParLst qryLst]); 
                                  (tupK, [Jsonhelper.parLstToJsonParLst tupLst])]

let placeJsonPlcQryConStr strPath plcK plcLst qryK qryLst conK contactsStr=
  let base_uri = Uri.of_string ("http://localhost:8080/"^strPath) in
  Uri.add_query_params base_uri [(plcK, [Jsonhelper.parLstToJsonParLst plcLst]);
                                 (qryK, [Jsonhelper.parLstToJsonParLst qryLst]);
                                 (conK, [contactsStr])]

let placeJsonPlcQryConVars strPath plcK plcLst qryK qryLst conK contactsStr varsK varsStr= 
  let base_uri = Uri.of_string ("http://localhost:8080/"^strPath) in
  Uri.add_query_params base_uri [(plcK,  [Jsonhelper.parLstToJsonParLst plcLst]);
                                 (qryK,  [Jsonhelper.parLstToJsonParLst qryLst]);
                                 (conK,  [contactsStr]);
                                 (varsK, [varsStr])]


let placeJsonQuLstTuLsts strPath qK qLst tK1 tLst1 tK2 tLst2 tK3 tLst3=  
  let base_uri = Uri.of_string ("http://localhost:8080/"^strPath) in
   Uri.add_query_params base_uri [(qK,  [Jsonhelper.parLstToJsonParLst qLst]); 
                                  (tK1, [Jsonhelper.parLstToJsonParLst tLst1]);
                                  (tK2, [Jsonhelper.parLstToJsonParLst tLst2]);
                                  (tK3, [Jsonhelper.parLstToJsonParLst tLst3])]
                                 
let placeJsonQrysTupLst strPath q1 qPar1 q2 qPar2 q3 qPar3 q4 qPar4 tupK tupLst=  
  let base_uri = Uri.of_string ("http://localhost:8080/"^strPath) in
   Uri.add_query_params base_uri [(q1,  [Jsonhelper.qry_to_jsonStr qPar1]); 
                                  (q2,  [Jsonhelper.qry_to_jsonStr qPar2]);
                                  (q3,  [Jsonhelper.qry_to_jsonStr qPar3]);
                                  (q4,  [Jsonhelper.qry_to_jsonStr qPar4]);
                                  (tupK,[Jsonhelper.parLstToJsonParLst tupLst])]


(* Execute querySer  *)
let testMoanaStrQry testAim strPath qry qryPar=
  Cohttp_async.Client.get (jsonStrQry strPath qry qryPar)  
  >>= fun (_, body) ->
  Cohttp_async.Body.to_string body 
  >>| fun jsonStr -> print_string(testAim ^"\nQuery response...\n" ^ jsonStr ^ "\n");
      print_newline()

(* Execute querySer  *)
let testMoana strPath strKey jsonParLst=
  Cohttp_async.Client.get (placeJsonQ strPath strKey jsonParLst)  (* placeJsonQry qry qryLst *)
  >>= fun (_, body) ->
  Cohttp_async.Body.to_string body 
  >>| fun jsonStr -> print_string("Remote test executed " ^ jsonStr ^ "\n");
      print_newline()


(* Execute querySer  *)
let testMoanaTupRsp strPath strKey jsonParLst=
  Cohttp_async.Client.get (placeJsonQ strPath strKey jsonParLst)  (* placeJsonQry qry qryLst *)
  >>= fun (_, body) ->
  Cohttp_async.Body.to_string body 
  >>| fun jsonStr -> let rspLst= Jsonhelper.getQryParLst jsonStr in
  (* Contactshelper.prtQryResponse rspLst; *)
  let lst= List.map ~f:(fun s -> Contactshelper.tup_to_str_long s) rspLst in
  List.iter ~f:(fun s -> print_string s) lst;
  print_newline()


(* Execute querySer  *)
let testMoanaSinglePar strPath qry qryPar=
  Cohttp_async.Client.get (placeJsonQrySinglePar strPath qry qryPar)  (* placeJsonQry qry qryLst *)
  >>= fun (_, body) ->
  Cohttp_async.Body.to_string body 
  >>| fun jsonStr -> print_string("Remote test executed " ^ jsonStr ^ "\n");
      print_newline()

(* Execute querySer  *)
let testMoanaQryLstTupLst strPath qryK qryLst tupK tupLst=
  Cohttp_async.Client.get (placeJsonQryTup strPath qryK qryLst tupK tupLst) 
  >>= fun (_, body) ->
  Cohttp_async.Body.to_string body 
  >>| fun jsonStr -> print_string("Remote test executed " ^ jsonStr ^ "\n");
      print_newline()
 
(* Execute querySer  *)
let testMoanaPlcQryConStr strPath plcK plcLst qryK qryLst conK contactsStr=
  Cohttp_async.Client.get (placeJsonPlcQryConStr strPath plcK plcLst qryK qryLst conK contactsStr) 
  >>= fun (_, body) ->
  Cohttp_async.Body.to_string body 
  >>| fun jsonStr -> print_string("Remote test executed " ^ jsonStr ^ "\n");
      print_newline()

(* Execute querySer  *)
let testMoanaPlcQryConVars testAim strPath plcK plcLst qryK qryLst conK contactsStr varsK varsLst=
  Cohttp_async.Client.get (placeJsonPlcQryConVars strPath plcK plcLst qryK qryLst conK contactsStr varsK varsLst) 
  >>= fun (_, body) ->
  Cohttp_async.Body.to_string body 
  >>| fun jsonStr -> print_string(testAim^ "\nQuery response...\n" ^ jsonStr ^ "\n");
      print_newline()



(* Execute querySer  *)
let testMoanaQrysTupLst strPath qK1 qP1 qK2 qP2 qK3 qP3 qK4 qP4 tupK tupLst=
  Cohttp_async.Client.get (placeJsonQrysTupLst strPath qK1 qP1 qK2 qP2 qK3 qP3 qK4 qP4 tupK tupLst) 
  >>= fun (_, body) ->
  Cohttp_async.Body.to_string body 
  >>| fun jsonStr -> print_string("Remote test executed " ^ jsonStr ^ "\n");
      print_newline()

(* Execute querySer  *)
let testMoanaQuLstTuLstTuLstTuLst strPath qK qLst tK1 tLst1 tK2 tLst2 tK3 tLst3=
  Cohttp_async.Client.get (placeJsonQuLstTuLsts strPath qK qLst tK1 tLst1 tK2 tLst2 tK3 tLst3) 
  >>= fun (_, body) ->
  Cohttp_async.Body.to_string body 
  >>| fun jsonStr -> print_string("Remote test executed " ^ jsonStr ^ "\n");
      print_newline()

(* Execute querySer  *)
let testMoanaQryTupLst strPath qry qryPar tupK tupLst=
  Cohttp_async.Client.get (placeJsonQryParTupLst strPath qry qryPar tupK tupLst) 
  >>= fun (_, body) ->
  Cohttp_async.Body.to_string body 
  >>| fun jsonStr -> print_string("Remote test executed " ^ jsonStr ^ "\n");
      print_newline()

 
let () =

(*
  testMoanaStrQry "ping_rcontacts" "key" "Olas Simon";  
*)

(*
 Retrieve and display all the contacts in the repository
*)

  testMoanaStrQry "\n\n\n\n\nTest aim: display all the contacts from the repository" 
                  "rcontacts_getStringOfTuples" 
                  "contacts" (Jsonhelper.lst_to_jsonStr   
                             [("jon",    jondata);
                              ("amir",    amirdata);
                              ("anil",    anildata);
                              ("carlos",  carlosdata);
                              ("richard", richarddata)]);




 
(*
 Show IDs of people that Jon knows
 *)
  testMoanaPlcQryConVars  "\n\n\n\n\nTest aim: show IDs of people that Jon knows"
                 "rcontacts_plc_qry_contacts_vars" 
                 "plcLst" [Contactsdata.plcy_tu1; Contactsdata.plcy_tu2; 
                           Contactsdata.plcy_tu3; Contactsdata.plcy_tu4] 
                 "qryLst" [Contactsdata.qt1jon]
                 "contactsStr" (Jsonhelper.lst_to_jsonStr   
                             [("jon",     jondata);
                              ("amir",    amirdata);
                              ("anil",    anildata);
                              ("carlos",  carlosdata);
                              ("richard", richarddata)])
                 "varsStr"  (Jsonhelper.strLst_to_jsonStr [ "?y"; "?name"; "?email" ] );


(*
 Show IDs of people that amir knows
 *)
  testMoanaPlcQryConVars  "\n\n\n\n\nTest aim: show IDs of people that Amir knows"
                 "rcontacts_plc_qry_contacts_vars" 
                 "plcLst" [Contactsdata.plcy_tu1; Contactsdata.plcy_tu2; 
                           Contactsdata.plcy_tu3; Contactsdata.plcy_tu4] 
                 "qryLst" [Contactsdata.qt1amir]
                 "contactsStr" (Jsonhelper.lst_to_jsonStr   
                             [("jon",     jondata);
                              ("amir",    amirdata);
                              ("anil",    anildata);
                              ("carlos",  carlosdata);
                              ("richard", richarddata)])
                 "varsStr"  (Jsonhelper.strLst_to_jsonStr [ "?y"; "?name"; "?email" ] );


(*
 Get carlos' email 
 *)
  testMoanaPlcQryConVars  "\n\n\n\n\nTest aim: show carlos' email"
                 "rcontacts_plc_qry_contacts_vars" 
                 "plcLst" [Contactsdata.plcy_tu1; Contactsdata.plcy_tu2; 
                           Contactsdata.plcy_tu3; Contactsdata.plcy_tu4] 
                 "qryLst" [Contactsdata.qt1carlosemail]
                 "contactsStr" (Jsonhelper.lst_to_jsonStr   
                             [("jon",     jondata);
                              ("amir",    amirdata);
                              ("anil",    anildata);
                              ("carlos",  carlosdata);
                              ("richard", richarddata)])
                 "varsStr"  (Jsonhelper.strLst_to_jsonStr [ "?y"; "?name"; "?email" ] );
(*
 Get richard's email 
 *)
  testMoanaPlcQryConVars  "\n\n\n\n\nTest aim: show richard's email"
                 "rcontacts_plc_qry_contacts_vars" 
                 "plcLst" [Contactsdata.plcy_tu1; Contactsdata.plcy_tu2; 
                           Contactsdata.plcy_tu3; Contactsdata.plcy_tu4] 
                 "qryLst" [Contactsdata.qt1richardemail]
                 "contactsStr" (Jsonhelper.lst_to_jsonStr   
                             [("jon",     jondata);
                              ("amir",    amirdata);
                              ("anil",    anildata);
                              ("carlos",  carlosdata);
                              ("richard", richarddata)])
                 "varsStr"  (Jsonhelper.strLst_to_jsonStr [ "?y"; "?name"; "?email" ] );


(*
 Show IDs of people that Jon knows
 *)

  testMoanaPlcQryConVars  "\n\n\n\n\nTest aim: show IDs, first names and emails of people that Jon knows"
                 "rcontacts_plc_qry_contacts_vars" 
                 "plcLst" [Contactsdata.plcy_tu1; Contactsdata.plcy_tu2; 
                           Contactsdata.plcy_tu3; Contactsdata.plcy_tu4] 
                 "qryLst" [Contactsdata.qt1jon; Contactsdata.qt2; Contactsdata.qt3 ]
                 "contactsStr" (Jsonhelper.lst_to_jsonStr   
                             [("jon",     jondata);
                              ("amir",    amirdata);
                              ("anil",    anildata);
                              ("carlos",  carlosdata);
                              ("richard", richarddata)])
                 "varsStr"  (Jsonhelper.strLst_to_jsonStr [ "?y"; "?name"; "?email" ] );


(*
 Show that carlos' last name is not visible 'cos of the policy in force (plcLst).  
 *)
  testMoanaPlcQryConVars  "\n\n\n\n\nTest aim: show that the policy in force makes that carlos' last name invisible"
                 "rcontacts_plc_qry_contacts_vars" 
                 "plcLst" [Contactsdata.plcy_tu1; Contactsdata.plcy_tu2; 
                           Contactsdata.plcy_tu3; Contactsdata.plcy_tu4] (* plcy_tu5_last *) 
                 "qryLst" [Contactsdata.qt1carloslast]
                 "contactsStr" (Jsonhelper.lst_to_jsonStr   
                             [("jon",     jondata);
                              ("amir",    amirdata);
                              ("anil",    anildata);
                              ("carlos",  carlosdata);
                              ("richard", richarddata)])
                 "varsStr"  (Jsonhelper.strLst_to_jsonStr [ "?y"; "?name"; "?email" ] );

(*
 Show Anil's last name. The policy is force (plcLst) makes it visible.
 *)
  testMoanaPlcQryConVars  "\n\n\n\n\nTest aim: show that the policy in force makes Anil's last name visible"
                 "rcontacts_plc_qry_contacts_vars" 
                 "plcLst" [Contactsdata.plcy_tu1; Contactsdata.plcy_tu2; 
                           Contactsdata.plcy_tu3; Contactsdata.plcy_tu4; 
                           Contactsdata.plcy_tu5_last]
                 "qryLst" [Contactsdata.qt1anillast]
                 "contactsStr" (Jsonhelper.lst_to_jsonStr   
                             [("jon",     jondata);
                              ("amir",    amirdata);
                              ("anil",    anildata);
                              ("carlos",  carlosdata);
                              ("richard", richarddata)])
                 "varsStr"  (Jsonhelper.strLst_to_jsonStr [ "?y"; "?name"; "?email" ] );


  never_returns (Scheduler.go())

(*

  testMoanaPlcQryConStr  "rcontacts_plc_qry_contacts" 
               "plcLst" [Contactsdata.plcy_tu1; Contactsdata.plcy_tu2; 
                         Contactsdata.plcy_tu3; Contactsdata.plcy_tu4] 
               "qryLst" [Contactsdata.qt1; Contactsdata.qt2; Contactsdata.qt3]
               "contactsStr" (Jsonhelper.lst_to_jsonStr   
                          [("jon",     jondata);
                           ("amir",    amirdata);
                           ("anil",    anildata);
                           ("carlos",  carlosdata);
                           ("richard", richarddata)]);
*)

(*
   testMoanaStrQry "rcontacts_getStrTuples" "repoName"  "any string"; 
   testMoanaStrQry "rcontacts_getwholemap"  "repoName"  "any string";
*)


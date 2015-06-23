(*
 @Program: client_rtests.ml
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

 @Execution: % client_rtests.byte

 
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
let testMoanaStrQry strPath qry qryPar=
  Cohttp_async.Client.get (jsonStrQry strPath qry qryPar)  
  >>= fun (_, body) ->
  Cohttp_async.Body.to_string body 
  >>| fun jsonStr -> print_string("Remote test executed " ^ jsonStr ^ "\n");
      print_newline()

(* Execute querySer  *)
let testMoana strPath strKey jsonParLst=
  Cohttp_async.Client.get (placeJsonQ strPath strKey jsonParLst)  (* placeJsonQry qry qryLst *)
  >>= fun (_, body) ->
  Cohttp_async.Body.to_string body 
  >>| fun jsonStr -> print_string("Remote test executed " ^ jsonStr ^ "\n");
      print_newline()


(* Execute querySer  *)
let testMoanaTupRsp testAim strPath strKey jsonParLst=
  Cohttp_async.Client.get (placeJsonQ strPath strKey jsonParLst)  (* placeJsonQry qry qryLst *)
  >>= fun (_, body) ->
  Cohttp_async.Body.to_string body 
  >>| fun jsonStr -> let rspLst= Jsonhelper.getQryParLst jsonStr in
  (* Contactshelper.prtQryResponse rspLst; *)
  let lst= List.map ~f:(fun s -> Contactshelper.tup_to_str_long s) rspLst in
  print_string(testAim);
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
let testMoanaQryLstTupLstRsp testAim strPath qryK qryLst tupK tupLst=
  Cohttp_async.Client.get (placeJsonQryTup strPath qryK qryLst tupK tupLst) 
  >>= fun (_, body) ->
  Cohttp_async.Body.to_string body 
  >>| fun jsonStr -> let rspLst= Jsonhelper.getQryParLst jsonStr in
  (* Contactshelper.prtQryResponse rspLst; *)
  let lst= List.map ~f:(fun s -> Contactshelper.tup_to_str_long s) rspLst in
  print_string(testAim);
  List.iter ~f:(fun s -> print_string s) lst;
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
let testMoanaPlcQryConVars strPath plcK plcLst qryK qryLst conK contactsStr varsK varsLst=
  Cohttp_async.Client.get (placeJsonPlcQryConVars strPath plcK plcLst qryK qryLst conK contactsStr varsK varsLst) 
  >>= fun (_, body) ->
  Cohttp_async.Body.to_string body 
  >>| fun jsonStr -> print_string("Remote test executed " ^ jsonStr ^ "\n");
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
  The server first stores a list of tuples [t1; ... ;t11] in
  irmin respository.
  Secondly, it retrives the tuples that match the query 
 *)
(*
  testMoanaTupRsp "\n\n\n\n\nTest aim: retrieves the tuples from the irmin repository that match qry: \n" 
             "remote_test0_tupRsp" 
             "qry" [Contactsdata.q1;Contactsdata.q2];
  
*)


(* 
  The server first stores a list of tuples [t1; ... ;t11] in
  irmin respository.
  Secondly, it retrives all the tuples found in irmin repository
 *)
(*
  testMoanaTupRsp "\n\n\n\n\nTest aim: retrieves all the tuples from the irmin repository: \n"
             "remote_test0_tupRsp" 
             "qry" [Contactsdata.q_get_all];
*)

(* 
  The server first stores a list of tuples given as tupLst in the
  irmin respository.
  Secondly, it retrives from the irmin repository all the tuples.
 *)
  testMoanaQryLstTupLstRsp "\n\n\n\n\n Test aim: \n  1st) init the irmin repository with tupLst.
  2nd) retrieve all the contacts tuples from the irmin repository: \n"
               "remote_test0_contacts_tuples" 
               "qryLst" [Contactsdata.q_get_all] 
               "tupLst" [Contactsdata.tup_jo1; Contactsdata.tup_jo2; Contactsdata.tup_jo3; Contactsdata.tup_jo4; 
                         Contactsdata.tup_jo5; Contactsdata.tup_jo6; Contactsdata.tup_jo7; Contactsdata.tup_jo8; 
                         Contactsdata.tup_jo9; Contactsdata.tup_jo10; 

                         Contactsdata.tup_am1; Contactsdata.tup_am2; Contactsdata.tup_am3; Contactsdata.tup_am4; 
                         Contactsdata.tup_am5; Contactsdata.tup_am6; Contactsdata.tup_am7; Contactsdata.tup_am8; 
                         Contactsdata.tup_am9;
 
                         Contactsdata.tup_an1; Contactsdata.tup_an2; Contactsdata.tup_an3; Contactsdata.tup_an4; 
                         Contactsdata.tup_an5; Contactsdata.tup_an6; Contactsdata.tup_an7;

                         Contactsdata.tup_ca1; Contactsdata.tup_ca2; Contactsdata.tup_ca3; Contactsdata.tup_ca4; 
                         Contactsdata.tup_ca5; Contactsdata.tup_ca6; Contactsdata.tup_ca7; Contactsdata.tup_ca8;
 
                         Contactsdata.tup_ri1; Contactsdata.tup_ri2; Contactsdata.tup_ri3; Contactsdata.tup_ri4; 
                         Contactsdata.tup_ri5; Contactsdata.tup_ri6; Contactsdata.tup_ri7; Contactsdata.tup_ri8];

(*

   testMoana "remote_test0_tu" 
             "qry" [Contactsdata.q1; Contactsdata.q2];

   testMoanaQryLstTupLst "remote_test0" 
                         "qryLst" [Contactsdata.q1;Contactsdata.q2] 
                         "tupLst" [Contactsdata.t1; Contactsdata.t9];

   testMoanaQryLstTupLst "remote_test1" 
                         "qryLst" [Contactsdata.q1; Contactsdata.q3; Contactsdata.q4; Contactsdata.q5] 
                         "tupLst" [Contactsdata.t1;Contactsdata.t2; Contactsdata.t5; Contactsdata.t7];

   testMoanaQryLstTupLst "remote_test2" 
                         "qryLst" [Contactsdata.q5; Contactsdata.q1; Contactsdata.q3; Contactsdata.q4] 
                         "tupLst" [Contactsdata.t7; Contactsdata.t1;Contactsdata.t2; Contactsdata.t5];

   testMoanaQryLstTupLst "remote_test3" 
                         "qryLst" [Contactsdata.q6; Contactsdata.q7] 
                         "tupLst" [Contactsdata.t11; Contactsdata.t10];

   testMoanaQuLstTuLstTuLstTuLst "remote_test8" 
                                 "qryLst"  [Contactsdata.q1; Contactsdata.q3; Contactsdata.q4; Contactsdata.q5]
                                 "tupLst1" [Contactsdata.t12; Contactsdata.t13; Contactsdata.t14; Contactsdata.t15]
                                 "tupLst2" [Contactsdata.t7; Contactsdata.t5;Contactsdata.t2; Contactsdata.t1]
                                 "tupLst3" [Contactsdata.t15; Contactsdata.t14; Contactsdata.t13; Contactsdata.t12];

   testMoanaQrysTupLst "remote_test9" 
                       "qry1"   Contactsdata.q1
                       "qry2"   Contactsdata.q3 
                       "qry3"   Contactsdata.q4 
                       "qry4"   Contactsdata.q5 
                       "tupLst" [Contactsdata.t7; Contactsdata.t5;Contactsdata.t2; Contactsdata.t1];

   testMoanaQryLstTupLst "remote_test11" 
                         "qryLst" [Contactsdata.q1;Contactsdata.q2] 
                         "tupLst" [Contactsdata.t1; Contactsdata.t9];

   testMoanaQryLstTupLst "remote_test12" 
                         "tupLst1" [Contactsdata.t1;Contactsdata.t2] 
                         "tupLst2" [Contactsdata.t1;Contactsdata.t2; Contactsdata.t3];

   testMoanaStrQry  "remote_test14" "qry" "(a,type,Car,context)";

   testMoanaQryLstTupLst "remote_test19" 
                         "qryLst" [Contactsdata.q1; Contactsdata.q2] 
                         "tupLst" [Contactsdata.t1; Contactsdata.t9];
 
   testMoanaQryLstTupLst "remote_test20" 
                         "qryLst" [Contactsdata.q1; Contactsdata.q2] 
                         "tupLst" [Contactsdata.t9; Contactsdata.t1]; 
    
   testMoanaQryLstTupLst "remote_test22" 
                         "qryLst" [Contactsdata.q1;Contactsdata.q2] 
                         "tupLst" [Contactsdata.t9; Contactsdata.t1]; 


   testMoanaQryTupLst "remote_test4" 
                      "qryPar" Contactsdata.q6 
                      "tupLst" [Contactsdata.t10; Contactsdata.t11];

   testMoanaQryTupLst "remote_test5" 
                      "qryPar" Contactsdata.q6 
                      "tupLst" [Contactsdata.t10; Contactsdata.t11];

   testMoanaQryTupLst "remote_test6" 
                      "qryPar" Contactsdata.q1 
                      "tupLst" [Contactsdata.t1];

*)

   (*
     tests against rcontacts.ml
    *)

  never_returns (Scheduler.go())



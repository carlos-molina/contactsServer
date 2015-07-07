(* * Copyright (c) 2014 Carlos Molina-Jimenez.                             *)
(* These tests were designed by Yan Shvartzshnaider.  I only addapted to   *)
(* run remotely.                                                           *)
(* Permission to use, copy,                                                *)
(* modify, and distribute this software for any * purpose with or without  *)
(* fee is hereby granted, provided that the above * copyright notice and   *)
(* this permission notice appear in all copies. * * THE SOFTWARE IS        *)
(* PROVIDED "AS IS" AND THE AUTHOR DISCLAIMS ALL WARRANTIES * WITH REGARD  *)
(* TO THIS SOFTWARE INCLUDING ALL IMPLIED WARRANTIES OF * MERCHANTABILITY  *)
(* AND FITNESS. IN NO EVENT SHALL THE AUTHOR BE LIABLE FOR * ANY SPECIAL,  *)
(* DIRECT, INDIRECT, OR CONSEQUENTIAL DAMAGES OR ANY DAMAGES * WHATSOEVER  *)
(* RESULTING FROM LOSS OF USE, DATA OR PROFITS, WHETHER IN AN * ACTION OF  *)
(* CONTRACT, NEGLIGENCE OR OTHER TORTIOUS ACTION, ARISING OUT OF * OR IN   *)
(* CONNECTION WITH THE USE OR PERFORMANCE OF THIS SOFTWARE.                *)
open Config
  
open Helper
  
open OUnit
  
open Lexing
  
(***************  TUPLES ***********************)
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
  
let tuples = [ t1; t2; t3; t4; t5; t6; t7; t8; t9; t10; t11 ]
  
(* Config.print_tuples tuples;; *)
(*************** QUERY TEMPLATE TUPLES *******************)
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
    obj  = Variable "?y";
    ctxt = Constant "context";
    time_stp = None;
    sign = None;
  }
  
let q4 =
  {
    subj = Variable "?y";
    pred = Constant "type";
    obj  = Constant "Color";
    ctxt = Constant "context";
    time_stp = None;
    sign = None;
  }
  
let q5 =
  {
    subj = Variable "?y";
    pred = Constant "rgbValue";
    obj  = Constant "White";
    ctxt = Constant "context";
    time_stp = None;
    sign = None;
  }
  
let q5' =
  {
    subj = Variable "?y";
    pred = Constant "rgbValue";
    obj  = Variable "?c";
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
  
(****************** TESTS **********************************)
let _ = print_endline "---- Unit tests ----- "
  
(* TEST 0:
   
 MAP {    
     ?x, type, Car
     ?x, hasColor, Red   
    }
     
*)


let test0 _ =
  let db = Moana_lists.S.init tuples in
  let query = [ q1; q2 ] and q_exp_res = [ [ t1; t9 ] ] in
  let res_q = Moana_lists.S.query db query in assert_equal q_exp_res res_q

let remote_test0_basic lst =
  let db     = Moana_lists.S.init tuples          in
  let query  = lst and q_exp_res = [ [ t1; t9 ] ] in
  let res_q  = Moana_lists.S.query db query       in 
  let result = assert_equal q_exp_res res_q       in
  "\n remote_test0_basic Pass \n"

let remote_test0_tu (* qryLst *)  tupLst=
  let db     = Moana_lists.S.init tuples          in
  let query  = [q1;q2] (* qryLst *) and q_exp_res = [tupLst] in
  let res_q  = Moana_lists.S.query db query       in 
  let result = assert_equal q_exp_res res_q       in
  "\n basic remote_test0_tu Pass \n"

let remote_test0 qryLst tupLst=
  let db     = Moana_lists.S.init tuples          in
  let query  = qryLst and q_exp_res = [tupLst] in
  let res_q  = Moana_lists.S.query db query       in 
  let result = assert_equal q_exp_res res_q       in
  "\n remote_test0 Pass !!! \n"

let remote_test0_contacts_tuples qryLst tupLst=
  let db     = Moana_lists.S.init tupLst          in
  let rspLst = Moana_lists.S.query db qryLst      in 
  let tupLst = List.flatten rspLst                in
  Jsonhelper.parLstToJsonParLst tupLst   

(*
let remote_test0_tupRsp qryLst=
  let db      = Moana_lists.S.init tuples          in
  let rspLst  = Moana_lists.S.query db qryLst      in 
  let tupLst  = List.flatten rspLst                in
  Jsonhelper.parLstToJsonParLst tupLst   
*)

let remote_test0_tupRsp qryLst=
  let db      = Moana_lists.S.init tuples          in
  let rspLst  = Moana_lists.S.query db qryLst      in 
  let tupLst  = List.flatten rspLst                in
  Jsonhelper.parLstToJsonParLst tupLst   

 
(*print_tuples_list res_q1;;
 print_tuples_list q1_exp_res;;*)
(* TEST 1:
   
 MAP  {    
     ?x, type, Car
     ?x, color, ?y
     ?y, type, Color
     ?y, rgbValue, White  
    }test1
     
*)
let test1 _ =
  let query = [ q1; q3; q4; q5 ] and q_exp_res = [ [ t1; t2; t5; t7 ] ] in
  let res_q = execute_query tuples query in assert_equal q_exp_res res_q


let remote_test1_basic lst =
  let query  = lst and q_exp_res = [ [ t1; t2; t5; t7 ] ] in
  let res_q  = execute_query tuples query   in 
  let result = assert_equal q_exp_res res_q in
  "\n remote_test1_basic Pass \n"
 

let remote_test1 qryLst tupLst=
  let query  = qryLst and q_exp_res = [ tupLst ] in
  let res_q  = execute_query tuples query   in 
  let result = assert_equal q_exp_res res_q in
  "\n remote_test1 Pass \n"

 
(* TEST 2: similar to test 1 query but the order of the tuple template is different
   
 MAP  {    
     ?y, rgbValue, White
     ?x, type, Car
     ?x, color, ?y
     ?y, type, Color  
    }
     
*)
let test2 _ =
  let query = [ q5; q1; q3; q4 ] and q_exp_res = [ [ t7; t1; t2; t5 ] ] in
  let res_q = execute_query tuples query in assert_equal q_exp_res res_q


let remote_test2_basic lst=  (* I cant make this pass 15 Feb *)
  let query = lst (* [ q5; q1; q3; q4 ] *) and q_exp_res = [ [ t7; t1; t2; t5 ] ] in
  let res_q  = execute_query tuples query   in 
  let result = assert_equal q_exp_res res_q in
  "\n remote_test2_basic Pass \n"

let remote_test2 qryLst tupLst=  (* I cant make this pass 15 Feb *)
  let query = qryLst and q_exp_res = [tupLst] in
  let res_q  = execute_query tuples query   in 
  let result = assert_equal q_exp_res res_q in
  "\n remote_test2 Pass \n"

  
(*
   print_tuples_list q2_exp_res;;*)
(* TEST 3: - Note - expected results are order sensetive
   
 MAP  {    
     ?x, relatesTo, ?y
     ?y, relatesTo, ?x
    }
     
*)
let test3 _ =
  let query = [ q6; q7 ] and q_exp_res = [ [ t11; t10 ]; [ t10; t11 ] ] in
  let res_q = execute_query tuples query in assert_equal q_exp_res res_q
 
let remote_test3_basic lst=
  let query = lst and q_exp_res = [ [ t11; t10 ]; [ t10; t11 ] ] in
  let res_q = execute_query tuples query   in 
  let result= assert_equal q_exp_res res_q in
  "\n remote_test3_basic Pass \n"

let remote_test3 qryLst tupLst=
  let query = qryLst and q_exp_res = [ tupLst; [ t10; t11 ] ] in
  let res_q = execute_query tuples query   in 
  let result= assert_equal q_exp_res res_q in
  "\n remote_test3 Pass \n"


 
(******* based on LUBM benchmark for SPARQL queries **** 
 *  http://swat.cse.lehigh.edu/projects/lubm/queries-sparql.txt
 *)
(* TODO TEST  :
   
MAP {
  ?X type GraduateStudent,
  ?X takesCourse GraduateCourse0}
     
*)
(* TODO TEST :
   
MAP  {    
  X type GraduateStudent,
  ?Y type University,
  ?Z type Department,
  ?X memberOf ?Z,
  ?Z subOrganizationOf ?Y,
  ?X undergraduateDegreeFrom ?Y 
  }
     
*)
(* TODO TEST :
   
MAP  {    
  ?X type Student,
  ?Y type Faculty,
  ?Z type Course,
  ?X advisor ?Y,
  ?Y teacherOf ?Z,
  ?X takesCourse ?Z,
}
     
*)
(*  test filter function *)
let test4 _ =
  let query = q6 and q_exp_res = [ t10; t11 ] in
  let res_q = Rete.filter query tuples in assert_equal q_exp_res res_q



(*  test filter function *)
let remote_test4_basic qry_q6 =
  let query = qry_q6 and q_exp_res = [ t10; t11 ] in
  let res_q = Rete.filter query tuples            in 
  let result= assert_equal q_exp_res res_q        in
  "\n remote_test4_basic Pass \n"

(*  test filter function *)
let remote_test4 qry_q6  tupLst=
  let query = qry_q6 and q_exp_res = tupLst in
  let res_q = Rete.filter query tuples            in 
  let result= assert_equal q_exp_res res_q        in
  "\n remote_test4 Pass \n"

  
(* create alpha memory and add tuples to it *)
let test5 _ =
  let query = q6 and q_exp_res = [ t10; t11 ] in
  let am = Rete.create_am query tuples in assert_equal q_exp_res am.tuples


(* create alpha memory and add tuples to it *)
let remote_test5_basic  qry_q6=
  let query = qry_q6 and q_exp_res = [ t10; t11 ] in
  let am = Rete.create_am query tuples            in 
  let results= assert_equal q_exp_res am.tuples   in
  "\n remote_test5_basic Pass \n"

(* create alpha memory and add tuples to it *)
let remote_test5  qry_q6 tupLst=
  let query = qry_q6 and q_exp_res = tupLst in
  let am = Rete.create_am query tuples            in 
  let results= assert_equal q_exp_res am.tuples   in
  "\n remote_test5 Pass \n"

  
(* test simple join function AM with empty BM *)
let test6 _ =
  let module Test =
    struct
      open Rete
        
      let q_exp_res = { solutions = [ ("?x", ((Constant "a"), [ t1 ])) ]; }
        
      let am = create_am q1 tuples
        
      let bm = { solutions = []; }
        
      let new_bm = join am bm
        
    end
  in assert_equal Test.new_bm Test.q_exp_res
 

(* test simple join function AM with empty BM *)
let remote_test6_basic qry_q1=
  let module Test =
    struct
      open Rete
        
      let q_exp_res = { solutions = [ ("?x", ((Constant "a"), [ t1 ])) ]; }
        
      let am = create_am qry_q1 tuples
        
      let bm = { solutions = []; }
        
      let new_bm = join am bm
        
    end in
  let result= assert_equal Test.new_bm Test.q_exp_res in
  "\n remote_test6_basic Pass \n"
(* test simple join function AM with empty BM *)


let remote_test6 qry_q1 tupLst=
  let module Test =
    struct
      open Rete
        
      let q_exp_res = { solutions = [ ("?x", ((Constant "a"), tupLst)) ]; }
        
      let am = create_am qry_q1 tuples
        
      let bm = { solutions = []; }
        
      let new_bm = join am bm
        
    end in
  let result= assert_equal Test.new_bm Test.q_exp_res in
  "\n remote_test6 Pass \n"


 
(* TEST :
 
Implement by create a combination of AM and BM
 MAP {    
     ?x, type, Car
     ?x, hasColor, Red   
    }
*)
let test7 _ =
  let module Test =
    struct
      open Rete
        
      let am1 = create_am q1 tuples

      and am2 = create_am q2 tuples

      and q_exp_res =
        { solutions = [ ("?x", ((Constant "a"), [ t9; t1 ])) ]; }
        
      let bm = { solutions = []; }
        
      let res_bm = join am2 (join am1 bm)
        
    end
  in assert_equal Test.res_bm Test.q_exp_res
  
(* TEST 8: 
   
 MAP  {    
     ?x, type, Car
     ?x, hasColor, ?y
     ?y, type, Color
     ?y, rgbValue, White  
    }
*)


let test8 _ =
  let module Test =
    struct
      open Rete
        
      let exd_tuples = [ t12; t13; t14; t15 ] @ tuples
        
      let q_exp_res =
        {
          solutions =
            [ ("?y", ((Constant "c"), [ t7; t5; t2; t1 ]));
              ("?y", ((Constant "c2"), [ t15; t14; t13; t12 ])) ];
        }
        
      let am1 = create_am q1 exd_tuples

      and am2 = create_am q3 exd_tuples
      and am3 = create_am q4 exd_tuples

      and am4 = create_am q5 exd_tuples
        
      (*let p = print_mappings am2*)
      let res_bm =
        join am4 (join am3 (join am2 (join am1 { solutions = []; })))
        
    end
  in (*in let p = print_bm res_bm*) assert_equal Test.res_bm Test.q_exp_res


let remote_test8 qryLst tupLst1 tupLst2 tupLst3=
  let module Test =
    struct
      open Rete
        
      let exd_tuples = tupLst1  @ tuples
        
      let q_exp_res =
        {
          solutions =
            [ ("?y", ((Constant "c"), tupLst2));
              ("?y", ((Constant "c2"), tupLst3)) ];
        }
        
      let am1 = create_am (* q1 *) (List.nth qryLst 0) exd_tuples

      and am2 = create_am (* q3 *) (List.nth qryLst 1) exd_tuples
      and am3 = create_am (* q4 *) (List.nth qryLst 2) exd_tuples

      and am4 = create_am (* q5 *) (List.nth qryLst 3) exd_tuples
        
      (*let p = print_mappings am2*)
      let res_bm =
        join am4 (join am3 (join am2 (join am1 { solutions = []; })))
        
    end in
     (*in let p = print_bm res_bm*) 
    let result= assert_equal Test.res_bm Test.q_exp_res in
    "\n remote.test8 Pass \n"


  
(* TEST 9: 
   
 MAP  {    
     ?x, type, Car
     ?x, hasColor, ?y
     ?y, type, Color
     ?y, rgbValue, White  
    }
*)
let test9 _ =
  let module Test =
    struct
      open Rete
        
      let q_exp_res =
        { solutions = [ ("?y", ((Constant "c"), [ t7; t5; t2; t1 ])) ]; }
        
      let am1 = create_am q1 tuples

      and am2 = create_am q3 tuples
      and am3 = create_am q4 tuples

      and am4 = create_am q5 tuples
        
      let am_list = [ am1; am2; am3; am4 ]
        
      (*let p = print_mappings am2*)
      let res_bm = execute_am_list (List.rev am_list)
        
    end
  in (*let p = print_bm res_bm in *) assert_equal Test.res_bm Test.q_exp_res


let remote_test9 qr_q1 qr_q3 qr_q4 qr_q5 tupLst =
  let module Test =
    struct
      open Rete
        
      let q_exp_res =
        { solutions = [ ("?y", ((Constant "c"), tupLst (* [ t7; t5; t2; t1 ] *) )) ]; }
        
      let am1 = create_am qr_q1 tuples

      and am2 = create_am qr_q3 tuples
      and am3 = create_am qr_q4 tuples

      and am4 = create_am qr_q5 tuples
        
      let am_list = [ am1; am2; am3; am4 ]
        
      (*let p = print_mappings am2*)
      let res_bm = execute_am_list (List.rev am_list)
        
    end in
  let result= (*let p = print_bm res_bm in *) assert_equal Test.res_bm Test.q_exp_res in
   "\n remote_test9 Pass \n"

  
(*** TEST 10: 
   
Accepts list of AM and create Rete network. The last BM in the network contains 
the matching tuples to the query: 
 
 MAP  {    
     ?x, type, Car
     ?x, hasColor, ?y
     ?y, type, Color
     ?y, rgbValue, White  
    }
***)
let test10 _ =
  let module Test =
    struct
      open Rete
        
      let q_exp_res =
        { solutions = [ ("?y", ((Constant "c"), [ t7; t5; t2; t1 ])) ]; }
        
      let am1 = create_am q1 tuples

      and am2 = create_am q3 tuples
      and am3 = create_am q4 tuples

      and am4 = create_am q5 tuples
        
      let am_list = [ am1; am2; am3; am4 ]
        
      (*let p = print_mappings am2*)
      let res_rete_network = gen_rete am_list
        
      let (Node (_, res_bm, _)) = execute_rete res_rete_network
        
    end
  in (*in let p = print_bm res_bm*) assert_equal Test.res_bm Test.q_exp_res
  
(* Tests for Irmin backed*)
(* TEST 11:
   
 MAP {    
     ?x, type, Car
     ?x, hasColor, Red   
    }
     
*)


let test11 _ =
  let db = Moana_irmin.S.init tuples in
  let query10 = [ q1; q2 ] and q10_exp_res = [ [ t1; t9 ] ] in
  let res_q10 = Moana_irmin.S.query db query10
  in
    (*let p = print_tuples_list res_q10 in let p1 = print_tuples_list q10_exp_resin*)
    assert_equal q10_exp_res res_q10


let remote_test11_basic lst =
  let db = Moana_irmin.S.init tuples in
  let query10 = lst (* [ q1; q2 ] *) and q10_exp_res = [ [ t1; t9 ] ] in
  let res_q10 = Moana_irmin.S.query db query10
  in
    (*let p = print_tuples_list res_q10 in let p1 = print_tuples_list q10_exp_resin*)
  let result= assert_equal q10_exp_res res_q10 in
  "\n remote_test11_basic Pass \n"


let remote_test11 qryLst tupLst=
  let db = Moana_irmin.S.init tuples in
  let query10 = qryLst and q10_exp_res = [ tupLst ] in
  let res_q10 = Moana_irmin.S.query db query10
  in
    (*let p = print_tuples_list res_q10 in let p1 = print_tuples_list q10_exp_resin*)
  let result= assert_equal q10_exp_res res_q10 in
  "\n remote_test11 Pass \n"

  
(* Test 12 *)
(* Tests the add function and to_list function*)
let test12 _ =
  let db = Moana_irmin.S.init [ t1; t2 ] in
  let exp_res11 = [ t1; t2; t3 ] in
  let res11 = Moana_irmin.S.add db t3
  in assert_equal exp_res11 (Moana_irmin.S.to_list res11)
 
(* Test 12 *)
(* Tests the add function and to_list function*)
let remote_test12 tupLst1 tupLst2 =
  let db = Moana_irmin.S.init tupLst1 in
  let exp_res11 = tupLst2 in
  let res11 = Moana_irmin.S.add db (List.nth tupLst2 2)  in  (* t3 *)
  let result= assert_equal exp_res11 (Moana_irmin.S.to_list res11) in
  "\n remote_test12 Pass \n"
 
(*** TEST 13: 
   
This is a standing query test. We first construct a rete network.
Ten we add a new tuple which triggers an activation and updates the final node 
of the network with the current result.
 
 MAP  {    
     ?x, type, Car
     ?x, hasColor, ?y
     ?y, type, Color
     ?y, rgbValue, White
    }
***)
let test13 _ =
  let module Test =
    struct
      open Rete
        
      let exd_tuples = [ t13; t14 ] @ tuples
        
      let q_exp_res =
        {
          solutions =
            [ ("?y", ((Constant "c"), [ t7; t5; t2; t1 ]));
              ("?y", ((Constant "c2"), [ t15; t14; t13; t12 ])) ];
        }
        
      let am1 = create_am q1 exd_tuples

      and am2 = create_am q3 exd_tuples
      and am3 = create_am q4 exd_tuples

      and am4 = create_am q5 exd_tuples
        
      let am_list = [ am1; am2; am3; am4 ]
        
      (*let p = print_mappings am2*)
      let rete_network = gen_rete am_list
        
      let (Node (_, res_bm, _)) = let n = add rete_network t12 in add n t15
        
			(*let p1 = Rete.print_mappings am4	*)
				
    end
  in  assert_equal Test.res_bm Test.q_exp_res
  
(*** TEST 14:   
Testing simple parser - create t1 tuple from a string

T1:    
		subj = Constant "a";
    pred = Constant "type";
    obj = Constant "Car";
    ctxt = Constant "context";
    time_stp = None;
    sign = None;
		
***)
let test14 _ =
  let res_tuple = to_tuple "(a,type,Car,context)"
  in (*let p = print_bm res_bm in*) assert_equal res_tuple t1
 
let remote_test14 strTup =
  let res_tuple = to_tuple strTup in (* "(a,type,Car,context)" *)
  let result= (* let p = print_bm res_bm in *) assert_equal res_tuple t1 in
  "\n remote_test14  Pass \n"


 
(*** TEST 15:   
Testing simple parser - create q4 tuple from a string

The difference to Test 14, q3 tuple contains variable elements
 
Q3:    
    subj = Variable "?x";
    pred = Constant "hasColor";
    obj = Variable "?y";
    ctxt = Constant "context";
    time_stp = None;
    sign = None;
        
***)
let test15 _ =
  let res_tuple = to_tuple "(?x,hasColor,?y,context)"
  in (*let p = print_bm res_bm in*) assert_equal res_tuple q3
  
(* TEST 16:
   
Convert query string to list of query tuples
 	
 MAP {    
     ?x, type, Car
     ?x, hasColor, Red   
    }
     
*)
let test16 _ =
  let res =
    str_query_list
      "MAP {
				?x, type, Car, context  
	  		?x, hasColor, Red, context   
    	 }"

  and exp_res = [ q1; q2 ] in assert_equal res exp_res
  
(* TEST 17:
   
Convert strings of tuples to list tuple  objects
     
*)
let test17 _ =
  let res =
    to_tuple_lst
      "{(a,type,Car,context) 
			 (a,hasColor,c,context) 
			 (b,type,Chair,context)
			 (b,hasColor,c,context)}"
  and exp_res = [t1; t2; t3; t4 ] in assert_equal res exp_res

(*** TEST 18: 
   
Accepts query string and creates a Rete network. The last BM in the network contains 
the matching tuples to the query: 
 
 MAP  {    
     ?x, type, Car
     ?x, hasColor, ?y
     ?y, type, Color
     ?y, rgbValue, White  
    }
***)
let test18 _ =
  let module Test =
    struct
      open Rete
      let q_exp_res =
        { solutions = [ ("?y", ((Constant "c"), [ t7; t5; t2; t1 ])) ]; }        
		  let res_rete_network = 
				to_rete "MAP{    
                 ?x, type, Car, context
                 ?x, hasColor, ?y, context
                 ?y, type, Color, context
                 ?y, rgbValue, White, context  
    						}" tuples        
      let (Node (_, res_bm, _)) = execute_rete res_rete_network
        
    end
  in  (*let p = Rete.print_bm Test.res_bm in*) assert_equal Test.res_bm Test.q_exp_res



(** Testing Make functor **)
let test19 _ =
  let module G = Moana.Make(Moana_irmin.S) in let graph = G.init tuples in  
  let query10 = [ q1; q2 ] and q10_exp_res = [ t1; t9 ]  in
  let res_q10 = G.map graph query10
  in
    (*let p = print_tuples_list res_q10 in let p1 = print_tuples_list q10_exp_resin*)
    assert_equal q10_exp_res (G.to_list res_q10)


(** Testing Make functor **)
let remote_test19 qryLst tupLst=
  let module G = Moana.Make(Moana_irmin.S) in let graph = G.init tuples in  
  let query10 = qryLst and q10_exp_res = tupLst             in
  let res_q10 = G.map graph query10                         in
    (*let p = print_tuples_list res_q10 in let p1 = print_tuples_list q10_exp_resin*)
  let result=  assert_equal q10_exp_res (G.to_list res_q10) in
  "\n remote_test19 Pass \n"


(** Same test as test 19, different backend store **)
let test20 _ =
  let module G = Moana.Make(Moana_lists.S) in let graph = G.init tuples in  
  let query10 = [ q1; q2 ] and q10_exp_res =  [ t9; t1 ]  in
  let res_q10 = G.map graph query10
  in
    (*let p = print_tuples_list res_q10 in let p1 = print_tuples_list q10_exp_resin*)
    assert_equal q10_exp_res (G.to_list res_q10)

(** Same test as test 19, different backend store **)
let remote_test20 qryLst tupLst=
  let module G = Moana.Make(Moana_lists.S) in let graph = G.init tuples in  
  let query10 = qryLst and q10_exp_res = tupLst  in
  let res_q10 = G.map graph query10 in
    (*let p = print_tuples_list res_q10 in let p1 = print_tuples_list q10_exp_resin*)
  let result=  assert_equal q10_exp_res (G.to_list res_q10) in
  "\n remote_test20 Pass \n"


(*** TEST 21: 
   
This is a standing query test. We first construct a rete network.
Then populate the data network with ne wtuples
of the network with the current result.
 
 MAP  {    
     ?x, type, Car
     ?x, hasColor, ?y
     ?y, type, Color
     ?y, rgbValue, White
    }
***)
let test21 _ =
  let module Test =
    struct
      open Rete
        
      let exd_tuples = [ t13; t14 ; t12; t15] @ tuples
        
      let q_exp_res =
        {
          solutions =
            [ ("?y", ((Constant "c"), [ t7; t5; t2; t1 ]));
              ("?y", ((Constant "c2"), [ t15; t14; t13; t12 ])) ];
        }
        
      (*let am1 = create_am q1 []

      and am2 = create_am q3 []
      and am3 = create_am q4 []

      and am4 = create_am q5 []
        
      let am_list = [ am1; am2; am3; am4 ]
        
      (*let p = print_mappings am2*)
      let rete_network = gen_rete am_list*)
			let rete_network = (List.map (fun q -> create_am q [])  [q1;q3;q4;q5]) |> gen_rete
        
      let (Node (_, res_bm, _)) = add_tuples rete_network  exd_tuples
        
			(*let p1 = Rete.print_bm res_bm*)
				
    end
  in  assert_equal Test.res_bm Test.q_exp_res


		
let test22 _ =
  let query = [ q1; q2 ] and q_exp_res =  [ t9; t1 ]  in
	let module G = Moana.Make(Moana_rete.S) in let graph = G.init ~query tuples in  
  let res_q10 = G.map graph query
  in
    assert_equal q_exp_res (G.to_list res_q10)


let remote_test22_basic lst =
  let query = lst and q_exp_res =  [ t9; t1 ]             in
  let module G = Moana.Make(Moana_rete.S)                 in 
  let graph = G.init ~query tuples                        in  
  let res_q10 = G.map graph query                         in
  let result=  assert_equal q_exp_res (G.to_list res_q10) in
  "\n remote_test22_basic Pass \n"



let remote_test22 qryLst tupLst=
  let query = qryLst and q_exp_res =  tupLst              in
  let module G = Moana.Make(Moana_rete.S)                 in 
  let graph = G.init ~query tuples                        in  
  let res_q10 = G.map graph query                         in
  let result=  assert_equal q_exp_res (G.to_list res_q10) in
  "\n remote_test22 Pass \n"


(*let test21 _ =
	let query10 = [ q1; q2 ] and q10_exp_res = [ [ t1; t9 ] ] in
  let module G = Moana.Make(Moana_rete.S) in let graph = G.init tuples in    
  let res_q10 = G.map graph query10
  in
    (*let p = print_tuples_list res_q10 in let p1 = print_tuples_list q10_exp_resin*)
    assert_equal q10_exp_res res_q10  *)                              


(*
 this is just a test
 *)

let bounceStr (s:string):string= s

(*															
let suite =
  "Unit tests" >:::
    [ "test0" >:: test0; "test1" >:: test1; "test2" >:: test2;
      "test3" >:: test3; "test4" >:: test4; "test5" >:: test5;
      "test6" >:: test6; "test7" >:: test7; "test8" >:: test8;
      "test9" >:: test9; "test10" >:: test10; "test11" >:: test11;
      "test12" >:: test12; "test13" >:: test13; "test14" >:: test14;
      "test15" >:: test15; "test16" >:: test16;"test17" >:: test17;
			"test18" >:: test18;"test19" >:: test19;"test20" >:: test20; 
			"test21:" >:: test21; "test22:" >:: test22 ]
  
let _ = run_test_tt_main suite

*)  

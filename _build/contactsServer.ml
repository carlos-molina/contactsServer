(* 
 @Program: server_rcontacts.ml
 @author: Carlos Molina-Jimenez
 @date: 26 Feb 2015, Computer Laboratory, Univ. of Cambridge

 @sources: 
  1) I copied the server from
     https://github.com/mirage/ocaml-cohttp/blob/master/examples/async/hello_world.ml
  2) I copied the moana files from 
     https://github.com/yansh/MoanaML/blob/master/moana_irmin.ml
  3) I copied irmin from
     https://github.com/mirage/irmin
            
 @compilation: build.sh

 @Execution: % server_rcontacts.byte
             Listening for HTTP on port 8080
             Try: 'curl http://localhost:8080/printcontacts?repositoryname=noname'
             
             % curl http://localhost:8080/printcontacts?repositoryname=noname
             < a fn Jon >
             < a last Crowcroft > 
             ...
             No more contacts                 

*)

open Core.Std
open Async.Std 
open Cohttp_async


let handler ~body:_ _sock req =
  let uri = Cohttp.Request.uri req in match Uri.path uri with

  | "/ping_contactsServer" -> Uri.get_query_param uri "key"
    |> Option.map ~f:(fun str-> Rcontacts.bounceStr str)
    |> Option.value ~default:"ping_rcontacts: no param str name supplied"
    |> Server.respond_with_string 

  | "/getContactDataByfirstName" -> Uri.get_query_param uri "firstName"
    |> Option.map ~f:(fun str-> Rcontacts.getContactDataByFirstName str)
    |> Option.value ~default:"getContactDataByFirstName: wrong param supplied"
    |> Server.respond_with_string 

  | "/getContactDataBylastName" -> Uri.get_query_param uri "lastName"
    |> Option.map ~f:(fun str-> Rcontacts.getContactDataByLastName str)
    |> Option.value ~default:"getContactDataByLastName: wrong param supplied"
    |> Server.respond_with_string 

  | "/getContactDataByemail" -> Uri.get_query_param uri "email"
    |> Option.map ~f:(fun str-> Rcontacts.getContactDataByEmail str)
    |> Option.value ~default:"getContactDataByEmail: wrong param supplied"
    |> Server.respond_with_string 

  | "/getContactDataByfirstNamePlcy" -> Uri.get_query_param uri "requestorName" 
    |> fun someOrNone -> let requestorName= match someOrNone with
       | None      -> raise (Failure "requestor's name empty") 
       | Some name -> name 
       in requestorName 
    |> fun _          -> let firstName= match Uri.get_query_param uri "firstName" with 
       | None         -> raise (Failure "first name is empty") 
       | Some name    -> name 
       in Some (requestorName, firstName)
    |> Option.map ~f:(fun rn_fn ->
               Rcontacts.getContactDataByFirstNamePlcy (fst(rn_fn)) (snd(rn_fn))) 
    |> Option.value ~default:" getContactDataByfirstNamePlcy: No parammeters supplied"
    |> Server.respond_with_string 


  | _ ->
    Server.respond_with_string ~code:`Not_found "Route not found"

let start_server port () =
  eprintf "contactsServer Listening for HTTP on port %d\n" port;
  eprintf "Try: 'curl http://localhost:%d/ping_contactsServer?key=anyString' or run clienthtml.byte\n%!" port; 

  Cohttp_async.Server.create ~on_handler_error:`Raise
    (Tcp.on_port port) handler
  >>= fun _ -> Deferred.never ()

let () =
  Command.async_basic
    ~summary:"Start an Async server that reponds to irmin request filetered by Moana"
    Command.Spec.(empty +>
      flag "-p" (optional_with_default 8080 int) 
        ~doc:"int Source port to listen on"
    ) start_server

  |> Command.run



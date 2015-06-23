#ContactsServer
==============

ContactsServer is a contacts application written in OCaml and
with Moana (see yansh/MoanaML)!](https://github.com/yansh/MoanaML)
support for enforcing access control policies.

## Installation
1. Clone this repo.

2. % build.sh (* compilation *) 

   The compilation produces _contactsServer.byte_
   and _clienthtml.byte_.


## Server execution
The server is a conventional hppt server that
that listens to requests on _http://localhost:8080_.

% ./contacts_server.byte (* run the server *)


## Place queries against the server

To place a request against the server you can either
* run the _clienthtml.byte_
* run _curl_ 


### client.byte
The client comes with several encoded requests that it
places against the server.

### curl
You can use curl to place request directly agains the server.

* To query by first name, use the 
 _getContactDataByfirstName_ path. The  _firstName_ key 
can take Jon, Carlos, Amir and Richar as
values.

* To query by last name, use the 
 _getContactDataBylastName_ path. The  _lastName_ key 
can take Crowcroft, Chaudhry, Madhavapeddy, Molina-jimenez,
and Mortier as values.

* See examples:

1. $ curl http://localhost:8080/getContactDataByfirstName?firstName=Jon
2. $ curl http://localhost:8080/getContactDataByfirstName?firstName=""
3. $ curl http://localhost:8080/getContactDataBylastName?lastName=Crowcroft
4. $ curl http://localhost:8080/getContactDataBylastName?lastName=""

## Request processing
1. The http request is received by the server.

2. The server calls a moana layer placed between the server
   and the actual contacts repository.
   The main resposibility of moana is to enforce
   access control policies that contrain the visibility of 
   the contacts records.

3. The response (retrieved records) is sent by the server to the client. 

## SPARql queries, SELECT variable and access control policies
*  Moana accepts queries in SPARql syntax. Thus the http request
   is converted to a SPARql query by the server before presenting
   it to moana. The following is an example of a SPARql
   query accepted by moana. It is written in OCaml.

          let query =  "MAP {
              ?y, fn,      Jon,      contacts 
              ?y, last,    ?last,    contacts 
              ?y, email,   ?email,   contacts 
             }" 

The query shown above can be placed to retrive, respectively,
the last name and email of the  contact whose first name is Jon. 

* SPARql uses SELECT variables that constraint the data that can
be retrived by a query. The server uses the following
**variables by default**. In the current version this variable
can be found in the _contactsdata.ml_ file: 

           let varsLstDefault= ["?fn"; "?last"; "?email"; "?title"; "?mobile"] 

* The current version of the server includes the following
  **deafult access control** which is encoded in the _contactsdata.ml_ file.  

           let defaultPlcy =
           "MAP {
           ?xbody,  canView,         ?ybody,      policies
           ?ybody,  ?anypred,        ?anyobj,     contacts
        }" 


As it is, the default policy grants the client unrestricted 
access to the contacts records: everything is visible.


## Manipulation of access control policies
With the current version of the server, variables and policies
(including default ones) are encoded in the _contactsdata.ml_ 
file.  This is only a temporary solution as their 
alteration requires the recompilation of the server.

The following examples, show the use of specific polices
imposed on the requestor that contrain the visibility
of the contacts records.

### Access contrain based on requestor's name

            $ curl http://localhost:8080/getContactDataByfirstNamePlcy?requestorName=Jon\&firstName=Carlos

In the requests shown above, the http request includes the requestor's name (Jon) in its lists
of parameters. Upon receiving the request, moana deploys and enforces the corresponding
policy. Complex access policies can be specified with the help of booleam formula on the
predicates (knows, email, title, fn, etc.).

For example, the following policy contrains the visibility to only the last
name of contacts that know Jon.

            let jonPlcy =
             "MAP {
                   a, canView, ?x, policies
                   ?x, knows,  Jon, contacts
                   ?x, last,   ?last, contacts     
                  }"

Running examples will be shown in the next release--after fixing some bugs.

### SPARql queries 
The complexity of the SPARql query is determined by the application. The server can
receive requests in json format and respond similarly.

Running examples will be shoen in the next release. 



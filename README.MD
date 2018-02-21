#ContactsServer
==============

ContactsServer is a contacts application written in the OCaml 
[(OCaml)](https://ocaml.org) 
Functional Programming Language.
with Moana [(see yansh/MoanaML)!](https://github.com/yansh/MoanaML)
support for enforcing access control policies.

## 1. Installation
1. Clone this repository.

2. % build.sh (* compilation *) 

   The compilation produces _contactsServer.byte_
   and _clienthtml.byte_.

## Server execution
The server is a conventional hppt server that
that listens to requests on _http://localhost:8080_.

% ./contacts_server.byte (* run the server *)


## 2. Place queries against the server

To place a request against the server you can either
* run the _clienthtml.byte_
* run _curl_ 


### clienthtml.byte
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

## 3. Request processing
1. The http request is received by the server.

2. The server calls a moana layer placed between the server
   and the actual contacts repository.
   The main resposibility of moana is to enforce
   access control policies that contrain the visibility of 
   the contacts records.

3. The response (retrieved records) is sent by the server to the client. 

## 4. Queries, SELECT variable and access control policies
*  Moana accepts queries presented as graph queries. The syntax is 
   similar to the standard SPARQL. Thus the http request
   is converted to a SPARQL-like query by the server before presenting
   it to moana. The following is an example of a graph 
   query accepted by moana. It is written in OCaml.

          let query =  "MAP {
              ?y, fn,      Jon,      contacts 
              ?y, last,    ?last,    contacts 
              ?y, email,   ?email,   contacts 
             }" 

The query shown above can be placed to retrive, respectively,
the last name and email of the  contact whose first name is Jon. 

The complexity of the graph query is determined by the application. The server can
receive requests in json format and respond similarly. We will show running examples 
in subsequent sections. 

* Moana graph queries use SELECT variables that constraint the data that can
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


## 5. Manipulation of access control policies
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
policy (the _jonPlcy_ shown below). Complex access policies can be specified with the help of booleam formula on the
predicates (knows, email, title, fn, etc.).

For example, the following policy contrains the visibility to only the last
name and email of contacts that know Jon.

            let jonPlcy =
             "MAP {
                   a, canView, ?x, policies
                   ?x, knows,  a, contacts
                   ?x, last,   ?last, contacts     
                   ?x, email,  ?email, contacts     
                  }"

Running examples will be shown in the next release--after fixing some bugs in the policies.

## 6. Significance of access control policies
Conceptually the control policies are equivalent to
to the conventional views that users can create in
relational databases with the help of the _CREATE VIEW_ 
command. Control policies create a virtual repository
(a projection) that includes only the records that that requestor is
allowed to query.

The power of control policies to contrain the visibility of
the contact records is determined by the expressiveness of the query
language. Currently, moana supports only some of the constructs
of the [SPARQL 1.1 Query Language](http://www.w3.org/TR/sparql11-query/#QSynTriples).
For intance, SPARQL 1.1  includes constructs such as _OPTIONAL_, _UNION_ and
_FILTER_ that is not supported yet.
 

For instance, the _UNION_ construct can help build a policy
for roducing a projection that includes only the last name 
and email of people that know Jon directly and of people that know 
Jon indirectly (they know somebody
that knows Jon).
 
            let jonPlcy =
             "MAP {
                   {
                    a,  canView, ?x,     policies
                    ?x,  knows,  a,     contacts
                    ?x, ?last,   ?last,  contacts
                    ?x, email    ?email, contacts
                   }
                   union
                   {
                    a,  canView, ?x,     policies
                    ?x, knows,   ?z,     contacts
                    ?z, knows,   a,      contacts
                    ?x, ?last,   ?last,  contacts
                    ?x, email    ?email, contacts
                   }
                  }"

One can argue that similar access control can be achived with
the help of the _CREATE VIEW_, _GRANT_ and _ROLE_ constructs
found in relational databases. This is correct but the wrong
argument.

The strength of moana is not on the query language but
on the implementation of the access control policy and the
encoding of the parameters (attributed associated with
subjects) involved in the policy. 

### Attribute Based Access Control (ABAC)
Moana follows the ABAC (Attribute Based Access Control) 
[(see Attribute-Based Systems)](https://seclab.illinois.edu/attribute-based-systems)
paradigm. Thus the strength of the approach lies on 

+ It is supported by a graph datamodel where the entities 
  are related to each other by attributes of different 
  nature (static, dynamic, context-related like time, date, current physical location, IP subnet, etc.)
  like those defined in the [FOAF Vocabulary](http://xmlns.com/foaf/spec/#term_knows). 
   For example,
   _Bob knows Alice_, _Alice title Dr_, _Bob currentLocation Tokyo_, 
   _Alice currentProject epsrcDocument_, etc. 

+  Moana is attribute centric: moana data model intuitively captures assertions 
  about facts that can be evaluated to true or false. This feature can
  be used to build attribute based access control mechanisms in a very
  intuitive manner. 

+  Attributes are used directly in the graph queries
  to compose policies for producing **standing queries**
  that contraint the access to the records. The standing
  queries are dynamic projection of the records. The
  projection is updated when the attributes (for example
  current location) change.

+  Attributed can be associated to both resources and requesters. The
  latter can be used for implementing attribute based authentication
  mechanisms that can help support interactions between people
  with no history of interactions. 

+  It places the user in direct control of its privacy. He or she
  is responsible for providing its entities with the right
  attributed and for imposing his policies without the
  need to worry about the maintenance (creation, updates,
  deletion) of roles and permissions.

 
## 7. Weaknesses of ABAC
Coming soon... 







#!/bin/bash

eval `opam config env`

rm *.byte

ocamlbuild -r -use-ocamlfind -use-menhir -classic-display -package core,async,cohttp.async,lwt,yojson,atdgen,git.unix,irmin,irmin.unix,oUnit,sexplib.syntax,comparelib.syntax,bin_prot.syntax -tag thread  tests.byte examples/contacts.byte clienthtml.byte contactsServer.byte -cflags -annot

#
#./tests.byte
#./contacts.byte
#
#

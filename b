#!/bin/bash

ocamlbuild -classic-display test_uf.byte
ocamlbuild -classic-display test_uf.native
ocamlbuild -classic-display test_stk.byte
ocamlbuild -classic-display test_stk.native
ocamlbuild -classic-display test_que.byte
ocamlbuild -classic-display test_que.native
ocamlbuild -classic-display test_bst.byte
ocamlbuild -classic-display test_bst.native
ocamlbuild -classic-display test_jsort.byte
ocamlbuild -classic-display test_jsort.native


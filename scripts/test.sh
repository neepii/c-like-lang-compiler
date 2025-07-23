#!/bin/sh

dune runtest test/cram1.t
dune exec test_qcheck
dune runtest

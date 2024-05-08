#!/bin/sh
acme -Wtype-mismatch cbm2diag.b
diff -s "cbm2diag.bin" "original/324835-01.bin"
cmp "cbm2diag.bin" "original/324835-01.bin"
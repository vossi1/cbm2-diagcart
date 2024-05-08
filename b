#!/bin/sh
acme -Wtype-mismatch -r cbm2diag.lst -l cbm2diag.sym cbm2diag.b
#acme -r cbm2diag.lst -l cbm2diag.sym cbm2diag.b
diff -s "cbm2diag.bin" "original/324835-01.bin"
cmp "cbm2diag.bin" "original/324835-01.bin"
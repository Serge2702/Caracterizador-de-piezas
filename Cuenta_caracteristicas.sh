#!/bin/bash

a1=$(grep "(0 +)" $1 | wc -l)
a2=$(grep "(0 -)" $1 | wc -l)
b1=$(grep "(1 +)" $1 | wc -l)
b2=$(grep "(1 -)" $1 | wc -l)
c1=$(grep "(2 +)" $1 | wc -l)
c2=$(grep "(2 -)" $1 | wc -l)
d1=$(grep "(3 +)" $1 | wc -l)
d2=$(grep "(3 -)" $1 | wc -l)
e1=$(grep "(4 +)" $1 | wc -l)
e2=$(grep "(4 -)" $1 | wc -l)
f1=$(grep "(5 +)" $1 | wc -l)
f2=$(grep "(5 -)" $1 | wc -l)

echo "(list (list $a1 $a2)(list $b1 $b2)(list $c1 $c2)(list $d1 $d2)(list $e1 $e2)(list $f1 $f2))"

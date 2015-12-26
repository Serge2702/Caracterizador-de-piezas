#!/bin/bash
# Cuenta las características de todas las propiedades encontradas que están en
# la carpeta de resultados.

for archivo in Resultados/*
do
a1=$(grep "(0 +)" $archivo | wc -l)
a2=$(grep "(0 -)" $archivo | wc -l)
b1=$(grep "(1 +)" $archivo | wc -l)
b2=$(grep "(1 -)" $archivo | wc -l)
c1=$(grep "(2 +)" $archivo | wc -l)
c2=$(grep "(2 -)" $archivo | wc -l)
d1=$(grep "(3 +)" $archivo | wc -l)
d2=$(grep "(3 -)" $archivo | wc -l)
e1=$(grep "(4 +)" $archivo | wc -l)
e2=$(grep "(4 -)" $archivo | wc -l)
f1=$(grep "(5 +)" $archivo | wc -l)
f2=$(grep "(5 -)" $archivo | wc -l)

echo "((equal nombre_archivo \"$archivo\")" >> Conteo_de_propiedades.lisp
echo "(defparameter *conteo_caracteristicas*" >> Conteo_de_propiedades.lisp
echo "(list (list $a1 $a2)(list $b1 $b2)(list $c1 $c2)(list $d1 $d2)(list $e1 $e2)(list $f1 $f2))))" >> Conteo_de_propiedades.lisp
done

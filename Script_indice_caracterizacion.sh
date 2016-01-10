#!/bin/bash
# Este script se usa para calcular automáticamente el índice de caracterización
# de todos los archivos de propiedades.

for archivo in Resultados/*.txt
do
    echo "$archivo" >>  Resultados/Indices.txt
    sbcl --script "indice_caracterizacion.lisp" "$archivo" >> Resultados/Indices.txt
done

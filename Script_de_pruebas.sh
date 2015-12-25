#!/bin/bash
# Este script es para determinar los mejores valores de alfa y beta para la
# caracterización de las piezas.

while read linea
do
    alfa=$(echo $linea | cut -f1 -d' ')
    beta=$(echo $linea | cut -f2 -d' ')
    echo "================================================================================"
    echo "alfa: $alfa y beta: $beta"
    sbcl --script "Script_caracterizador_de_piezas.lisp" $alfa $beta
done < "Archivos_mascaras/Pares_de_numeros.txt"

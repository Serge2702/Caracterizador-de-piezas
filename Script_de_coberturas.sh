#!/bin/bash
# Este script se usa automatizar el proceso del cálculo de las cobertura.

for archivo in Resultados/*.txt
do
    numeros=$(cat $archivo | grep -o '[0-9]\+-[0-9]\+')
    alfa=$(cut -d'-' -f1)
    beta=$(cut -d'-' -f2)
    echo "$archivo: $alfa - $beta"
    echo "$archivo" >> Resultados/Calculo_coverturas.txt
    sbcl --script Cobertura.lisp "$archivo" $alfa $beta >> Resultados/Calculo_coverturas.txt
done

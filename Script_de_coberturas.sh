#!/bin/bash
# Este script se usa automatizar el proceso del cálculo de las cobertura.

for archivo in Resultados/*
do
    echo "$archivo" >> Resultados/Calculo_coverturas.txt
    sbcl --script Cobertura.lisp "$archivo" >> Resultados/Calculo_coverturas.txt
done

#!/bin/bash
# Este script realiza la clasificación automática de todas las piezas usando
# todos los archivos de propiedades encontradas.

for archivo in Resultados/*
do
    sbcl --script "Script_clasificador_universal.lisp" "$archivo" >> Resultados/Clasificacion_"$(basename $archivo)"
done

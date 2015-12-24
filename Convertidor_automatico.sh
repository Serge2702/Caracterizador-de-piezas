#!/bin/bash
# Este script convierte los patrones por frase a patrones por pieza usando el
# otro código en common lisp. Pero esto lo imprime a un archivo para que sea por
# artista.
# Este Script debe ejecutarse directamente en la carpeta donde está contenido.
# El argumento que recibe es la carpeta del artista. Esta debe ser relativa a la
# posición de este script.

for directorio in $1/*/
do
    echo $directorio
    sbcl --script "De_frases_a_piezas.lisp" "$directorio" >> "$1"/Patrones_piezas.txt
done



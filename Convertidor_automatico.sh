#!/bin/bash
# Este script convierte los patrones por frase a patrones por pieza usando el
# otro código en common lisp. Pero esto lo imprime a un archivo para que sea por
# artista.
echo $1
for directorio in $1/*/
do
    echo "================================================================================"
    echo $directorio
    #sbcl --script "~/Documentos/Maestría/5to_Semestre/Tesis/Caracterizador de piezas/De_frases_a_piezas.lisp" "$directorio"
    sbcl --script "De_frases_a_piezas.lisp" "$directorio"
done

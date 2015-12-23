#!/bin/bash
#Este archivo añade el número total de líneas a un archivo de texto

for file in *
do
    lineas=$(wc -l $file | cut -f1 -d' ')
    sed -i '1i ;'
    sed -i "1i $lineas"
done

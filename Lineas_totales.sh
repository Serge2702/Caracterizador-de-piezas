#!/bin/bash
#Este archivo añade el número total de líneas a un archivo de texto

for directory in  */
do
    for file in "$directory"*
    do
        lineas=$(wc -l $file | cut -f1 -d' ')
        sed -i '1i ;' $file 
        sed -i "1i $lineas" $file 
    done
done

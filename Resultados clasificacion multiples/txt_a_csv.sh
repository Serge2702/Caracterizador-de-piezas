#!/bin/bash
# Pega los índices del archivo "indices.txt" a los demás archivos, y luego
# les pone extensión csv

for archivo in *.txt
do
    nuevo=$(echo $archivo | sed 's/txt/csv/')
    echo $nuevo
    paste -d, Indices $archivo > $nuevo
done

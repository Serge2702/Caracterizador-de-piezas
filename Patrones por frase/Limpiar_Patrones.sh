#!/bin/sh
sed -i -s -e '/(/!d' -e 's/(/#(/' *.txt
for file in *.txt
do
    lineas=$(wc -l < $file)
    sed -i '1i;' $file
    sed -i "1i$lineas" $file
done

#!/bin/bash
nombre=true
row1=1
while read line
do
    if [ "$nombre" = true ]
    then
        dir_actual=$(basename $line .txt)
        if [ ! -d $dir_actual ]
        then
            mkdir $dir_actual
        fi
        echo $dir_actual
        nombre=false
    else
        longitud=$line
        row2=$(($row1+$longitud-1))
        for file in *.txt
        do
            sed -n "$row1,$row2"p $file > ./$dir_actual/$file
            #echo $file $row1 $longitud $row2
        done
        echo $row1 $longitud $row2
        row1=$(($row2+1))
        nombre=true
    fi
done < Size.md

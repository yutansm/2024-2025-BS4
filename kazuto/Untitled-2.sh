#!bin/bash
#create a function that takes all .ps files. then it passes them to the ps2pdf command to convert them to .pdf files
function ps3pdf {
    for file in *.ps
    do
        ps2pdf $file
    done
}
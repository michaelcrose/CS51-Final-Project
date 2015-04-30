We developed and ran our project inside the CS51 appliance and recommend
running it there. Conceivably it should be compatible with any standard 
OCaml installation on Mac or Windows that includes the Graphics library.
To compile the program, execute the following command in the directory
containing ourlife.ml:

ocamlopt -o life graphics.cmxa ourlife.ml

Then, run the program with

./life 3 23

to run the Game of Life with Conway's original rules: a dead cell becomes
alive if bounded by exactly 3 live cells, a live cell survives if bounded
by 2 or 3 other live cells, and a live cell dies otherwise. Then, you can 
change the rules of the game and the patterns created by altering these two 
values for birth and survival. Two cool rule changes we might suggest are...


./life 1 23
./life 1 34

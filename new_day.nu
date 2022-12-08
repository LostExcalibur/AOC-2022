#!/usr/bin/env nu

dune clean
let day = (ls day* | length) + 1
let name = $"day($day | into string | str lpad -l 2 -c '0')"
cp -r sample $name
cd $name
mv dayN.ml $"day($day).ml"
open dune | str replace -a 'N' $"($day)" | save dune
dune build
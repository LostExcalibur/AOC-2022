#!/usr/bin/env nu

def main [
    day: int
    --sample
    ] {
        let dir = $"day($day | into string | fill -a right -w 2 -c '0')"
        let name = $"day($day | into string)"
        let program = $"($dir)/($name).exe"
        if $sample {
            dune exec $program $"($dir)/sample.txt"
        } else {
            dune exec $program $"($dir)/($name).txt"
        }
    }
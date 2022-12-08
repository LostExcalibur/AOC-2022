#!usr/bin/env nu

ls -la **/*.ml* | get name | each { |it| open $it | wc -l | into int } | reduce { |it, acc| $acc + $it }

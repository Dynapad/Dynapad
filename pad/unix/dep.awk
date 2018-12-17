{a[$1] = a[$1] " " $2}
END {for (v in a) print v, a[v]}

"0:\t1\n1:\t1\n" ->

1
1

0 while dup 100 2 - < do 
    dup 2 + -> ":\t" ->
    rot
    over + dup -> "\n" -> swap
    irot
    1 +
end

pop pop pop
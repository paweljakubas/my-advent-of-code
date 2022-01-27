data=: ". > cutLF (fread jpath './input.txt')

part1=: +/ data

NB. (a) Let's assume we have data
NB.   data
NB. 1 _2 3 1
NB. (b) We are constructing scanl with addition
NB.   ]d=: +/\ data,data
NB. 1 _1 2 3 4 2 5 6
NB. (c) log the occurence of each elements as 1
NB.    =/~ d
NB. 1 0 0 0 0 0 0 0
NB. 0 1 0 0 0 0 0 0
NB. 0 0 1 0 0 1 0 0
NB. 0 0 0 1 0 0 0 0
NB. 0 0 0 0 1 0 0 0
NB. 0 0 1 0 0 1 0 0
NB. 0 0 0 0 0 0 1 0
NB. 0 0 0 0 0 0 0 1
NB. (d) sum cumulatively column wise
NB.    +/\ =/~ d
NB. 1 0 0 0 0 0 0 0
NB. 1 1 0 0 0 0 0 0
NB. 1 1 1 0 0 1 0 0
NB. 1 1 1 1 0 1 0 0
NB. 1 1 1 1 1 1 0 0
NB. 1 1 2 1 1 2 0 0
NB. 1 1 2 1 1 2 1 0
NB. 1 1 2 1 1 2 1 1
NB. (e) we need now to spot first row in which 2 is present
NB. Let's first detect (as 1s) rows where 2 is present
NB.    2 e."1 +/\ =/~ d
NB. 0 0 0 0 0 1 1 1
NB. Now let's calculate indices where 1s are present
NB.    I. 2 e."1 +/\ =/~ d
NB. 5 6 7
NB. We are interested in the first index
NB.    {. I. 2 e."1 +/\ =/~ d
NB. 5
NB.    findIx=: {{ {. I. 2 e."1 +/\ =/~ y }}
NB.    findIx d
NB. 5
NB.    (findIx d) { d
NB. 2

findIx=: {{ {. I. 2 e."1 +/\ =/~ y }}
findFirstDuplicate=: {{ (findIx y) { y }}
NB. it is ok solution for small data (matrix is created from it)
NB. Unfortunately, not ok for part2 - too many cycles are needed and too big resultant matrix
NB. It is better if we don't create matrix.
NB. Below we will use sieve ~: that creates 1s and 0s (if duplicate is met)
NB. then we negate them and ask for indices in vector for 1s
NB. The first index is the one we search for
NB.    d=: 1 2 _4 2
NB.    ]data=. +/\ 0, ,(^:0)~ d
NB. 0 1 3 _1 1
NB.    ]data=. +/\ 0, ,(^:1)~ d
NB. 0 1 3 _1 1 2 4 0 2
NB.    ]data=. +/\ 0, ,(^:2)~ d
NB. 0 1 3 _1 1 2 4 0 2 3 5 1 3
NB.    ]ixs=. I. -. ~: data
NB. 4 7 8 9 11 12

cycle=: 4 : 0
data=. +/\ 0, ,(^:x)~ y
ixs=. I. -. ~: data
if. 0<#ixs do. ({.ixs){data else. (>:x) cycle y end.
)
part2=: 1 cycle data

]part1
]part2
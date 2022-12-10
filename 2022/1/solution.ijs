data=: {{ (".;._2);._2 (y,LF) rplc LF2;LF,'X'}} (fread jpath './input.txt')
NB.    data=: {{ (y,LF) rplc LF2;LF,'X'}} (fread jpath './inputTest1.txt')
NB.    ]data
NB. 1000
NB. 2000
NB. 3000
NB. X4000
NB. X5000
NB. 6000
NB. X7000
NB. 8000
NB. 9000
NB. X10000
NB. X
NB.    {{ <;._2 (y,LF) rplc LF2;LF,'X'}} (fread jpath './inputTest1.txt')
NB. ┌───────────────┬─────┬──────────┬───────────────┬──────┐
NB. │1000 2000 3000 │4000 │5000 6000 │7000 8000 9000 │10000 │
NB. └───────────────┴─────┴──────────┴───────────────┴──────┘
NB.    {{ (".;._2);._2 (y,LF) rplc LF2;LF,'X'}} (fread jpath './inputTest1.txt')
NB.  1000 2000 3000
NB.  4000    0    0
NB.  5000 6000    0
NB.  7000 8000 9000
NB. 10000    0    0

NB. Sum the calory per elf rows and then find max
part1 =: >./ +/"1 data

NB. Sum the calory per elf rows, sort them in ascending order,
NB. take last 3, ie. the biggest ones, and sum them
part2 =: +/ _3 {. /:~ +/"1 data

]part1
]part2
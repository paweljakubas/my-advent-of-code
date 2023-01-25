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

NB. Also using state machine parsing (LF - 1; digit - 2; rest - 0)
NB.    m=: (a.=LF)+2*a.e.'0123456789'
NB.    s0=: +.".>cutLF {{)n
NB.   1j1 2j1 1j1  NB. start here
NB.   1j0 2j0 1j0  NB. non-newline
NB.   1j0 1j2 1j0  NB. newline
NB. }}
NB.
NB.    (0;s0;m;0 _1 0 _1) ;: (fread jpath './inputTest1.txt')
NB. ┌───────────────┬──────┬───────────┬────────────────┬───────┐
NB. │1000 2000 3000 │ 4000 │ 5000 6000 │ 7000 8000 9000 │ 10000 │
NB. └───────────────┴──────┴───────────┴────────────────┴───────┘
NB.
NB.    (<0;s1;m;0 _1 0 _1) ;:&.> (0;s0;m;0 _1 0 _1) ;: (fread jpath './inputTest1.txt')
NB. ┌────────────────┬──────┬───────────┬────────────────┬───────┐
NB. │┌────┬────┬────┐│┌────┐│┌────┬────┐│┌────┬────┬────┐│┌─────┐│
NB. ││1000│2000│3000│││4000│││5000│6000│││7000│8000│9000│││10000││
NB. │└────┴────┴────┘│└────┘│└────┴────┘│└────┴────┴────┘│└─────┘│
NB. └────────────────┴──────┴───────────┴────────────────┴───────┘
NB.
NB.    ]data=: ".&.> >&.> (<0;s1;m;0 _1 0 _1) ;:&.> (0;s0;m;0 _1 0 _1) ;: (fread jpath './inputTest1.txt')
NB. ┌──────────────┬────┬─────────┬──────────────┬─────┐
NB. │1000 2000 3000│4000│5000 6000│7000 8000 9000│10000│
NB. └──────────────┴────┴─────────┴──────────────┴─────┘
NB.
NB.    ]part1=: >./ > +/&.> data
NB. 24000
NB.
NB. ]part2 =: +/ _3 {. /:~ > +/&.> data
NB. 45000


NB. Sum the calory per elf rows and then find max
part1 =: >./ +/"1 data

NB. Sum the calory per elf rows, sort them in ascending order,
NB. take last 3, ie. the biggest ones, and sum them
part2 =: +/ _3 {. /:~ +/"1 data

]part1
]part2
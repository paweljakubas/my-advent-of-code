   sample1=: fread jpath '/home/pawel/Work/Personal/my-advent-of-code/2023/1/sample1.txt'
   input=: fread jpath '/home/pawel/Work/Personal/my-advent-of-code/2023/1/input.txt'
   
NB.    sample1
NB. 1abc2
NB. pqr3stu8vwx
NB. a1b2c3d4e5f
NB. treb7uchet

   parse=: {{". ({. , {:) y ([-. -.) '0123456789'}};._2
   
NB.    parse sample1
NB. 12 38 15 77

   part1=. {{ +/ parse y }}
   
   part1 sample1
NB. 142

   part1 input
NB. 55488

   alterStr=: ;:'one o1e two t2o three t3e four f4r five f5e six s6x seven s7n eight e8t nine n9e'
   
NB.    alterStr
NB. ┌───┬───┬───┬───┬─────┬───┬────┬───┬────┬───┬───┬───┬─────┬───┬─────┬───┬────┬───┐
NB. │one│o1e│two│t2o│three│t3e│four│f4r│five│f5e│six│s6x│seven│s7n│eight│e8t│nine│n9e│
NB. └───┴───┴───┴───┴─────┴───┴────┴───┴────┴───┴───┴───┴─────┴───┴─────┴───┴────┴───┘

   sample2=: fread jpath '/home/pawel/Work/Personal/my-advent-of-code/2023/1/sample2.txt'
   
NB.    sample2
NB. two1nine
NB. eightwothree
NB. abcone2threexyz
NB. xtwone3four
NB. 4nineeightseven2
NB. zoneight234
NB. 7pqrstsixteen

   rplc&alterStr sample2
NB. t2o1n9e
NB. eight2ot3e
NB. abco1e2t3exyz
NB. xtwo1e3f4r
NB. 4n9ee8ts7n2
NB. zo1eight234
NB. 7pqrsts6xteen

NB. ^ Second line needs another round to make it right

   rplc&alterStr^:2 sample2
NB. t2o1n9e
NB. e8t2ot3e
NB. abco1e2t3exyz
NB. xt2o1e3f4r
NB. 4n9ee8ts7n2
NB. zo1e8t234
NB. 7pqrsts6xteen

   part2=: {{part1 rplc&alterStr^:2 y}} 
NB.    part2 sample2
NB. 281

   part2 input
NB. 55614

   sample3=: fread jpath '/home/pawel/Work/Personal/my-advent-of-code/2023/1/sample3.txt'
   part2 sample3
NB. 607
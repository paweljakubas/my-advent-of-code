   isColor=: {{ 0: ` (*/ @: (x&=)) @. ((#x)& = @ #) y }}
   
NB.    'red' isColor 'green'
NB. 0
NB.    'red' isColor 'gre'
NB. 0
NB.    'red' isColor 'red'
NB. 1

   toColor=: 3 : 0
'num col'=. ;: y
if. 'red' isColor col do.
  (". num), 0, 0
elseif. 'blue' isColor col do.
  0, (". num), 0
elseif. 'green' isColor col do.
  0, 0, (". num)
else.
  0, 0, 0
end.
)

NB.    toColor '1 green'
NB. 0 0 1
NB.    toColor '22 red'
NB. 22 0 0
NB.    toColor '333 blue'
NB. 0 333 0
NB.    toColor '333 black'
NB. 0 0 0
NB. 
NB.    <;._2 'Game 1: 3 blue, 4 red; 1 red, 2 green, 6 blue; 2 green',':'
NB. ┌──────┬───────────────────────────────────────────────┐
NB. │Game 1│ 3 blue, 4 red; 1 red, 2 green, 6 blue; 2 green│
NB. └──────┴───────────────────────────────────────────────┘
NB. 
NB.    processLine=: 3 : 0
NB. 'game cubes'=. <;._2 y ,':'
NB. 'str gameNumStr'=. ;: game
NB. tosses=. <;._2 cubes , ';'
NB. (". gameNumStr);tosses
NB. )
NB.    processLine 'Game 1: 3 blue, 4 red; 1 red, 2 green, 6 blue; 2 green'
NB. ┌─┬──────────────┬───────────────────────┬────────┐
NB. │1│ 3 blue, 4 red│ 1 red, 2 green, 6 blue│ 2 green│
NB. └─┴──────────────┴───────────────────────┴────────┘
NB. 
NB.    >"0 toColor&.> <;._2 '1 red, 2 green, 6 blue' ,','
NB. 1 0 0
NB. 0 0 2
NB. 0 6 0
NB.    +/ >"0 toColor&.> <;._2 '1 red, 2 green, 6 blue' ,','
NB. 1 6 2
NB.    {{ +/ >"0 toColor&.> <;._2 y , ',' }} '1 red, 2 green, 6 blue'
NB. 1 6 2
NB. 
NB.    processLine=: 3 : 0
NB. 'game cubes'=. <;._2 y ,':'
NB. 'str gameNumStr'=. ;: game
NB. tosses=. <;._2 cubes , ';'
NB. (". gameNumStr); {{ +/ >"0 toColor&.> <;._2 y , ',' }} &.>tosses
NB. )
NB.    processLine 'Game 1: 3 blue, 4 red; 1 red, 2 green, 6 blue; 2 green'
NB. ┌─┬─────┬─────┬─────┐
NB. │1│4 3 0│1 6 2│0 0 2│
NB. └─┴─────┴─────┴─────┘

   processLine=: 3 : 0
'game cubes'=. <;._2 y ,':'
'str gameNumStr'=. ;: game
tosses=. <;._2 cubes , ';'
(". gameNumStr); >"0 {{ +/ >"0 toColor&.> <;._2 y , ',' }} &.>tosses
)

NB.    processLine 'Game 1: 3 blue, 4 red; 1 red, 2 green, 6 blue; 2 green'
NB. ┌─┬─────┐
NB. │1│4 3 0│
NB. │ │1 6 2│
NB. │ │0 0 2│
NB. └─┴─────┘
NB. 
NB.    checkGame=: 3 : 0
NB. {{ */ y <: limit }}"1 y
NB. )
NB.    1{processLine 'Game 1: 3 blue, 4 red; 1 red, 2 green, 6 blue; 2 green'
NB. ┌─────┐
NB. │4 3 0│
NB. │1 6 2│
NB. │0 0 2│
NB. └─────┘
NB.    >1{processLine 'Game 1: 3 blue, 4 red; 1 red, 2 green, 6 blue; 2 green'
NB. 4 3 0
NB. 1 6 2
NB. 0 0 2
NB.    checkGame >1{processLine 'Game 1: 3 blue, 4 red; 1 red, 2 green, 6 blue; 2 green'
NB. 1 1 1

   checkGame=: 3 : 0
*/ {{ */ y <: limit }}"1 y
)

NB.    checkGame >1{processLine 'Game 1: 3 blue, 4 red; 1 red, 2 green, 6 blue; 2 green'
NB. 1

   limit=: 12,14,13
   
   sample=: {{)n
Game 1: 3 blue, 4 red; 1 red, 2 green, 6 blue; 2 green
Game 2: 1 blue, 2 green; 3 green, 4 blue, 1 red; 1 green, 1 blue
Game 3: 8 green, 6 blue, 20 red; 5 blue, 4 red, 13 green; 5 green, 1 red
Game 4: 1 green, 3 red, 6 blue; 3 green, 6 red; 3 green, 15 blue, 14 red
Game 5: 6 red, 1 blue, 3 green; 2 blue, 1 red, 2 green
}}

NB.    processLine&.> <;._2 sample
NB. ┌─────────┬─────────┬───────────┬───────────┬─────────┐
NB. │┌─┬─────┐│┌─┬─────┐│┌─┬───────┐│┌─┬───────┐│┌─┬─────┐│
NB. ││1│4 3 0│││2│0 1 2│││3│20 6  8│││4│ 3  6 1│││5│6 1 3││
NB. ││ │1 6 2│││ │1 4 3│││ │ 4 5 13│││ │ 6  0 3│││ │1 2 2││
NB. ││ │0 0 2│││ │0 1 1│││ │ 1 0  5│││ │14 15 3││└─┴─────┘│
NB. │└─┴─────┘│└─┴─────┘│└─┴───────┘│└─┴───────┘│         │
NB. └─────────┴─────────┴───────────┴───────────┴─────────┘

   checkGame=: 3 : 0
*/ {{ */ y <: limit }}"1 y
)

NB.    checkGame >1}>0{processLine&.> <;._2 sample
NB. 1

   filterIds=: 4 : 0
'id cubes'=. >x
if. checkGame cubes do.
  y,id
else.
  y
end.
)

NB.    (0$0)]F..filterIds processLine&.> <;._2 sample
NB. 1 2 5

NB.    +/ (0$0)]F..filterIds processLine&.> <;._2 sample
NB. 8

   input=: fread jpath '/home/pawel/Work/Personal/my-advent-of-code/2023/2/input.txt'
   part1=: +/ (0$0)]F..filterIds processLine&.> <;._2 input
NB. 2085 OK
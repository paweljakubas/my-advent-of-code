d:first each .Q.def[enlist[`filename]!enlist enlist "input.txt"] .Q.opt .z.x;

parseData:{"I" $ "\n" vs/: "\n\n" vs x};
part1:{max sum each x}
part2:{sum 3#desc sum each x}

data:` sv read0 `$ d[`filename];
0N!part1 parseData data;
0N!part2 parseData data;

exit 0;

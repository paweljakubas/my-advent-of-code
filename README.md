### Running Haskell code
```
// source ghcup env first: source ~/.ghcup/env
// run from the project directory (e.g., 2018/1/)
// Per-project: run `gen-hie > hie.yaml` in each directory with a `.cabal` file.
// myproject.cabal needs to be there
// run `cabal clean` before committing
cabal run myproject -- "part1" < input.txt
cabal run myproject -- "part2" < input.txt
```

### Running J code (after entering jconsole)
```
0!:1 < 'solution.ijs'
```

### Running Rust code
```
// before you run first time: cargo install rust-script
rust-script --test solution.rs
rust-script solution.rs input.txt part1
rust-script solution.rs input.txt part2
```

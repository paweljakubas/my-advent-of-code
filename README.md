### Running Haskell code
```
//stack solution.hs "tests"
cabal run solution.hs -- "part1" < input.txt
cabal run solution.hs -- "part2" < input.txt
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

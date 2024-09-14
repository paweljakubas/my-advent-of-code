### Running Haskell code
```
cat input.txt | stack solution.hs "part1"
cat input.txt | stack solution.hs "part2"
```

### Running J code (after entering jconsole)
```
0!:1 < 'solution.ijs'
```

### Running Rust code
```
// before you run first time: cargo install rust-script
// Run tests
rust-script --test solution.rs

// Run Rust code with file
rust-script solution.rs input.txt part1
```

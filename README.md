# Tyche Language Interpreter

## Compiling and running the interpreter

To compile the interpreter all you have to do is run:
```bash
make
```
After this the interpreter executable will be created which can be accessed by running:
```bash
./interpreter [file]
```
or
```bash
./interpreter
```
and providing the program to run in the standard input.

## Compiling the grammar using BNF Converter
To compile grammar from `tyche.cf` in bnfc with line information run:
```bash
cd bnfc
stack --stack-yaml stack-8.6.4.yaml build
stack --stack-yaml stack-8.6.4.yaml --local-bin-path ~/tmp/bin install
cd ..
~/tmp/bin/bnfc --functor -m -d tyche.cf
```
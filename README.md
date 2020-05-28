# Tyche Language Interpreter

## Running the interpreter
Simply run
```bash
./interpreter [file]
```
or
```bash
./interpreter
```
and provide the program to run in the standard input.
There is also a short help describing interpreter usage
```bash
./interpreter --help
```

## Compiling the interpreter

To compile the interpreter all you have to do is run:
```bash
make
```
After this the interpreter executable will be created.

## Changing language grammar
To compile grammar from `tyche.cf` in bnfc with line information run:
```bash
cd bnfc
stack --stack-yaml stack-8.6.4.yaml build
stack --stack-yaml stack-8.6.4.yaml --local-bin-path ~/tmp/bin install
cd ..
~/tmp/bin/bnfc --functor -m -d tyche.cf
make
rm Makefile
mv Makefile.bak Makefile
make
```
Then you need to adjust `Tyche/TypeCheck*` and `Tyche/Trans*` files to match your new grammar and replace the bnfc Makefile with old one:
```bash
rm -rf Makefile && mv Makefile.bak Makefile
```

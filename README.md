# Tyche Language Interpreter

## Running the interpreter
Simply run
```bash
./interpreter [file]
```
There is also a short help command available describing interpreter usage
```bash
./interpreter --help
```

## Compiling the interpreter
WARNING! If you are running this on the students server please replace stack location in Makefile. Replace:
```
stack setup
stack install random
stack ghc -- --make Tyche/Main.hs -o interpreter
```
with:
```
/home/students/inf/PUBLIC/MRJP/Stack/stack setup
/home/students/inf/PUBLIC/MRJP/Stack/stack install random
/home/students/inf/PUBLIC/MRJP/Stack/stack ghc -- --make Tyche/Main.hs -o interpreter
```

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
```
Then you need to adjust `Tyche/TypeCheck*` and `Tyche/Trans*` files to match your new grammar, replace the bnfc Makefile with old one and build the interpreter:
```bash
rm -rf Makefile && mv Makefile.bak Makefile
make
```

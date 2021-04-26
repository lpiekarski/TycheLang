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
WARNING! If you are running this on the students server run script `./students-setup.sh` instead of running make.

To compile the interpreter all you have to do is run (doesn't work on students):
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

## Code examples
1. Data types:
```
do

def a : int = 11
def b : float = 10.05
def c : string = "asdasdasd"
```
2. Literals and arithmetic comparisons:
```
do

def a : int = 10
def b : float = 10.05
if a > b do
  print_string("a is bigger than b")
  println()
else do
  print_string("b is bigger than a")
  println()
```
3. Variables and assignment operator:
```
do

def x : int = 10

def print_x : void (val x : readonly int) do
  print_string("x is equal ")
  print_int(x)
  println()
  return void

print_x(x)

if true do
  def x : int = x + 5
  print_x(x)

print_x(x)
```
4. Input/Output functions:
```
do

print_string("hello world")
println()

print_int(100)
println()

print_char(100)
println()

print_string("input a character: ")
println()
def x : int = read_char()
print_string("Your input was: ")
print_char(x)
println()
```
5. While loop and if statements:
```
do

def x : int = 0
def y : int = 42

while x * x < y do
  x = x + 1

print_int(x)
println()

x = 15
y = 15
if x equals y do
  print_string("x equals y")
  println()
else do
  print_string("x doesn't equal y")
  println()

x = 16
y = 15
if x equals y do
  print_string("x equals y")
  println()
else do
  print_string("x doesn't equal y")
  println()
```
6. Recursive functions:
```
do

def factorial : int (val x : readonly int) do
  if x < 2 do
    return 1
  return x * factorial(x - 1)


print_int(factorial(7))
println()
```
7. Function parameters passing (variable, value, in-out):
```
do

def a : int = 0
def b : int = 0
def c : int = 0


def f : void (var a : int, val b : int, inout c : int) do
  a = a + 1
  b = b + 2
  c = c + 3
  return void

f(a, b, c)
print_int(a)
println()
print_int(b)
println()
print_int(c)
println()

f(a, a, a)
print_int(a)
println()
print_int(b)
println()
print_int(c)
println()
```
8. For loop and readonly variables:
```
do

for i from 0 to 10 do
  print_string("foo")
  println()

for i from 10 to 0 do
  print_string("bar")
  println()

/*
// loop variable is readonly, uncomment to see the error
for i from 0 to 10 do
  i = i - 1;
*/

for each i from [0, 5, 2, 3] do
  print_int(i)
  println()
```
9. Overriding and static binding:
```
do

def x : int = 1


def f : (var int) -> void () do
  return lambda : void (var y : int) do
    y = y + x
    return void


def g : void (var x : int) do
  def a : int = 0
  def l : (var int) -> void = f()
  l(a)
  if a == 1 do
    print_string("a is 1")
    println()
  else do
    print_string("a is not 1")
    println()
    print_string("a is ")
    print_int(a)
    println()
  return void

def x : int = 10
def arg : int = 10
g(arg)
```
10. Runtime exceptions:
```
do

def x : int = 10
def y : int = 0

x = x / y         // this is 10 / 0

print_string("this is not displayed")
println()
```
11. Functions returning value:
```
do

def pi : float (val samples : readonly int) do
  def prob : float = probability sampled samples times of
    def x : readonly float = random_float() * 2.0 - 1.0
    def y : readonly float = random_float() * 2.0 - 1.0
  satisfying
    x * x + y * y <= 1.0
  return 4.0 * prob

print_string("estimated pi value: ")
print_float(pi(100000))
println()
```
12. Static typing:
```
do

def x : string = "asd"

/*
// Static Error
x = 10
*/

// OK
def x : int = 10
```
13. Arrays and lists:
```
do

def arr : {int} = {0, 1, 2, 3, 4}
def lst : [int] = [0, 1, 2, 3, 4]
def arr2 : {int} = array 100 times 5 : int

lst = -1 . lst    // cons
print_int(arr[2])   // reference to array element
println()
print_int(arr2[90])
println()
```
14. Break and continue loop control:
```
do

print_string("first loop:")
println()
for i from 0 to 10 do
  print_int(i)
  println()
  if i == 5 do
    break

print_string("second loop:")
println()
for i from 0 to 10 do
  if i == 5 do
    continue
  print_int(i)
  println()
```
15. Anonymous and higher order functions:
```
do

def x : int = 1


def f : (var int) -> void () do
  def z : int = x
  return lambda : void (var y : int) do
    y = y + z
    return void

def f1 : (var int) -> void = f()
x = 3
def f2 : (var int) -> void = f()

def y : int = 10

f1(y)
print_int(y)
println()

f2(y)
print_int(y)
println()
```

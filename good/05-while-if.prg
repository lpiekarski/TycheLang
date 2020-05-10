def x : int = 0;
def y : int = 42;

while x * x < y do
  x = x + 1;

def printIntBackwards : void (var x : readonly int) {
  def v : int = x;
  while v > 0 do {
    def r : int = v % 10;
    if r == 0 then
      print("0");
    else if r == 1 then
      print("1");
    else if r == 2 then
      print("2");
    else if r == 3 then
      print("3");
    else if r == 4 then
      print("4");
    else if r == 5 then
      print("5");
    else if r == 6 then
      print("6");
    else if r == 7 then
      print("7");
    else if r == 8 then
      print("8");
    else if r == 9 then
      print("9");
    v = v / 10;
  }
}

printIntBackwards(x);

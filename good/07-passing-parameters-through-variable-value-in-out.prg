def f : void (var a : int, val b : int, inout c : int) {
  a = a + 1;
  b = b + 1;
  c = c + 1;
  return;
}

def a : int = 0;
def b : int = 0;
def c : int = 0;

f(a, b, c);

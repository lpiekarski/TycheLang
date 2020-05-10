def x : int = 1;

def f : (var int) -> void ( ) {
  return lambda : void (var y : int) -> {
    y = y + x;
    return;
  };
}

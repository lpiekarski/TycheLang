def x : int = 1;

def f : (var int) -> void ( ) {
  return lambda : void (var y : int) -> {
    y = y + x;
    return;
  };
}

def g : void (var x : int) {
  def a : int = 0;
  def l : (var int) -> void = f( );
  l(a);
  if a == 1 then
    ; // binding is static
  else
    ; // binding is not static
}

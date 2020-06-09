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

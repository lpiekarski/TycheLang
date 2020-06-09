do

def random_float : float (
    val min : readonly float,
    val max : readonly float) do
  def epsilon : float = 1.0e-16
  def result : float = 0.0
  def frac : float = 0.1
  while frac > epsilon do
    result = result + (random from [0.0, 1.0, 2.0, 3.0, 4.0, 5.0, 6.0, 7.0, 8.0, 9.0]) * frac
    frac = frac * 0.1
  return result * (max - min) + min

def pi : float (val samples : readonly int) do
  def prob : float = probability sampled samples times of
    def x : readonly float = random_float(-1.0, 1.0)
    def y : readonly float = random_float(-1.0, 1.0)
  satisfying
    x * x + y * y <= 1.0
  return 4.0 * prob

print_string("pi is ")
print_float(pi(100000))
println()

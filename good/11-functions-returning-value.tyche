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

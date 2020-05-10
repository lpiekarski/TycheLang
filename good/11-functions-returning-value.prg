def random_float : float (
    val min : readonly float,
    val max : readonly float) {
  def epsilon : float = 1.0e-16;
  def result : float = 0.0;
  def frac : float = 0.1;
  while frac > epsilon do {
    result = result + (random from [0.0, 1.0, 2.0, 3.0, 4.0, 5.0, 6.0, 7.0, 8.0, 9.0]) * frac;
    frac = frac * 0.1;
  }
  result * (max - min) + min;
}

def pi : float (val samples : readonly int)
  4.0 * (probability tested samples times of {
    def x : readonly float = random_float(-1.0, 1.0);
    def y : readonly float = random_float(-1.0, 1.0);
  } satisfying x * x + y * y <= 1.0);

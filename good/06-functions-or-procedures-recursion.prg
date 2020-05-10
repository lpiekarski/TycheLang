def factorial : int (var x : readonly int)
  x equals 0 ? 1 : factorial(x - 1);

factorial(10);

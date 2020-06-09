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

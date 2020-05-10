def str : string = "foo";
str = "bar";


def str2 : readonly string = "foo";
/*
// TypeCheck Error: at 8:1 Cannot assign value to readonly variable
str2 = "bar";
*/

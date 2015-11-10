function main (argument) {
  var f = function(x) { x(1); };
  var g = function(y) { y+2; };
  var h = function(z) { z+3; };
  f(g) + f(h);
}


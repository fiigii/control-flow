function main (argument) {
  var f = function (x) x;
  f(function(y)y)(f)
}

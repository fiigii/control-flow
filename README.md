## Control Flow Analysis
The control flow analyzer (0-CFA) for my Come language. 

For program:  
```  
function main (argument) {
  var f = function(x) { x(1); };
  var g = function(y) { y+2; };
  var h = function(z) { z+3; };
  f(g) + f(h);
}
```

The control flow is :  
```  
cabal run
Preprocessing executable '0CFA' for 0CFA-0.1.0.0...
Running 0CFA...

Labeled Program
(var f = function (x) {x @<1> (1 @<2> ) @<3> } @<4> ;
 var g = function (y) {y @<5> +2 @<6>  @<7> } @<8> ;
 var h = function (z) {z @<9> +3 @<10>  @<11> } @<12> ;
f @<13> (g @<14> ) @<15> +f @<16> (h @<17> ) @<18>  @<19>  @<20> ,20)

Cache
1 -> [function (y) {y @<5> +2 @<6>  @<7> } @<8> ,function (z) {z @<9> +3 @<10>  @<11> } @<12> ]
2 -> []
3 -> []
4 -> [function (x) {x @<1> (1 @<2> ) @<3> } @<4> ]
5 -> []
6 -> []
7 -> []
8 -> [function (y) {y @<5> +2 @<6>  @<7> } @<8> ]
9 -> []
10 -> []
11 -> []
12 -> [function (z) {z @<9> +3 @<10>  @<11> } @<12> ]
13 -> [function (x) {x @<1> (1 @<2> ) @<3> } @<4> ]
14 -> [function (y) {y @<5> +2 @<6>  @<7> } @<8> ]
15 -> []
16 -> [function (x) {x @<1> (1 @<2> ) @<3> } @<4> ]
17 -> [function (z) {z @<9> +3 @<10>  @<11> } @<12> ]
18 -> []
19 -> []
20 -> []

Envir
f -> [function (x) {x @<1> (1 @<2> ) @<3> } @<4> ]
g -> [function (y) {y @<5> +2 @<6>  @<7> } @<8> ]
h -> [function (z) {z @<9> +3 @<10>  @<11> } @<12> ]
x -> [function (y) {y @<5> +2 @<6>  @<7> } @<8> ,function (z) {z @<9> +3 @<10>  @<11> } @<12> ]
y -> []
z -> []

```

## Author  
* Fei Peng

## Copyright

Copyright (c) 2015 Fei Peng

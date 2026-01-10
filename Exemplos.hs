
123


num n return i ->  digit<d>
                   {k = 10*n + d;}
                   num<k,i>
                   /
                   {i = n;}

[1]23      1[2]3      12[3]       123[]
num<0,123> => num<1,123>    => num<12,123> => num<123,123> =>
  d = 1        d = 2           d = 3
  k = 10*0+1   k = 10*1 + 2    k = 10*12 + 3

====== Compilando ======
n -> 0
i -> 1
d -> 2
k -> 3
num n return i ->  digit<d>
                   {k = 10*n + d;}
                   num<k,i>
                   /
                   {i = n;}

num:  choice endN
      call digit
      store 2
      push 10
      load 0
      mul
      load 2
      add
      sotre 3
      load 3
      call num
      store 1
      load 1
      return 1
      commit fim
endN:

fim: halt.


A return n ::= 'a'
               A<k>;
               {n = k + 1;}
               /
               {n = 0;}

[n-> 0;k->1]

0     call A;
1     halt;
2 A : choice "alt1";
3     char 'a';
4     call A;
5     store 1;
6     push 1;
7     load 1;
8     add;
9     store 0;
10    commit "end";
11 alt1: push 0;
12       store 0;
13 end: load 0;
14      return 1;

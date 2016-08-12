b = factor(c("a","b"))
c = factor(c("b","c"))
b = factor(b,levels = c)
> b
[1] <NA> b   
Levels: b c
b = factor(b, unique(c(levels(b),levels(c))))
> b
[1] a b
Levels: a b c

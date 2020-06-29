### to demonstrate the code 

source("voting_system_diagrams_main.r")

basic.v.vec <- c(20, 6, 13,10, 4, 21, 2,4,3)
v.vec <- basic.v.vec/sum(basic.v.vec)

plot.condorcet.result(v.vec, in.cycle = "kemeny", add.fp.result = T)

plot.av.result(v.vec, add.fp.result = T)

plot.positional.result(v.vec, positional.s = .5, add.fp.result = T)

# TODO: better commented code, better options for inputting the voting result (named list?), conorcet completions?  
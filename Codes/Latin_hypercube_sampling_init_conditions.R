# create latin hypercube to select initial conditions for state variables -----------
rm(list=ls()) #remove previous variable assignments

# load library
library(lhs)

# set conditions
lhs_design <- optimumLHS(n = 50, k = 7, verbose = FALSE)
lhs_design <- as.data.frame(lhs_design)
colnames(lhs_design) <- c("m1", "m2", "m3", "s", "e", "i", "r")

# divide to make SEI = 1 and SEIR = 1
lhs_design$IC <- seq(1:nrow(lhs_design))
lhs_design$total.m <- rowSums(lhs_design[1:3])
lhs_design$total.h <- rowSums(lhs_design[4:7])
lhs_design$m1 <- lhs_design$m1/lhs_design$total.m
lhs_design$m2 <- lhs_design$m2/lhs_design$total.m
lhs_design$m3 <- lhs_design$m3/lhs_design$total.m
lhs_design$s <- lhs_design$s/lhs_design$total.h
lhs_design$e <- lhs_design$e/lhs_design$total.h
lhs_design$i <- lhs_design$i/lhs_design$total.h
lhs_design$r <- lhs_design$r/lhs_design$total.h

initCond <- lhs_design[,1:8]

# save sampling design
write.csv(initCond, "Concatenated_Data/sensitivity_analyses/LHS_inputs.csv", row.names = F)

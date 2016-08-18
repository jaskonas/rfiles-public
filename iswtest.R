library(igraph)
library(fmsb)
library(ggplot2)
library(tidyr)
library(grid)
library(gridExtra)
library(scales)
library(network)
# setwd("~/Dropbox/Appcare")
test1 <- barabasi.game(200, power = 0.8, m = NULL,
                       out.dist = NULL, out.seq = NULL,
                       out.pref = FALSE, zero.appeal = 0.5, directed = FALSE,
                       algorithm = c("psumtree", "psumtree-multiple", "bag"),
                       start.graph = NULL)
V(test1)$size=3
V(test1)$color="#04173b"
plot(test1, vertex.label="")
library(intergraph)
# test2=network(matrix(data=1))
# for (i in 1:10){
#   add.vertices(2)
# }
net1=asNetwork(test1)
ggplot(net1)

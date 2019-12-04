# to igraph
library(rgexf)
library(stringr)
setwd('ME/HSE/SNA/SNA Home Assignment 2/')
x <- read.gexf(x = 'egonet.gexf')
x$nodes
y <- gexf.to.igraph(gexf.obj = x)
y <- set_vertex_attr(graph = y, name = 'name', value = substr(x$nodes$label, 1, 3))
plot(y)
write.graph(graph = y, file = 'egonet.gml', format = 'gml')

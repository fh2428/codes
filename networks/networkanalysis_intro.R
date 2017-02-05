

setwd("C:/Users/u0112618/Documents/1. Nicolas/1. KU Leuven/1. Euthority/3.R Analysis/4. Network analysis")
# setwd("~/Downloads")

library(igraph)
dat <- read.csv2("MetFrames.csv", header=TRUE, row.names=1, check.names=FALSE) 
m <- as.matrix(dat)
g <- graph.adjacency(m ,mode="undirected", weighted=NULL, diag=FALSE)
plot.igraph(g)

net <- graph.adjacency(m, mode="directed", weighted=TRUE, diag=FALSE) #the only difference between this and the weighted network code is that mode="directed"
plot(net)

plot.igraph(net ,vertex.label=V(net)$name, layout=layout.fruchterman.reingold, 
            vertex.label.color="black", edge.color="black", edge.width=E(net)$weight/3, edge.arrow.size=1.5)

plot.igraph(net, vertex.label=V(net)$name, layout=layout.fruchterman.reingold, vertex.label.color="black", edge.color="black",
            edge.width=E(net)$weight/3, edge.arrow.size=1.5,edge.curved=TRUE)



#Fraukes playground
# setting colour and shape parameters for nodes and vertices (size is easy enough to add also)
set.seed(991)
plot.igraph(net ,vertex.label=V(net)$name, layout=layout.fruchterman.reingold, vertex.label.dist=1,
            vertex.label.color="black", edge.color="black", edge.width=E(net)$weight/5, edge.arrow.size=.4)

plot.igraph(net ,vertex.label=V(net)$name, layout=layout.fruchterman.reingold, vertex.label.dist=0.9, vertex.size=6,
            vertex.label.color="black", edge.color="black", edge.width=E(net)$weight/5, edge.arrow.size=.4)
set.seed(991)
plot.igraph(net ,vertex.label=V(net)$name, layout=layout.fruchterman.reingold, vertex.label.dist=1, vertex.size=6,
            vertex.label.color="black", vertex.color="#E0E0E0", edge.color="#404040", edge.arrow.size=.2,
            edge.width=E(net)$weight/5, edge.arrow.size=.4, frame=F, margin=0.01)

set.seed(991)
plot.igraph(net ,vertex.label=V(net)$name, layout=layout.fruchterman.reingold, vertex.label.dist=1, vertex.size=6,
            vertex.label.color="black", vertex.color="#E0E0E0", edge.color="#404040", edge.arrow.size=.2,
            edge.width=E(net)$weight/5, edge.arrow.size=.4, frame=F, margin=0.01, edge.curved=0.3)


set.seed(991)
plot.igraph(net ,vertex.label=V(net)$name, vertex.label.dist=1, vertex.size=6,
            vertex.label.color="black", vertex.color="#E0E0E0", edge.color="#404040", edge.arrow.size=.2,
            edge.width=E(net)$weight/5, edge.arrow.size=.4, frame=F, margin=0.01, edge.curved=0.3, layout=layout_randomly)
            

#finding a layout
set.seed(991)
plot.igraph(net ,vertex.label=V(net)$name, vertex.label.dist=1, vertex.size=6,
            vertex.label.color="black", vertex.color="#E0E0E0", edge.color="#404040", edge.arrow.size=.2,
            edge.width=E(net)$weight/5, edge.arrow.size=.4, frame=F, margin=0.01, edge.curved=0.3, layout=layout_randomly)

set.seed(991)
circle=layout_in_circle(net)
plot.igraph(net ,vertex.label=V(net)$name, vertex.label.dist=1, vertex.size=6,
            vertex.label.color="black", vertex.color="#E0E0E0", edge.color="#404040", edge.arrow.size=.2,
            edge.width=E(net)$weight/5, edge.arrow.size=.4, frame=F, margin=0.01, edge.curved=0.3, layout=circle, vertex.label.cex=.5)

set.seed(991)
sphere=layout_on_sphere(net)
plot.igraph(net ,vertex.label=V(net)$name, vertex.label.dist=1, vertex.size=6,
            vertex.label.color="black", vertex.color="#E0E0E0", edge.color="#404040", edge.arrow.size=.2,
            edge.width=E(net)$weight/5, edge.arrow.size=.4, frame=F, margin=0.01, edge.curved=0.3, layout=sphere, vertex.label.cex=.5)

#advantage: we see whitch nodes have no connection
set.seed(991)
x11()    # command to open a new active window for UNIX [on windows it is win()]
frame=layout_with_fr(net)
plot.igraph(net ,vertex.label=V(net)$name, vertex.label.dist=1, vertex.size=6,
            vertex.label.color="black", vertex.color="#E0E0E0", edge.color="#404040", edge.arrow.size=.2,
            edge.width=E(net)$weight/5, edge.arrow.size=.4, frame=F, margin=0.01, edge.curved=0.3, rescale=F, layout=frame, vertex.label.cex=.5)

# tkplot() basically like inkspace
frame=layout_with_fr(net)
tkplot(net ,vertex.label=V(net)$name, vertex.label.dist=1, vertex.size=6,
            vertex.label.color="black", vertex.color="#E0E0E0", edge.color="#404040", edge.arrow.size=.2,
            edge.width=E(net)$weight/5, edge.arrow.size=.4, frame=F, margin=0.01, edge.curved=0.3, rescale=F, layout=frame, vertex.label.cex=.5)

# tkplot() basically like inkspace
cluster=cluster_edge_betweenness(net)
plot(cluster, net)


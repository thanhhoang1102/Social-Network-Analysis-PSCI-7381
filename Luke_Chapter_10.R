# SNA 
# Federico Ferrero
# Luke: Chapter 10: RANDOM NETWORK MODELS

library(igraph)
g <- erdos.renyi.game(n=12,10,type='gnm')
g

graph.density(g)

op <- par(mar=c(0,1,3,1),mfrow=c(1,2))
plot(erdos.renyi.game(n=12,10,type='gnm'),
     vertex.color=2,
     main="First random graph")
plot(erdos.renyi.game(n=12,10,type='gnm'),
     vertex.color=4,
     main="Second random graph")
par(op)

g <- erdos.renyi.game(n=1000,.005,type='gnp')
plot(degree.distribution(g),
     type="b",xlab="Degree",ylab="Proportion")

crnd <- runif(500,1,8)
cmp_prp <- sapply(crnd,function(x)
  max(clusters(erdos.renyi.game(n=1000,
                                p=x/999))$csize)/1000)
smoothingSpline <- smooth.spline(crnd,cmp_prp,
                                 spar=0.25)
plot(crnd,cmp_prp,col='grey60',
     xlab="Avg. Degree",
     ylab="Largest Component Proportion")
lines(smoothingSpline,lwd=1.5)

n_vect <- rep(c(50,100,500,1000,5000),each=50)
g_diam <- sapply(n_vect,function(x)
diameter(erdos.renyi.game(n=x,p=6/(x-1))))
library(lattice)
bwplot(g_diam ˜ factor(n_vect), panel = panel.violin,
       xlab = "Network Size", ylab = "Diameter")


g1 <- watts.strogatz.game(dim=1, size=30, nei=2, p=0)
g2 <- watts.strogatz.game(dim=1, size=30, nei=2, p=.05)
g3 <- watts.strogatz.game(dim=1, size=30, nei=2, p=.20)
g4 <- watts.strogatz.game(dim=1, size=30, nei=2, p=1)
op <- par(mar=c(2,1,3,1),mfrow=c(2,2))
plot(g1,vertex.label=NA,layout=layout_with_kk,
     main=expression(paste(italic(p)," = 0")))
plot(g2,vertex.label=NA,
     main=expression(paste(italic(p)," = .05")))
plot(g3,vertex.label=NA,
     main=expression(paste(italic(p)," = .20")))
plot(g4,vertex.label=NA,
     main=expression(paste(italic(p)," = 1")))
par(op)


g100 <- watts.strogatz.game(dim=1,size=100,nei=2,p=0)
g100

diameter(g100)
p_vect <- rep(1:30,each=10)
g_diam <- sapply(p_vect,function(x)
  diameter(watts.strogatz.game(dim=1, size=100,
                               nei=2, p=x/200)))
smoothingSpline = smooth.spline(p_vect, g_diam,
                                spar=0.35)
plot(jitter(p_vect,1),g_diam,col='grey60',
     xlab="Number of Rewired Edges",
     ylab="Diameter")
lines(smoothingSpline,lwd=1.5)

g <- barabasi.game(500, directed = FALSE)
V(g)$color <- "lightblue"
V(g)[degree(g) > 9]$color <- "red"
node_size <- rescale(node_char = degree(g), low = 2,high = 8)
plot(g, vertex.label = NA, vertex.size = node_size)

median(degree(g))
mean(degree(g))

table (degree(g))

op <- par(mfrow=c(1,2))
plot(degree.distribution(g),xlab="Degree",
     ylab="Proportion")
plot(degree.distribution(g),log='xy',
     xlab="Degree",ylab="Proportion")
par(op)

g<- barabasi.game(500, out.dist = c(0.25, 0.5, 0.25),
                 directed = FALSE, zero.appeal = 1)
V(g)$color <- "lightblue"
V(g)[degree(g) > 9]$color <- "red"
node_size <- rescale(node_char = degree(g), low = 2,
                     high = 8)
plot(g, vertex.label = NA, vertex.size = node_size)

g1 <- barabasi.game(10,m=1,directed=FALSE)
g2 <- barabasi.game(25,m=1,directed=FALSE)
g3 <- barabasi.game(50,m=1,directed=FALSE)
g4 <- barabasi.game(100,m=1,directed=FALSE)
op <- par(mfrow=c(2,2),mar=c(4,0,1,0))
plot(g1, vertex.label= NA, vertex.size = 3,
     xlab = "n = 10")
plot(g2, vertex.label= NA, vertex.size = 3,
     xlab = "n = 25")
plot(g3, vertex.label= NA, vertex.size = 3,
     xlab = "n = 50")
plot(g4, vertex.label= NA, vertex.size = 3,
     xlab = "n = 100")
par(op)

# HOMEWORK 4
# Federico Ferrero
# 4/6/2021

# clear your memory
rm(list=ls())

# Importing edgelist data into R
library(igraph)

edges<- read.csv("https://raw.githubusercontent.com/federico-jf/Social-Network-Analysis-PSCI-7381/main/Edgelist-Media-Edges.csv", header=T, as.is=T)
nodes<- read.csv("https://raw.githubusercontent.com/federico-jf/Social-Network-Analysis-PSCI-7381/main/Edgelist-Media-Nodes.csv", header=T, as.is=T)

head(edges) # checking variables in edges
head(nodes) # checking variables in nodes

# Transforming it into a network object
network <- graph_from_data_frame(d=edges, vertices=nodes, directed = TRUE)
network
summary(network) # asking for summary of network

# Removing the loops
network2 <- simplify(network, remove.multiple = F, remove.loops = T) 

# Plotting the network
plot(network2)

# verify layouts in igraph
?layout

# Random layout
plot(network2, layout=layout_randomly)

# Circular layout
plot(network2, layout=layout_in_circle)

# Fruchterman-Reingold layout (FR)
plot(network2, layout=layout_with_fr)

# Kamada and Kawai layout (KK)
plot(network2, layout=layout_with_kk)

# Graphopt layout
plot(network2, layout=layout_with_graphopt)

# Graphopt layout with smaller arrows
plot(network2, layout=layout_with_graphopt, edge.arrow.size=.15)

# Changing the color of nodes depending on media type
col <- c("cadetblue2", "coral2", "goldenrod2")
V(network2)$color <- col[V(network2)$media.type]
plot(network2, layout=layout_with_graphopt, edge.arrow.size=.15)

# Changing the shape of nodes based on type of media
?igraph.vertex.shapes # I have only two very different shapes (square and circle,). 
# I Need a triangle. I will use the code suggested by the documentation.

# first, adding a triangle as a shape
mytriangle <- function(coords, v=NULL, params) {
  vertex.color <- params("vertex", "color")
  if (length(vertex.color) != 1 && !is.null(v)) {
    vertex.color <- vertex.color[v]
  }
  vertex.size <- 1/135 * params("vertex", "size")
  if (length(vertex.size) != 1 && !is.null(v)) {
    vertex.size <- vertex.size[v]
  }
  
  symbols(x=coords[,1], y=coords[,2], bg=vertex.color,
          stars=cbind(vertex.size, vertex.size, vertex.size),
          add=TRUE, inches=FALSE)
}
add_shape("triangle", clip=shapes("circle")$clip,
          plot=mytriangle)

# Now I have three very different shapes with igraph. Plot the network with the shapes
shape <- c("square", "circle", "triangle")
V(network2)$shape <- shape[V(network2)$media.type]
plot(network2, layout=layout_with_graphopt, edge.arrow.size=.3)

# Changing node size using centrality measures
?degree # See documentation
degree <- degree(network2, mode="in") # all, in, out
V(network2)$size <- degree*6
plot(network2, layout=layout_with_graphopt, edge.arrow.size=.3)

# Using the audience size value to modify node size
head(nodes) # checking variables in nodes
V(network2)$size <- V(network2)$audience.size*0.8
plot(network2, layout=layout_with_graphopt, edge.arrow.size=.3)

# Labeling nodes with the name of media organization
plot(network2, layout=layout_with_graphopt, edge.arrow.size=.15, vertex.label=V(network2)$media, 
     vertex.label.font=35, vertex.label.color="black",
     vertex.label.cex=.7, edge.color="gray27")

# Changing the size of the edges depending on the weight
head(edges) # checking variables in edges
E(network2)$width <- E(network2)$weight/7
plot(network2, layout=layout_with_graphopt, edge.arrow.size=.15, vertex.label=V(network2)$media, 
     vertex.label.font=35, vertex.label.color="black",
     vertex.label.cex=.7, edge.color="gray27")

# Changing the color of the edges depending on the type of tie
E(network2)$color <- ifelse(E(network2)$type=="hyperlink",'blue','hotpink')
plot(network2, layout=layout_with_graphopt, edge.arrow.size=.2, vertex.label=V(network2)$media, 
     vertex.label.font=35, vertex.label.color="black",
     vertex.label.cex=.7, color.edge=E(network2)$color)

# Adding the legend
legend(x=-1.9, y=-0.9, c("Newspaper","Television", "Online News"), pch=c(22,21,24),
       col=col, pt.bg=col,text.font= 8, pt.cex=2, cex=.8, bty="n", ncol=1)

### Making an optimal visualization
dev.off() # deleting formatting

# Changing the color of nodes depending on media type
col <- c("cadetblue2", "coral2", "goldenrod2")
V(network2)$color <- col[V(network2)$media.type]
plot(network2, layout=layout_with_graphopt, edge.arrow.size=.15)

# Changing node size using indegree centrality 
degree <- degree(network2, mode="in") # all, in, out
V(network2)$size <- degree*6
plot(network2, layout=layout_with_graphopt, edge.arrow.size=.3)

# Labeling nodes with the name of media organization
plot(network2, layout=layout_with_graphopt, edge.arrow.size=.15, vertex.label=V(network2)$media, 
     vertex.label.font=35, vertex.label.color="black",
     vertex.label.cex=.7)

# Changing the size of the edges depending on the weight
head(edges) # checking variables in edges
E(network2)$width <- E(network2)$weight/7
plot(network2, layout=layout_with_graphopt, edge.arrow.size=.15, vertex.label=V(network2)$media, 
     vertex.label.font=35, vertex.label.color="black",
     vertex.label.cex=.7, edge.color="gray27")

# Changing the color of the edges depending on the type of tie
E(network2)$color <- ifelse(E(network2)$type=="hyperlink",'blue','hotpink')
plot(network2, layout=layout_with_graphopt, edge.arrow.size=.2, vertex.label=V(network2)$media, 
     vertex.label.font=35, vertex.label.color="black",
     vertex.label.cex=.8, color.edge=E(network2)$color, main="Making an Optimal Visualization")

# Adding the legenda
legend(x=-1.9, y=-0.9, c("Newspaper","Television", "Online News"), pch=c(22,21,24),
       col=col, pt.bg=col,text.font= 8, pt.cex=2, cex=.8, bty="n", ncol=1)

legend(x=0.4, y=-0.999999, legend=c("Hyperlink", "Mention"),
       col=c("blue","hotpink"), lty=1, cex=0.8, box.lty=0, text.font= 8)

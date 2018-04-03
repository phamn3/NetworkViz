#Adapted From: Ognyanova, K. (2017) Network visualization with R. Retrieved from www.kateto.net/network-visualization.

#install required packages
install.packages("igraph")
install.packages("network")
install.packages("sna")
install.packages("visNetwork")
install.packages("threejs")
install.packages("networkD3")
install.packages("ndtv")

#color, position, size, and shape are impt. for network visualizations
#COLORS - Figure 1
#pch = point symbol shape, cex=size, col=color
plot(x=1:10, y=rep(5,10), pch=19, cex=3, col="dark red")
points(x=1:10, y=rep(6,10), pch=19, cex=3, col="557799")
points(x=1:10, y=rep(4,10), pch=19, cex=3, col=rgb(.25,.5,.3))

#Figure 2-set transparency of objects using parameter alpha (range 0-1)
plot(x=1:5, y=rep(5,5), pch=19, cex=12, col=rgb(.25,.5,.3, alpha=.5), xlim=c(0,6))
par(bg="gray40")
col.tr <- grDevices::adjustcolor("557799", alpha=0.7)
plot(x=1:5, y=rep(5,5), pch=10, cex=12, col=col.tr, xlim=c(0,6))

#taking a look at color palettes
palte1 <- heat.colors(5, alpha=1) #opaque, from heat palette
palte2 <- rainbow(5, alpha=.5) #transparent, from heat palette
plot(x=1:10, y=1:10, pch=19, cex=5, col=palte1)
plot(x=1:10, y=1:10, pch=19, cex=5, col=palte2)

#####Dataset 1: edgelist############
nodes <- Dataset1.Media.Example.NODES
links <- Dataset1.Media.Example.EDGES

head(nodes)
nrow(nodes)
length(unique(nodes$id))
head(links) #IN A LIST FORM --> USEFUL FOR PROJECT
nrow(links)
nrow(unique(links[,c("from", "to")])) #more links than unique from-to combos
#indicates that there are multiple links between the same two nodes, so will need to
#collapse links by summing weights

links <- aggregate(links[,3], links[,-3], sum)
links <- links[order(links$from, links$to),]
colnames(links)[4] <- "weight"
rownames(links) <- NULL

#convert data to igraph
#d represents edges of network: 1st two columns must be ID of source & target
#vertices represent types of node with attributes
library('igraph')
net <- graph_from_data_frame(d=links, vertices = nodes, directed=T)
net
#look at pieces of igraph
E(net) #edges of net
V(net) #vertices of net
E(net)$type #Edge attribute type
V(net)$media #Vertex attribute media
V(net)[media=="BBC"] #find nodes & edges by attribute
E(net)[type=="mention"]

#edgelist or matrix
as_edgelist(net, names=T)
as_adjacency_matrix(net, attr="weight")

#plot the igraph
plot(net)
net <- simplify(net, remove.multiple = F, remove.loops = T) #removing loops in graph
plot(net, edge.arrow.size=.4, vertex.label=NA) #reducing arrow size & removing labels
plot(net, edge.arrow.size=.4) #playing around with plot parameters below
plot(net, edge.arrow.size=.4, edge.curved = .1) #curved edges
plot(net, edge.arrow.size=.2, edge.color="orange",
     vertex.color="orange", vertex.frame.color="#ffffff",
     vertex.label=V(net)$media, vertex.label.color="black")

#color nodes based on media type, size based on # of links connected to node
colrs <- c("gray50", "tomato", "gold") #generate colors based on media type
V(net)$color <- colrs[V(net)$media.type]
deg <- degree(net, mode="all") #compute node degree
V(net)$size <- deg*3
# We could also use the audience size value:
V(net)$size <- V(net)$audience.size*0.6
V(net)$label <- NA #remove labels
E(net)$width <- E(net)$weight/6 #set edge width based on weight
#change arrow size and edge color:
E(net)$arrow.size <- .2
E(net)$edge.color <- "gray80"
E(net)$width <- 1+E(net)$weight/12
plot(net)
legend(x=-1.5, y=-1.1, c("Newspaper","Television", "Online News"), pch=21,
       col="#777777", pt.bg=colrs, pt.cex=2, cex=.8, bty="n", ncol=1) #add legend


#plotting only labels of nodes
plot(net, vertex.shape="none", vertex.label=V(net)$media,
     vertex.label.font=2, vertex.label.color="gray40",
     vertex.label.cex=.7, edge.color="gray85")



####### Dataset 2: matrix ########## example of two-mode/bipartite network
nodes2 <- Dataset2.Media.User.Example.NODES
links2 <- Dataset2.Media.User.Example.EDGES

head(nodes2) #examine
head(links2) #edges are in a matrix form !!!!
links2 <- as.matrix(links2)
dim(links2)
dim(nodes2)

net2 <- graph_from_incidence_matrix(links2) #read matrix into graph object
table(V(net2)$type)



##### Network Layouts ######
#basic random generated network graph - Barabasi-Albert model
net.bg <- sample_pa(80)
V(net.bg)$size <- 8
V(net.bg)$frame.color <- "white"
V(net.bg)$color <- "orange"
V(net.bg)$label <- ""
E(net.bg)$arrow.mode <- 0
plot(net.bg)

#Force-directed layouts - nicer looking graphs, "electrically charged particules"
#Fruchterman-Reingold
l <- layout_with_fr(net.bg) #this step is like setting the seed
plot(net.bg, layout=l)
#Kamada Kawai
#LGL algorithm
#MDS - multidimensional scaling --> tries to place nodes based on measure of similarity
  # POTENTIAL FOR PROJECT^^^^? nodes often overall though
plot(net.bg, layout=layout_with_mds)


#Potential Ideas for Filtering
hist(links$weight)
mean(links$weight)
sd(links$weight)
cut.off <- mean(links$weight)
net.sp <- delete_edges(net, E(net)[weight<cut.off])
plot(net.sp)
#clustering
par(mfrow=c(1,2))
clp <- cluster_label_prop(net)
class(clp)
# Community detection returns an object of class "communities"
# which igraph knows how to plot:
plot(clp, net)
# We can also plot the communities without relying on their built-in plot:
V(net)$community <- clp$membership
colrs <- adjustcolor( c("gray50", "tomato", "gold", "yellowgreen"), alpha=.6)
plot(net, vertex.color=colrs[V(net)$community])
dev.off()


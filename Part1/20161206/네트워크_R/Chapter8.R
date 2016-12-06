#Chapter8. Subgroups
#Cliques #K-Cores #Modulaiity

# A Clique : a maximally complete subgraph. 
#            a subset of noeds the have all possible ties 
#            among them

library(igraph)
clqexmp <- graph.formula(A:B:C:D--A:B:C:D,D-E,E-F-G-E)
plot(clqexmp)
clique.number(clqexmp)
cliques(clqexmp)
#클리크 크기가 3보다 큰 것들만 의미가 있는 듯...
cliques(clqexmp, min=3)
maximal.cliques(clqexmp,min=3)
largest.cliques(clqexmp)

g25 <- erdos.renyi.game(25, 75, type="gnm") 
g50 <- erdos.renyi.game(50, 150, type="gnm") 
g100 <- erdos.renyi.game(100, 300, type="gnm") 
g500 <- erdos.renyi.game(500, 1500, type="gnm") 
nodes <- c(25,50,100,500) 
lrgclq <- c(clique.number(g25),
            clique.number(g50), 
            clique.number(g100),
            clique.number(g500)) 
numclq <- c(length(cliques(g25,min=3)), 
            length(cliques(g50,min=3)),
            length(cliques(g100,min=3)), 
            length(cliques(g500,min=3))) 
clqinfo <- data.frame(Nodes=nodes,
                      Largest=lrgclq, 
                      Number=numclq)
View(clqinfo)
#네트워크가 노드수가 많아져도 클리크 수는 별로 늘지 않고
#가장 큰 클리크 크기도 별로 커지지 않는다. 
#클리크는 네트워크가 커질 수록 희소해진다....

#K-Cores
#a K-Core : a maximal subgraph whrere each vertex is connected
#           to at least k other vertices in the subgraph
#좋은 점 : nested, x겹침, 알아보기 쉬움.

library(statnet)
library(intergraph)
data(DHHS)
#DHHS : collaoration ties among tobaccon control experts 
#working across
#various  institutes and agecies 
#within the Department of Helath and Human Servieces in 2005
network.size(DHHS)
plot(DHHS)
list.edge.attributes(DHHS)
collabLab<-get.edge.attribute(DHHS,"collab")
gplot(DHHS,usearrows = FALSE, edge.col = DHHS %e% 'collab',
      label = collabLab,mode = "fruchtermanreingold")
iDHHS <-asIgraph(DHHS)
plot(iDHHS)
graph.density(iDHHS)
iDHHS <- subgraph.edges(iDHHS,E(iDHHS)[collab > 2])
graph.density(iDHHS)
gplot(DHHS,usearrows = FALSE, edge.col = DHHS %e% 'collab',
      label = collabLab,mode = "fruchtermanreingold")
plot(iDHHS,edge.color=(E(iDHHS)$collab))
network.size(iDHHS)


coreness <- graph.coreness(iDHHS)
table(coreness)
maxCoreness <- max(coreness)
maxCoreness


list.vertex.attributes(iDHHS)
nDHHS<-asNetwork(iDHHS)
list.vertex.attributes(nDHHS)
Vname <- get.vertex.attribute(nDHHS,'vertex.names') 
V(iDHHS)$name <- Vname 
V(iDHHS)$color <- coreness + 1 
op <- par(mar = rep(0, 4)) 
plot(iDHHS,vertex.label.cex=0.8) 
par(op)


colors<-rainbow(maxCoreness)
View(colors)
op<-par(mar = rep(0,4))
plot(iDHHS,vertex.label=coreness,
     vertex.color=colors[coreness])

V(iDHHS)$name <- coreness 
V(iDHHS)$color <- colors[coreness] 
iDHHS1_6 <- iDHHS 
iDHHS2_6 <- induced.subgraph(iDHHS, vids=which(coreness > 1)) 
iDHHS3_6 <- induced.subgraph(iDHHS, vids=which(coreness > 2)) 
iDHHS4_6 <- induced.subgraph(iDHHS, vids=which(coreness > 3))
iDHHS5_6 <- induced.subgraph(iDHHS, vids=which(coreness > 4)) 
iDHHS6_6 <- induced.subgraph(iDHHS, vids=which(coreness > 5))

lay <- layout.fruchterman.reingold(iDHHS) 
op <- par(mfrow=c(3,2),mar = c(3,0,2,0)) 
plot(iDHHS1_6,layout=lay,main="All k-cores") 
plot(iDHHS2_6,layout=lay[which(coreness > 1),], 
     main="k-cores 2-6") 
plot(iDHHS3_6,layout=lay[which(coreness > 2),], 
     main="k-cores 3-6") 
plot(iDHHS4_6,layout=lay[which(coreness > 3),], 
     main="k-cores 4-6") 
plot(iDHHS5_6,layout=lay[which(coreness > 4),], 
     main="k-cores 5-6")
plot(iDHHS6_6,layout=lay[which(coreness > 5),], 
     main="k-cores 6-6") 
par(op)

#modularity
#Modularity is a measure of the structure of the network,
#speci???cally the extent to which nodes exhibit clustering 
#where there is greater density within the clusters 
#and less density between them 
#-1/2 ~ 1 까지

g1 <- graph.formula(A-B-C-A,D-E-F-D,G-H-I-G,A-D-G-A) 
V(g1)$grp_good <- c(1,1,1,2,2,2,3,3,3) 
V(g1)$grp_bad <- c(1,2,3,2,3,1,3,1,2)

op <- par(mar=c(5,5,5,5),mfrow=c(1,2)) 
plot(g1,vertex.color=(V(g1)$grp_good), vertex.size=20,
     main="Good Grouping") 
plot(g1,vertex.color=(V(g1)$grp_bad), vertex.size=20, 
     main="Bad Grouping") 
par(op)

modularity(g1,V(g1)$grp_good)
modularity(g1,V(g1)$grp_bad)

data(DHHS)
iDHHS <-asIgraph(DHHS)
table(V(iDHHS)$agency)
V(iDHHS)[1:10]$agency
V(iDHHS)[1:20]$agency
modularity(iDHHS,(V(iDHHS)$agency+1))


data(Moreno)
iMoreno<-asIgraph(Moreno)
list.vertex.attributes(Moreno)
table(V(iMoreno)$gender)
V(iMoreno)$gender
modularity(iMoreno,V(iMoreno)$gender)

data(Facebook)
list.vertex.attributes(Facebook)
plot(Facebook)
levels(factor(V(Facebook)$group))
V(Facebook)$group
grp_num <- as.numeric(factor(V(Facebook)$group))
grp_num
op <- par(mar=c(5,5,5,5))
plot(Facebook,vertex.color=grp_num, vertex.size=20,
     main="Facebook Group") 
modularity(Facebook,grp_num)

#Modularity를 계산하려면 
#알고자 하는 그 속성이 
#노드별로 숫자로 표현되어 있어야 하는군요.....

#끝. 

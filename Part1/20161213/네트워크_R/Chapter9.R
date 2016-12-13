#Chapter 9 Affiliation Network

## : a network where the members are affiliated with one another
##based on co-membership in a group, or co-participation 
##in some type of event.
## 소속, 가맹, 가입....



C1 <- c(1,1,1,0,0,0) 
C2 <- c(0,1,1,1,0,0) 
C3 <- c(0,0,1,1,1,0) 
C4 <- c(0,0,0,0,1,1) 
aff.df <- data.frame(C1,C2,C3,C4) 
row.names(aff.df) <- c("S1","S2","S3","S4","S5","S6")

##affiliation networks = two-mode networks
##bipartite network 

library(igraph)
bn <- graph.incidence(aff.df)
plt.x <- c(rep(2,6),rep(4,4)) 
plt.y <- c(7:2,6:3) 
lay <- as.matrix(cbind(plt.x,plt.y))
shapes <- c("circle","square") 
colors <- c("blue","red") 
plot(bn,vertex.color=colors[V(bn)$type+1], 
     vertex.shape=shapes[V(bn)$type+1], 
     vertex.size=10,vertex.label.degree=-pi/2, 
     vertex.label.dist=1.2,vertex.label.cex=0.9, layout=lay)
get.incidence(bn)
V(bn)$type
V(bn)$name
V(bn)[V(bn)$name=="S1"]$type
V(bn)$type[V(bn)$name=="S1"]

#Edge Lists 가지고 Affiliation Network 만들기

el.df <- data.frame(rbind(c("S1","C1"), 
                          c("S2","C1"), 
                          c("S2","C2"), 
                          c("S3","C1"), 
                          c("S3","C2"), 
                          c("S3","C3"), 
                          c("S4","C2"), 
                          c("S4","C3"), 
                          c("S5","C3"), 
                          c("S5","C4"), 
                          c("S6","C4"))) 
el.df
bn2<-graph.data.frame(el.df, directed = FALSE)
bn2

V(bn2)$name
V(bn2)$type
V(bn2)$type <- V(bn2)$name %in% el.df[,1]
V(bn2)$type
bn2

graph.density(bn)==graph.density(bn2)


#그래프로 띄워보기
shapes <- c("circle","square") 
colors <- c("blue","red") 
plot(bn,vertex.color=colors[V(bn)$type+1], 
     vertex.shape=shapes[V(bn)$type+1], 
     vertex.size=10,vertex.label.degree=-pi/2, 
     vertex.label.dist=1.2,vertex.label.cex=0.9)

# 프로젝션!!
bn.pr <- bipartite.projection(bn)
bn.pr
##프로젝트1은 학생들, ##프로젝트2는 클래스

graph.density(bn.pr$proj1)
graph.density(bn.pr$proj2)
bn.student <- bn.pr$proj1
bn.class <- bn.pr$proj2

get.adjacency(bn.student,sparse = FALSE,attr="weight")
get.adjacency(bn.class,sparse = FALSE,attr="weight")

shapes <- c("circle","square") 
colors <- c("blue","red") 
op <- par(mfrow=c(1,2)) 
plot(bn.student,vertex.color="blue", 
     vertex.shape="circle",main="Students", 
     edge.width=E(bn.student)$weight*2, 
     vertex.size=15,vertex.label.degree=-pi/2, 
     vertex.label.dist=1.2,vertex.label.cex=1) 
plot(bn.class,vertex.color="red", 
     vertex.shape="square",main="Classes", 
     edge.width=E(bn.student)$weight*2, 
     vertex.size=15,vertex.label.degree=-pi/2, 
     vertex.label.dist=1.2,vertex.label.cex=1) 
par(op)


#헐리우드 영화배우로 예를 보여드립니다...
data(hwd)
h1 <- hwd
h1
V(h1)$name[1:10]
V(h1)$type[1:10]
V(h1)$IMDBrating[1:10]
V(h1)$name[155:165]
##160번까지가 '영화'노드이고, 
##161번부터는 '영화배우' 노드이다...

h1
##1365개의 노드, 1600개의 타이
##영화 160편   영화배우 1205명...
##각 영화별로 10명씩만 영화배우를 연결지어 놓음....

V(h1)$shape <- ifelse(V(h1)$type==TRUE,"square","circle")
V(h1)$shape[1:10]
h1
V(h1)$color <- ifelse(V(h1)$type==TRUE,"red","lightblue")
h1
##영화는 type이 TRUE, 조건문에 의해 빨간 네모로 표시될 예정
##영화배우는 밝은 파랑 동그라미로 표시될 예정

#서브그래프 만들기
h2 <- subgraph.edges(h1, E(h1)
                     [inc(V(h1)[name %in%
                                  c("The Wolf of Wall Street",
                                    "Gangs of New York", 
                                    "The Departed")])]) 
plot(h2, layout = layout_with_kk)
##inc펑션이 [] 속 조건에 맞는 노드들을 챙기고 
##그와 관련된 엣지들로 가져와줌...
##layout_with_kk는 카마다카와이 알고리즘으로 보여주라는 뜻
plot(h2)

graph.density(h1)
table(degree(h1,v=V(h1)[type==FALSE]))
mean(degree(h1,v=V(h1)[type==FALSE]))


V(h1)$deg <- degree(h1)
V(h1)[type==FALSE & deg > 4] $ name
busy_actor <- data.frame(cbind(
  Actor = V(h1)[type==FALSE & deg > 4]$name, 
  Movies = V(h1)[type==FALSE & deg > 4]$deg
)) 
busy_actor[order(busy_actor$Movies,decreasing=TRUE),]



for (i in 161:1365) {
  V(h1)[i]$totrating <- sum(V(h1)[nei(i)]$IMDBrating) 
} 
##nei(i)함수는 (i)노드가 연결되어 있는 이웃노드를 가져옴.

max(V(h1)$totrating,na.rm=TRUE)

pop_actor <- data.frame(cbind( 
  Actor = V(h1)[type==FALSE & totrating > 40]$name, 
  Popularity = V(h1)[type==FALSE & totrating > 40]$totrating))
pop_actor[order(pop_actor$Popularity,decreasing=TRUE),]


for (i in 161:1365) { 
  V(h1)[i]$avgrating <- mean(V(h1)[nei(i)]$IMDBrating) }
num <- V(h1)[type==FALSE]$deg 
avgpop <- V(h1)[type==FALSE]$avgrating 
summary(lm(avgpop ~ num))
##lm은 Fitting linear model

scatter.smooth(num,avgpop,col="lightblue", 
               ylim=c(2,10),span=.8, 
               xlab="Number of Movies", 
               ylab="Avg. Popularity")


h1.pr <- bipartite.projection(h1) 
h1.act <- h1.pr$proj1 
h1.mov <- h1.pr$proj2 
h1.act
h1.mov


op <- par(mar = rep(0, 4)) 
plot(h1.mov,vertex.color="red", vertex.shape="circle", 
     vertex.size=(V(h1.mov)$IMDBrating)-3, vertex.label=NA)

graph.density(h1.mov)
no.clusters(h1.mov)
clusters(h1.mov)$csize
table(E(h1.mov)$weight)

h2.mov <- induced.subgraph(h1.mov, 
                           vids=clusters(h1.mov)$membership==1)
plot(h2.mov,vertex.color="red", 
     edge.width=sqrt(E(h1.mov)$weight), vertex.shape="circle", 
     vertex.size=(V(h2.mov)$IMDBrating)-3, vertex.label=NA)
table(graph.coreness(h2.mov))
##k-cores 구하기
h3.mov <- induced.subgraph(h2.mov, 
                           vids=graph.coreness(h2.mov)>4) 
h3.mov
plot(h3.mov,vertex.color="red", vertex.shape="circle", 
     edge.width=sqrt(E(h1.mov)$weight), vertex.label.cex=0.7,
     vertex.label.color="darkgreen", vertex.label.dist=0.3, 
     vertex.size=(V(h3.mov)$IMDBrating)-3)
graph.density(h3.mov)


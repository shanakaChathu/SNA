
#Find comunities in largeest graph 
community=function(graph)
{
#Edge betweenness based community detection 
ebc = edge.betweenness.community(graph,directed=F,modularity = T,membership = T)
mods1= sapply(0:ecount(graph),
              function(i)
              {
                g2 = delete.edges(graph, ebc$removed.edges[seq(length=i)])
                cl=  clusters(g2)$membership
                modularity(graph,cl)
              }
) 
mods1
plot(mods1)
g2=delete.edges(graph, ebc$removed.edges[seq(length=which.max(mods1)-1)])
g2
}
communities=community(uLargeGraph)


#Plotting communities in the large graph 
communityPlot=function(graph,communities)
{
  
  V(graph)$color=clusters(graph)$membership
  graph$layout=layout.fruchterman.reingold
  
  #Plotting 
  clp = cluster_label_prop(communities)
  plot(clp,graph,vertex.label=NA,vertex.size=1)  
  
}
communityPlot(uLargeGraph,communities)


#Assign communityId's into dtaset
CommunityAssign=function(cGraph)
{

  cGraph=communities
dg=decompose.graph(cGraph)
nclu=nrow(summary(dg))
communityData=data.frame()

   for( i in 1:nclu )
   {
        gra=dg[[i]]
        edlist=as_edgelist(gra)
        anumber=edlist[,1]
        bnumber=edlist[,2]
        totcallsM=E(gra)$totcallsM
        totcallsD=E(gra)$totcallsD
        totcallsN=E(gra)$totcallsN
        totcallsWD=E(gra)$totcallsWD
        totcallsWE=E(gra)$totcallsWE
        totDurM=E(gra)$totDurM
        totDurD=E(gra)$totDurD
        totDurN=E(gra)$totDurN
        totDurWD=E(gra)$totDurWD
        totDurWE=E(gra)$totDurWE
        community=i
      
        df=data.frame(anumber,bnumber,totcallsM,totcallsD,totcallsN,totcallsWD,totcallsWE,
                      totDurM,totDurD,totDurN,totDurWD,totDurWE,community)
       communityData=rbind(communityData,df)
   }
save(communityData,file='communityData.RData')
communityData
}

communityData=CommunityAssign(communities)




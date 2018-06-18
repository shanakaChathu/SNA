#Load R object 
load("data.RData")
load("ugraph.RData")
load("dgraph.RData")

#Function to build initial basic network (directed or undirected)
Network = function(data,anumberI,bnumberI,dir)
{
 
  #Convert mobile numbers into characters
  data[,anumberI]=as.character(data[,anumberI])
  data[,bnumberI]=as.character(data[,bnumberI])
  
  #Budiling adjacency matrix
  dataMatrix=as.matrix(data)
  
  #Undirected network graph
  graph=graph.edgelist(dataMatrix[,c(anumberI,bnumberI)],directed=dir)
  
  #Setting edge attributes 
  E(graph)$totcallsM=as.numeric(data[,3])
  E(graph)$totcallsD=as.numeric(data[,4])
  E(graph)$totcallsN=as.numeric(data[,5])
  E(graph)$totcallsWD=as.numeric(data[,6])
  E(graph)$totcallsWE=as.numeric(data[,7])
  E(graph)$totDurM=as.numeric(data[,8])
  E(graph)$totDurD=as.numeric(data[,9])
  E(graph)$totDurN=as.numeric(data[,10])
  E(graph)$totDurWD=as.numeric(data[,11])
  E(graph)$totDurWE=as.numeric(data[,12])
  
  graph=simplify(graph,remove.multiple = T, remove.loops = T,
  edge.attr.comb=c(totcallsM='sum',totcallsD='sum',totcallsN='sum',totcallsWD='sum',totcallsWE='sum',
                   totDurM='sum',totDurD='sum',totDurN='sum',totDurWD='sum',totDurWE='sum')
  )
  
  if(dir==TRUE)
  {
      dGraph=graph
      save(dGraph,file='dGraph.RData') 
      dGraph
  }
  else
  {
     uGraph=graph
     save(uGraph,file='uGraph.RData') 
     uGraph
  }
}
graph=Network(data,1,2,FALSE)

#--------------------------------End of function Network----------------------------------

#Building Initial network graph according to the directed or undirected
NetworkGraph=function(dir)
{
   if(dir==TRUE)
   {
     load("dGraph.RData")
     plot(dGraph,vertex.label=NA,vertex.size=1)
   }
  else
  {
    load("uGraph.RData")
    plot(uGraph,vertex.label=NA,vertex.size=1)
  }

}
NetworkGraph(FALSE)
#--------------------------------End of function NetworkGraph----------------------------------




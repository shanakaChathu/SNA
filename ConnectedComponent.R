#Load object
load("uConnectedComp.RData")
load("uLargeGraph.RData")
load("uGraph.RData")

#No of users in connected components 
connectedComp=function(graph)
{
  totUsers=length(V(graph))
  clu=clusters(graph)
  
  cluSize=clu$csize
  perc=(cluSize/totUsers)*100
  df=data.frame(cluSize)
  df=data.frame(row.names(df),df,perc)
  colnames(df)=c('componentID','nUsers','percentage')
  row.names(df)=NULL
  df=df[order(-df$percentage),] 
  df
}

uConnectedComp=connectedComp(uGraph)
save(uConnectedComp,file="uConnectedComp.RData")


#--------------------------End of function connectedComp-----------------------------------------------------

#Extracts connected components(CC) according to the "no of users" in the CC

connectedCompLimit=function(graph,lower,upper)
{
  clu=clusters(graph)
  
  cluSize=clu$csize
  df=data.frame(cluSize)
  df=data.frame(row.names(df),df)
  colnames(df)=c('CluNo','nUsers')
  row.names(df)=NULL
  
  df=df[(df$nUsers>=lower),]
  df=df[df$nUsers<=upper,]
  
  compID=as.numeric(as.character(df$CluNo))
  compID
}

connectedCompLimit(uGraph,10,20)
#--------------------------End of function connectedCompLimit-----------------------------------------------------

#Select required connected components  using componentID with user defined treshold 
#for "percentage number of users"

bigComponents=function(graph,treshold)
{
  dg=decompose.graph(graph)
  df=connectedComp(graph)
  cid=df[df$percentage>=treshold,1]
  compID=as.numeric(as.character(cid))
  compID
}

ID=bigComponents(uGraph,40)
#Extracting largeest connected componentt from graph
dg=decompose.graph(uGraph)
uLargeGraph=dg[[ID]]
save(uLargeGraph,file='uLargeGraph.RData')

#--------------------------End of function bigComponents-----------------------------------------------------

#Extracting large connected component and plotting with colors a
largeComp=function(graph)
{
  cl=clusters(graph)
  nodes = which(cl$membership == which.max(cl$csize))
  V(graph)[nodes]$color = "Green"
  rglplot(graph,vertex.label=NA,vertex.size=2) 
}
largeComp(uLargeGraph)


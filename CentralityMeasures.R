#Loading objects 
load('uLargeGraphCent.RData')

#Function to calcuate the centrality measures. 
centrality = function(graph)
{
  #calculating 4 centrality measures
  a=degree(graph)
  b=betweenness(graph)
  c=closeness(graph)
  d=eigen_centrality(graph)$vector
  
  cent=data.frame(a,b,c,d)
  cent=data.frame(row.names(cent),cent)
  rownames(cent)=NULL
  colnames(cent)=c('Subsnumber','Degree','Betweenness','closeness','eigenvector')
  cent
  
}

uCent=centrality(uLargeGraph) 
uLargeGraphCent=uCent
save(uLargeGraphCent,file='uLargeGraphCent.RData')

head(uLargeGraphCent)







load("communityData.RData")
head(communityData)
table(communityData$community)

#Multi bar chart accoering to the community 
communityAnalysis=function(data,ind)
{
      data=communityData
      ind=c(11,12,13)
      
      sub=communityData[,ind]
      melted=melt(sub,id.vars=c("community"))
      melt.data=ddply(melted,c("community","variable"),summarise,aggregate=median(value))
      
      ggplot(melt.data, aes(community,aggregate )) +   
      geom_bar(aes(fill = variable), position = "dodge", stat="identity")
}

#total calls for Mor,Day,Night
communityAnalysis(communityData,c(3,4,5))

#total calls for weekday and weekend
communityAnalysis(communityData,c(6,7))

#total duration for Mor,Day,Night
communityAnalysis(communityData,c(8,9,10))

#total duration for weekday and weekend
communityAnalysis(communityData,c(11,12))





#Load CSV file
preprocess=function()
{
  #Loading 
  data=file.choose()
  data=read.csv(data,header=TRUE)
  
  #Cleaning 
  data[data=='']=NA
  data=data[complete.cases(data),]
  
  #Save data object 
  save(data,file="data.Rdata")
  data
}

data=preprocess()




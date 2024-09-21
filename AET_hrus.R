# What this file is trying to do is read in the top row of a giant csv file and find out which columns
# are associated with the hrus in our region. 

library(data.table)

setwd("C:/Users/Andrea/OneDrive - University of Waterloo/FEWTures Data/Model Data")

# find hru ids
Carb_hru_file<-read.csv("CARB_hrus_final.csv")
HRUs<-Carb_hru_file$hru_id_nat
head(Carb_hru_file)

#load in aet (big file)

aet <- read.csv("nhru_hru_actet.csv",header=FALSE,nrows=1)
                
# loop to find needed id's

aet_ID<-list()
for(i in 1:length(HRUs))
  for(j in 1:length(aet))
    if(HRUs[i]==aet[j]){
      aet_ID[[i]]=j}

write.csv(aet_ID, 'aet_ID.csv')

#aet_ID<-read.csv("aet_ID.csv")

# get gw flow data for each segment from file and append to list
loc<-as.numeric(unlist(aet_ID)) #converting list to vector
actet<-fread("nhru_hru_actet.csv", select = c(loc[1]))

for(x in 2:length(aet_ID)) {
  actet2<-fread("nhru_hru_actet.csv", select = c(loc[x]))
  actet<-cbind(actet, actet2)
  }
  
#write the file
write.csv(actet, 'actet_carb.csv')


    
    
    
    
    

    















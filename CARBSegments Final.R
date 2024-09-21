library("ncdf4")

setwd("C:/Users/lilia/OneDrive - University of Waterloo/Model Data")

# find segment ids
Carb_seg_file<-read.csv("segments_CARB.csv")
Segments<-Carb_seg_file$seg_id_nat
Segment <- c(1,2)
head(Carb_seg_file)
#View(start)

#loading shapefile
nc <- ncdf4::nc_open("seg_outflow.nc") # you might have a different directory structure
v3 <- nc$var[[1]]
varsize <- v3$varsize
ndims <- v3$ndims
nsegments <- varsize[Segments]
timeval <- ncdf4::ncvar_get( nc, v3$dim[[2]]$name, start=1, count=varsize[2])


for( i in 1:length(Segments)) {       # Loop from 1 to length of list
   outdata<-data.frame(Segments[i])
   colnames(outdata)<-c("Segment ID")}


Dates <- as.Date("1980-10-01") + timeval
for (t in Dates){
  currentdate<-Dates[t]
  for (i in 1:length(Segments)) { #stream segment counter
    start<-c(Segments[i], t) # row # for each stream
    count<-c(1,1) # get one value
    Streamflow<- ncdf4::ncvar_get( nc, v3, start = start, count = count) # get the value
    tempdata<-rbind(Dates, Streamflow) # add the value to the column for that time
  colnames(tempdata)<-c(Dates) }}# name the column after adding flow for all segments
  outdata<-cbind(outdata, tempdata) # add column to outdata

ncdf4::nc_close(nc)
write.csv(outdata, "CARBstreamflow_final.csv")
    
 
  
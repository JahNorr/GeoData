

library(ggplot2)

#all_trails[all_trails$segment_id]

####################################
#
#   set up the area of interest
#
area_of_interest<-"flathead"
area_id<-getTrailAreaID(area_of_interest)

########################################
#
#   get the trails for this area
#
trails<-all_trails[all_trails$area_id==area_id,]

########################################
#
#   get the unique trail names for this area
#
trail_ids<-unique(trails[,"ID"])

########################################
#
#   get the number of segments for
#   each trail and add it to the unique 
#   names to create a data.frame
#
seg_counts<-sapply(trail_ids,function(x) {
    max(trails[trails$ID==x,"segment_id"])
})

trail_seg_counts<-data.frame(trail_id=trail_ids,seg_count=seg_counts)

###########################################
#
#   find the trails with only one segment
#
seg_trails<-as.character(trail_seg_counts[,"trail_id"])

#################################################
#
#   eliminate trails that are already saved
#
area_trails_accepted<-as.character(df_trails_accepted[df_trails_accepted$area_id==area_id,"ID"])
ok_trails<-which(!(seg_trails %in% area_trails_accepted))

trails_to_check<-as.character(seg_trails[ok_trails])
seg_count<-as.integer(trail_seg_counts[ok_trails,"seg_count"][1])
#trail_id<-trails_to_check[1]
#################################################
#
#   plot it and store it
#
trail_id<-trails_to_check[1]
trail_name<-unique(as.character(trails[trails$ID==trail_id,"NAME"]))

if (F) {
reverse_segments(area_id,trail_id,seg_count)
}

print(paste("trail ",trail_id," (",trail_name,")  has ",seg_count," segments",sep=""))
latlons_plot<-plot_trail(area_of_interest,trail_id,write_segs=T)


yn_accept<-FALSE
if (yn_accept) {
    accept_trail(area_of_interest,trail_id,latlons_plot)
}


#trail_id<-"353"
plot_trail_saved(area_of_interest,trail_id)


reverse_segments<-function(area_id,trail_id,seg_count) {
    rearrange<-read.csv("./data/trails/arrange_points.csv",header = T)
    rearrange<-rearrange[!(rearrange$trail_id==trail_id && rearrange$area_id==area_id),]
    
    df<-as.data.frame(matrix(ncol = ncol(rearrange),nrow=seg_count))
    colnames(df)<-colnames(rearrange)
    for(i in 1:seg_count) {
        df[i,1]=area_id
        df[i,2]=trail_id
        df[i,3]=i
        df[i,4]=seg_count+1-i
    }
    
    rearrange<-rbind(rearrange,df)
    write.csv(rearrange,file="./data/trails/arrange_points.csv")
}

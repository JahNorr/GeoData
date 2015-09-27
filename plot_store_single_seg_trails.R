

library(ggplot2)

#all_trails[all_trails$segment_id]

####################################
#
#   set up the area of interest
#
area_of_interest<-"kootenai"
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
single_seg_trails<-as.character(trail_seg_counts[trail_seg_counts$seg_count==1,"trail_id"])

#################################################
#
#   eliminate trails that are already saved
#
area_trails_accepted<-as.character(df_trails_accepted[df_trails_accepted$area_id==area_id,"ID"])
ok_trails<-which(!(single_seg_trails %in% area_trails_accepted))

trails_to_check<-as.character(single_seg_trails[ok_trails])

#trail_id<-trails_to_check[1]

#################################################
#
#   plot it and store it
#

sapply(trails_to_check[1:28],function(x) {
    latlons_plot<-plot_trail(area_of_interest,x,write_segs=F)
    accept_trail(area_of_interest,x,latlons_plot)
})


latlons_plot<-plot_trail(area_of_interest,trail_id,write_segs=T)
# 
# 
yn_accept<-FALSE
if (yn_accept) {
    accept_trail(area_of_interest,trail_id,latlons_plot)
}


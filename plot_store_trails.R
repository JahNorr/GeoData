

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
seg_trails<-as.character(trail_seg_counts[,"trail_id"])

#################################################
#
#   eliminate trails that are already saved
#

repeat {
    
    area_trails_accepted<-as.character(df_trails_accepted[df_trails_accepted$area_id==area_id,"ID"])
    #################################################
    #   eliminate trails that are marked as skip
    #
    skip_trails<-df_skip[df_skip$area_id==area_id,"trail_id"]
    ok_trails<-which(!(seg_trails %in% area_trails_accepted) & !(seg_trails %in% skip_trails))
    trails_to_check<-as.character(seg_trails[ok_trails])
    seg_count<-as.integer(trail_seg_counts[ok_trails,"seg_count"][1])
    #################################################
    #   plot it and store it
    #
    trail_id<-trails_to_check[1]
    trail_name<-unique(as.character(trails[trails$ID==trail_id,"NAME"]))
    
    if (F) {
        # if (check_reverse(area_id,trail_id,seg_count)) {
        reverse_segments(area_id,trail_id,seg_count)
        
        index<-1
        split_segments(area_id,trail_id,seg_count,index)
        # }
    }
    
    arrange_segments(area_id,trail_id)
    
    print(paste("trail ",trail_id," (",trail_name,")  has ",seg_count," segments",sep=""))
    latlons_plot<-plot_trail(area_of_interest,trail_id,write_segs=T)
    
    cat("\n","This trail ok (y[es]/n[o]/s[kip]/r[everse]/q[uit]):","\n") # prompt 
    input<-scan(n=1,what="character") 
    
    if (input[1]=="q") break
    else if (input[1]=="y") {
        accept_trail(area_of_interest,trail_id,latlons_plot)
    }
    else if (input[1]=="s") {
        add_skip(area_id,trail_id)
    }
    else if (input[1]=="r") {
        reverse_segments(area_id,trail_id,seg_count)
    }
}


plot_trail_saved(area_of_interest,trail_id)



all_trail_latlons[all_trail_latlons$area_id==area_id & 
                      all_trail_latlons$trail_id==trail_id & 
                      all_trail_latlons$segment_id==3,"segment_id"]<-1



add_skip<-function(area_id,trail_id) {
    df_skip_add<-data.frame(area_id=area_id,trail_id=trail_id)
    df_skip<<-rbind(df_skip,df_skip_add)
}
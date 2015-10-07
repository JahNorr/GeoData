

library(ggplot2)

#all_trails[all_trails$segment_id]

####################################
#
#   set up the area of interest
#
area_of_interest<-"kootenai"
forest_id<-getForestID(area_of_interest)

########################################
#
#   get the trails for this area
#
trails<-all_trails[all_trails$forest_id==forest_id,]

########################################
#
#   get the unique trail names for this area
#
trail_nums<-unique(trails[,"ID"])

########################################
#
#   get the number of segments for
#   each trail and add it to the unique 
#   names to create a data.frame
#
seg_counts<-sapply(trail_nums,function(x) {
    max(trails[trails$ID==x,"segment_id"])
})

trail_seg_counts<-data.frame(trail_num=trail_nums,seg_count=seg_counts)

###########################################
#
#   find the trails with only one segment
#
seg_trails<-as.character(trail_seg_counts[,"trail_num"])

#################################################
#
#   eliminate trails that are already saved
#
check_skip()

repeat {
    fixed<-F
    
    area_trails_accepted<-as.character(df_trails_accepted[df_trails_accepted$forest_id==forest_id,"ID"])
    #################################################
    #   eliminate trails that are marked as skip
    #
    skip_trails<-df_skip[df_skip$forest_id==forest_id,"trail_num"]
    ok_trails<-which(!(seg_trails %in% area_trails_accepted) & !(seg_trails %in% skip_trails))
    trails_to_check<-as.character(seg_trails[ok_trails])
    seg_count<-as.integer(trail_seg_counts[ok_trails,"seg_count"][1])
    #################################################
    #   plot it and store it
    #
    trail_num<-trails_to_check[1]
    trail_name<-unique(as.character(trails[trails$ID==trail_num,"NAME"]))
    
    if (F) {
        # if (check_reverse(forest_id,trail_num,seg_count)) {
        reverse_segments(forest_id,trail_num,seg_count)
        
        index<-1
        split_segments(forest_id,trail_num,seg_count,index)
        # }
    }
    
    fixed<-arrange_segments(forest_id,trail_num)
    
    if(!fixed) fixed<-arrange_segments_closest(forest_id,trail_num)
    
    print(paste("trail ",trail_num," (",trail_name,")  has ",seg_count," segments",sep=""))
    latlons_plot<-plot_trail(area_of_interest,trail_num,write_segs=T)
    
    cat("\n","This trail ok (y[es]/n[o]/s[kip]/c[losest]/r[everse]/q[uit]):","\n") # prompt 
    input<-scan(n=1,what="character") 
    
    if (input[1]=="q") break
    else if (input[1]=="y") {
        accept_trail(area_of_interest,trail_num,latlons_plot)
    }
    else if (input[1]=="s") {
        add_skip(forest_id,trail_num)
    }
    else if (input[1]=="r") {
        reverse_segments(forest_id,trail_num,seg_count)
    }
  }
}


plot_trail_saved(area_of_interest,trail_num)



all_trail_latlons[all_trail_latlons$forest_id==forest_id & 
                      all_trail_latlons$trail_num==trail_num & 
                      all_trail_latlons$segment_id==3,"segment_id"]<-1


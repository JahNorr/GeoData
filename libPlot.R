
library(ggplot2)


plot_trail<-function(area_of_interest,trail_num,write_segs=F) {
#    df_rearrange_trails<-read.csv("./data/trails/arrange_points.csv",header = T)
    
    forest_id<-getForestID(area_of_interest)
    
    
    latlons<-get_trail_latlons(forest_id,trail_num)
    
    
    seg_count<-max(latlons$segment_id)
    
#    arrange<-df_rearrange_trails[(df_rearrange_trails$trail_num==trail_num) & (df_rearrange_trails$forest_id==forest_id),]
    
    if(write_segs) {
        write_segment_ends(latlons,key="in",seg_count = seg_count)
    }
    
    
    latlons$segment_id<-abs(latlons$segment_id)
    
    ord<-order(latlons$segment_id,as.integer(rownames(latlons)))
    
    latlons_plot<-latlons[ord,]
    
    
    if(write_segs) {
        write_segment_ends(latlons_plot,key="out",seg_count = seg_count)
    }
    
    trail_map <- ggplot(latlons_plot, aes(x=longitude, y=latitude)) +
        geom_path( colour="blue")
    
    final_map<-trail_map + coord_map() + ggtitle(trail_num)
    
    plot(final_map)
    
    latlons_plot
}

plot_trail_saved<-function(area_of_interest,trail_num) {
    if(is.numeric(area_of_interest)) {
        forest_id<-area_of_interest
    } else {
        forest_id<-getForestID(area_of_interest)
    }
    
    df_latlons<-df_trail_coords_accepted
    
#    trail_name<-df_trails_accepted[df_trails_accepted$trail_num==trail_num,"NAME"]
    trail_name<-df_trails_accepted[(df_trails_accepted$ID==trail_num) & (df_trails_accepted$forest_id==forest_id),"NAME"]
    
    
    lloi<-(df_latlons$trail_num==trail_num) & (df_latlons$forest_id==forest_id)
    latlons<-df_latlons[lloi,]
    
#    ord<-order(latlons$segment_id,as.integer(rownames(latlons)))
    
    latlons_plot<-latlons#[ord,]
    
    trail_map <- ggplot(latlons_plot, aes(x=longitude, y=latitude)) +
        geom_path( colour="blue")
    
    final_map<-trail_map + coord_map() + ggtitle(paste(trail_name,"\n",trail_num,sep=""))
    
    plot(final_map)
    
}


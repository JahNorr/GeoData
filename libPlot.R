
library(ggplot2)

get_trail_latlons<-function(area_id,trail_id) {
    
    df_latlons<-all_trail_latlons
       
    lloi<-(df_latlons$trail_id==trail_id) & (df_latlons$area_id==area_id)
    latlons<-df_latlons[lloi,]
    
}

plot_trail<-function(area_of_interest,trail_id,write_segs=F) {
    df_rearrange_trails<-read.csv("./data/trails/arrange_points.csv",header = T)
    
    area_id<-getTrailAreaID(area_of_interest)
    
    
    latlons<-get_trail_latlons(area_id,trail_id)
    
    
    seg_count<-max(latlons$segment_id)
    
    arrange<-df_rearrange_trails[(df_rearrange_trails$trail_id==trail_id) & (df_rearrange_trails$area_id==area_id),]
    
    if(write_segs) {
        write_segment_ends(latlons,key="in",seg_count = seg_count)
    }
    
    
    mapply(function(x,y) {
        inc<-latlons["segment_id"]==x
        #print(inc)
        latlons[inc,"segment_id"]<<-(-y)
        
    },arrange$segment_old,arrange$segment_new)
    
    
    latlons$segment_id<-abs(latlons$segment_id)
    
    ord<-order(latlons$segment_id,as.integer(rownames(latlons)))
    
    latlons_plot<-latlons[ord,]
    
    
    if(write_segs) {
        write_segment_ends(latlons_plot,key="out",seg_count = seg_count)
    }
    
    trail_map <- ggplot(latlons_plot, aes(x=longitude, y=latitude)) +
        geom_path( colour="blue")
    
    final_map<-trail_map + coord_map() + ggtitle(trail_id)
    
    plot(final_map)
    
    latlons_plot
}

plot_trail_saved<-function(area_of_interest,trail_id) {
    
    area_id<-getTrailAreaID(area_of_interest)
    
    df_latlons<-df_trail_coords_accepted
    
    
    
    lloi<-(df_latlons$trail_id==trail_id) & (df_latlons$area_id==area_id)
    latlons<-df_latlons[lloi,]
    
#    ord<-order(latlons$segment_id,as.integer(rownames(latlons)))
    
    latlons_plot<-latlons#[ord,]
    
    trail_map <- ggplot(latlons_plot, aes(x=longitude, y=latitude)) +
        geom_path( colour="blue")
    
    final_map<-trail_map + coord_map() + ggtitle(trail_id)
    
    plot(final_map)
    
    latlons_plot
}



all_trails<-rbind(flathead_trails,kootenai_trails)
all_trail_latlons<-rbind(flathead_latlons,kootenai_latlons)

df_in<-get_trail_latlons(area_id,trail_id)

segs_in<-get_segment_ends(df_in)
segs_in

table(all_trail_latlons[all_trail_latlons$area_id==area_id &
                            all_trail_latlons$trail_id==trail_id ,"segment_id"])

x<-arrange_segments(area_id,trail_id)
plot_trail(area_of_interest,trail_id,write_segs=T)

table(all_trail_latlons[all_trail_latlons$area_id==area_id &
all_trail_latlons$trail_id==trail_id ,"segment_id"])

df_out<-get_trail_latlons(area_id,trail_id)

segs_out<-get_segment_ends(df_out)


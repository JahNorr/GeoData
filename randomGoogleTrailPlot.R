

trail_id<-sample(x = df_trails_accepted$trail_id,size = 1 )


latlons<- df_trail_coords_accepted[df_trail_coords_accepted$trail_id==trail_id,]

google_plot_latlons(latlons)

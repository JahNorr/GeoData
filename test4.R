


trails_ids<-mapply(function(x,y) {
    df_trails_accepted[(df_trails_accepted$forest_id==x) & (df_trails_accepted$ID==y),"trail_id"]
} df_trail_coords_accepted$forest_id,df_trail_coords_accepted$trail_num)



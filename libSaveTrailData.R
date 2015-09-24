

accept_trail<-function(trails_name,trail_id,df_coords) {
    accepted_trail_data_file<-"./data/trails/acceptedTrailData.RData"
    yn_bind<-TRUE
    
    df_trails_name<-get_trail_data_name(trails_name)
    df_trails<-get(df_trails_name,envir=.GlobalEnv)
    df<-df_trails[df_trails$ID==trail_id,]
    
    df<-df[df$segment_id==1,c("ID","NAME","area_id","trail_id","segment_id")]
    
    df_coords$segment_id<-NULL
    
    if(!exists("df_trails_accepted",envir=.GlobalEnv)) {
        if(!file.exists(accepted_trail_data_file)) {
            yn_bind<-FALSE
            
        } else {
            load(accepted_trail_data_file)
        }
    }
    if(yn_bind) {
        df_trails_accepted<<-rbind(df_trails_accepted,df)
        df_trail_coords_accepted<<-rbind(df_trail_coords_accepted,df_coords)
    } else {
        df_trails_accepted<<-df
        df_trail_coords_accepted<<-df_coords
    }
       
    save(df_trails_accepted,df_trail_coords_accepted, file=accepted_trail_data_file,envir=.GlobalEnv)
    
}
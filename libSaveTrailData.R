
get_accepted_trail_data_file<-function() {
    return ("./data/trails/acceptedTrailData.RData")
}

remove_accepted_trail_data_file<-function() {
    file.remove(get_accepted_trail_data_file())
}

check_trails_accepted<-function() {
    accepted_trail_data_file<-get_accepted_trail_data_file()
    if(!exists("df_trails_accepted",envir=.GlobalEnv)) {
        if(!file.exists(accepted_trail_data_file)) {
            return (FALSE)
            
        } else {
            load(accepted_trail_data_file,envir = .GlobalEnv)
            return (TRUE)
        }
    }
    return (TRUE)
} 

accept_trail<-function(forest_id,trail_num,df_coords,trail_id,ynsave=T) {
    accepted_trail_data_file<-get_accepted_trail_data_file()
    yn_bind<-TRUE
    
    if(!("index" %in% colnames(df_coords))) {
        df_coords$index<-1:nrow(df_coords)
    }
    
#    df_trails_name<-get_trail_data_name(trails_name)
#    df_trails<-get(df_trails_name,envir=.GlobalEnv)
    df<-all_trails[all_trails$ID==trail_num & all_trails$forest_id==forest_id,]
    
#     CREATE TABLE "trails" (
#         `trail_id`	INTEGER, "trail_id" 
#         `trail_name`	TEXT, "NAME"  
#         `trail_number`	TEXT,"ID"
#         `forest_id`	INTEGER,
#         `description`	TEXT,
#         `trail_type`	TEXT,"TRAIL_TYPE" 
#         `trail_jurisdiction`	TEXT,"JURISDICTI"
#         `trail_system`	TEXT,"TRAIL_SYST"
#         `begin_terminus`	TEXT,"BEGIN_TERM"
#         `end_terminus`	TEXT,"END_TERMIN"
#         `bmp`	REAL,"BMP"  
#         `emp`	REAL, "EMP"
#         `trail_segments`	INTEGER, "segment_id"
#         `datetime_update`	TEXT
#     )
# 
#                "forest_id"    
#                "NATIONAL_T"

    df<-df[df$segment_id==1,]
    df$trail_id<-trail_id
    df_coords$segment_id<-NULL
    df_coords$trail_id<-trail_id
    
    tmp <- sapply(df, is.factor)
    df[tmp] <- lapply(df[tmp], as.character)
    
    yn_bind<-check_trails_accepted()
    
    if(yn_bind) {
        df_trails_accepted<<-rbind(df_trails_accepted,df)
        df_trail_coords_accepted<<-rbind(df_trail_coords_accepted,df_coords)
    } else {
        df_trails_accepted<<-df
        df_trail_coords_accepted<<-df_coords
    }
       
    if (ynsave) save(df_trails_accepted,df_trail_coords_accepted, file=accepted_trail_data_file,envir=.GlobalEnv)
    
}

accept_trail_OLD<-function(trails_name,trail_num,df_coords,trail_id,ynsave=T) {
    accepted_trail_data_file<-get_accepted_trail_data_file()
    yn_bind<-TRUE
    
    if(!("index" %in% colnames(df_coords))) {
        df_coords$index<-1:nrow(df_coords)
    }
    
    df_trails_name<-get_trail_data_name(trails_name)
    df_trails<-get(df_trails_name,envir=.GlobalEnv)
    df<-df_trails[df_trails$ID==trail_num,]
    
    #     CREATE TABLE "trails" (
    #         `trail_id`    INTEGER, "trail_id" 
    #         `trail_name`	TEXT, "NAME"  
    #         `trail_number`	TEXT,"ID"
    #         `forest_id`	INTEGER,
    #         `description`	TEXT,
    #         `trail_type`	TEXT,"TRAIL_TYPE" 
    #         `trail_jurisdiction`	TEXT,"JURISDICTI"
    #         `trail_system`	TEXT,"TRAIL_SYST"
    #         `begin_terminus`	TEXT,"BEGIN_TERM"
    #         `end_terminus`	TEXT,"END_TERMIN"
    #         `bmp`	REAL,"BMP"  
    #         `emp`	REAL, "EMP"
    #         `trail_segments`	INTEGER, "segment_id"
    #         `datetime_update`	TEXT
    #     )
    # 
    #                "forest_id"    
    #                "NATIONAL_T"
    
    df<-df[df$segment_id==1,]
    df$trail_id<-trail_id
    df_coords$segment_id<-NULL
    df_coords$trail_id<-trail_id
    
    tmp <- sapply(df, is.factor)
    df[tmp] <- lapply(df[tmp], as.character)
    
    yn_bind<-check_trails_accepted()
    
    if(yn_bind) {
        df_trails_accepted<<-rbind(df_trails_accepted,df)
        df_trail_coords_accepted<<-rbind(df_trail_coords_accepted,df_coords)
    } else {
        df_trails_accepted<<-df
        df_trail_coords_accepted<<-df_coords
    }
    
    if (ynsave) save(df_trails_accepted,df_trail_coords_accepted, file=accepted_trail_data_file,envir=.GlobalEnv)
    
}
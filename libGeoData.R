library(stringr) 


getNodes<-function(x,name,ynClose=T) {
    start<-paste("<",name,sep="")
    if(ynClose) start<-paste(start,">",sep="")
    end<-paste("</",name,">",sep="")
    
    p0<-gregexpr(start,x,fixed=T)
    p0<-as.vector(p0[[1]])
    len<-nchar(start)
    p0<-p0+len
    
    
    p1<-gregexpr(end,x,fixed=T)
    p1<-as.vector(p1[[1]])
    p1<-p1-1
    
    text<-x
    
    result<-mapply(function(x,y) {
        substr(text,x,y)
    },p0,p1)
    
    result
}

get_trail_coord_set<-function(coords_in) {
    #    df<-data.frame(latitude=numeric(),longitude=numeric,elevation=numeric())
    
    
    my_coords<-as.vector(str_split(string = str_trim(coords_in)," ")[[1]])
    
    len<-length(my_coords)
    df_coords_local<-data.frame(forest_id=integer(len),
                                trail_num=character(len),
                                segment_id=integer(len),
                                latitude=numeric(len),
                                longitude=numeric(len),
                                elevation=numeric(len),
                                stringsAsFactors=F
    )
    for(i in 1:len) {
        lle<-as.vector(str_split(string = my_coords[i],",")[[1]])
        df_coords_local$latitude[i]<-as.numeric(lle[2])
        df_coords_local$longitude[i]<-as.numeric(lle[1])
        df_coords_local$elevation[i]<-as.numeric(lle[3])
    }
    
    df_coords_local
}

getLatLons<-function(df_trails) {
    
    df_coords<-data.frame(forest_id=integer(),trail_num=character(),segment_id=integer(),latitude=numeric(),longitude=numeric(),elevation=numeric())
    
    len = nrow(df_trails)
    for(i in 1:len) {
        df<-get_trail_coord_set(df_trails[i,"coords"])
        df$forest_id<-df_trails[i,"forest_id"]
        df$trail_num<-as.character(df_trails[i,"ID"])
        df$segment_id<-df_trails[i,"segment_id"]
        df_coords<-rbind(df_coords,df)
    }
    
    df_coords
}

setSegments<-function(x) {
    
    len = length(x)
    result=integer(len)
    
    for(i in 1:len) {
        df_segment=x[1:i]
        ok<-which(df_segment==x[i])
        result[i]<-length(ok)
    }
    
    result
}

    
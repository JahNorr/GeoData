library(stringr) 

source("./libGeoData.R")

getTrails<-function(key,filename) {
    area_id<-getTrailAreaID(key)
    
    txt<-readChar(filename, file.info(filename)$size)
    txt<-gsub(pattern = "\t"    ,"",txt,fixed=T,)
    txt<-gsub(pattern = "\n"    ,"",txt,fixed=T,)
    
    trails<-getNodes(txt,"Placemark")
    
    trails_tmp<-trails
    
    df_trails<-data.frame()
    
    temp<-lapply(trails_tmp, function(trail) {
        
        simple<-getNodes(trail,"SimpleData",F)
        
        tmp<-gsub(pattern = "\"","",x = simple,fixed=T,)
        tmp<-gsub(pattern = "name=","",x = tmp,fixed=T,)
        
        df_simple<-as.data.frame(matrix(nrow = 0,ncol=2))
        colnames(df_simple)<-c("name","value")
        
        tst<-lapply(tmp,function(x){
            pos<-str_locate(x,">")[1]
            s1<-str_trim(substr(x,1,pos-1))
            s2<-str_trim(substr(x,pos+1,999))
            df<-data.frame(name=s1,value=s2)
            df_simple<<-rbind(df_simple,df)
        })
        
        
        rownames(df_simple)<-df_simple$name
        df_simple$name<-NULL
        df_final<-t(df_simple)
        df_trails<<-rbind(df_trails,df_final)
    })
    
    df_trails$area_id<-area_id
    df_trails$trail_id<-1:nrow(df_trails)
    df_trails$segment_id<-setSegments(df_trails$ID)
    df_trails$coords<-getNodes(txt,"coordinates",T)
    
    df_trails
}

getTrailLatLons<-function(key,filename,trailNames) {
    
    area_id<-getTrailAreaID(key)
    
    txt<-readChar(filename, file.info(filename)$size)
    
    coords_v<-as.vector(getNodes(txt,"coordinates",T))
    coords<-data.frame(area_id=area_id,trail_id=trailNames,coords=coords_v)
  
    
    df_coords<-get_coords(coords)
    
}

getTrailAreaID<-function(key){
    if(!exists("df_trail_areas",envir=.GlobalEnv)) build_trail_areas()
    
    yn_found<-(df_trail_areas$key==key)
    if(length(which(yn_found))==0) {
        if( length(df_trail_areas$id)==0) {
            id=1
        } else {
            id<-max(df_trail_areas$id,na.rm = T)+1
        }
        df_trail_areas<<-rbind(df_trail_areas,data.frame(id=id,key=key))
        
    } else {
        id<-df_trail_areas[yn_found,"id"]
    }
    save(df_trail_areas,file="./data/trailAreas.RData",envir = .GlobalEnv)  

    id
    
}

build_trail_areas<-function() {
    
    if (file.exists("./data/trails/trailAreas.RData")) {
        load(file="./data/trails/trailAreas.RData",envir=.GlobalEnv) 
    } else {
        
        df_trail_areas<-data.frame(id = numeric(), key = character())
        assign(x="df_trail_areas",value=df_trail_areas,envir=.GlobalEnv)
    }
}

write_segment_ends<-function(df,seg_count,key="tmp") {
    
    dfMinMax<-get_segment_ends(df )
    
    write.csv(x=dfMinMax,file=paste("./output/seg_temp_",key,".csv",sep=""))
    
}

get_segment_ends<-function(df) {
    
    seg_count<-length(unique(df$segment_id))
    
    startlats<-numeric(seg_count)
    endlats<-numeric(seg_count)
    startlons<-numeric(seg_count)
    endlons<-numeric(seg_count)
    
    for(i in 1:seg_count) {
        ids<-row.names(df[df$segment_id==i,])
        lats<-df[df$segment_id==i,"latitude"]
        lons<-df[df$segment_id==i,"longitude"]
        startpt<-min(ids)
        endpt<-max(ids)
        startlats[i]<-df[startpt,"latitude"]
        endlats[i]<-df[endpt,"latitude"]
        startlons[i]<-df[startpt,"longitude"]
        endlons[i]<-df[endpt,"longitude"]
        
    }
    
    
    
    dfMinMax<-data.frame(segment_id=1:seg_count,startlats=startlats,endlats=endlats,startlons=startlons,endlons=endlons)

}

get_trail_data_name<-function(name) {
    paste(name,"_trails",sep="")
}

get_trail_latlons_data_name<-function(name) {
    paste(name,"_latlons",sep="")
}

get_raw_trails<-function(name, type="kml") {

    filename_in<-paste("./data_raw/trails/",type,"/",name,".kml",sep="")
    
    trails_df_name<-get_trail_data_name(name)
    latlons_df_name<-get_trail_latlons_data_name(name)
    
    trails_df<-getTrails(name,filename_in)
    latlons_df<-getLatLons(trails_df)
    trails_df[,"coords"]<-NULL

    assign(trails_df_name,trails_df,envir = .GlobalEnv)
    assign(latlons_df_name,latlons_df,envir = .GlobalEnv)       
    
    save_trails(name)
}

save_trails<-function(name) {
    filename_save<-paste("./data/trails/",name,".RData",sep="")
    
    trails_df_name<-get_trail_data_name(name)
    latlons_df_name<-get_trail_latlons_data_name(name)
    
    save(list=c(trails_df_name,latlons_df_name),file=filename_save,envir = .GlobalEnv)
}

add_missing_columns<-function(to,from) {
    df1<-get(to,envir = .GlobalEnv)
    df2<-get(from,envir = .GlobalEnv)
    tnames<-colnames(df1)
    fnames<-colnames(df2)
    
    addcols<-fnames[!(fnames %in% tnames)]
    
    df1[,addcols]<-NA
    assign(x = to,value = df1,envir = .GlobalEnv)
}

load_trails<-function(name) {
    filename_save<-paste("./data/trails/",name,".RData",sep="")
    
    load(file=filename_save,envir = .GlobalEnv)
}

arrange_segments<-function(area_id,trail_id) {
    segs_in<-get_segment_ends(get_trail_latlons(area_id,trail_id))
    
    total_rows<-nrow(segs_in)
    
    segs_out<-segs_in[1,]
    segs_in<-segs_in[2:nrow(segs_in),]
    yndone = F
    nxt<-1
    changed<-F
    
    repeat {
           
        roi<-segs_in[nxt,]
        
        if(roi[1,"endlons"]==segs_out[1,"startlons"]) {
            segs_out<-rbind(roi,segs_out)
            if(nrow(segs_out)==total_rows) {
                yndone<-T
            } else {
                segs_in<-segs_in[!((1:nrow(segs_in)) %in% nxt),]
            }
            changed<-T
            
        } else if(roi[1,"startlons"]==segs_out[nrow(segs_out),"startlons"]) {
            segs_out<-rbind(segs_out,roi)
            if(nrow(segs_out)==total_rows) {
                yndone<-T
            } else {
                segs_in<-segs_in[!((1:nrow(segs_in)) %in% nxt),]
            }
            changed<-T
            
        } else {
            
        }
        
        if (changed) {
            nxt<-1
            changed<-F
        } else {
            nxt<-nxt+1
            if(nxt>nrow(segs_in)) yndone<-T
        }
        
        
        if(yndone) break
    }
    
    if(nrow(segs_out)==total_rows) {
        mapply(function(x,y,z) {
            all_trail_latlons[all_trail_latlons$area_id==area_id & 
                                  all_trail_latlons$trail_id==trail_id & 
                                  all_trail_latlons$segment_id==x ,"segment_id"]<<-y+z
        },1:nrow(segs_out),segs_out$segment_id,total_rows)#, MoreArgs = list(area_id=area_id,trail_id=trail_id)) 
        
#         all_trail_latlons[all_trail_latlons$area_id==area_id & 
#                               all_trail_latlons$trail_id==trail_id ,"segment_id"]<-abs(all_trail_latlons[all_trail_latlons$area_id==area_id && 
#                                                                                                              all_trail_latlons$trail_id==trail_id ,"segment_id"])
    }
    
    segs_out
}


check_reverse<-function(area_id,trail_id,seg_count) {
    if(seg_count==2) {
        ends<-get_segment_ends(get_trail_latlons(area_id,trail_id),seg_count = seg_count)
        if(abs(ends[1,"startlons"]-ends[2,"endlons"])<abs(ends[2,"startlons"]-ends[1,"endlons"])) {
            return (TRUE)
        }
    } 
    return (FALSE)
}


reverse_segments<-function(area_id,trail_id,seg_count) {
    
    df<-data.frame(area_id=integer(seg_count),    
                   trail_id=character(seg_count),
                   segment_old=integer(seg_count),
                   segment_new=integer(seg_count),stringsAsFactors=F)
    
    #area_id    trail_id	segment_old	segment_new
    
    for(i in 1:seg_count) {
        df[i,1]=area_id
        df[i,2]=trail_id
        df[i,3]=i
        df[i,4]=seg_count+1-i
    }
   
    
    mapply(function(x,y,z) {
        all_trail_latlons[all_trail_latlons$area_id==area_id & 
                              all_trail_latlons$trail_id==trail_id & 
                              all_trail_latlons$segment_id==x ,"segment_id"]<<-y+z
    },df$segment_old,df$segment_new,seg_count)
    
    mapply(function(y,z) {
        all_trail_latlons[all_trail_latlons$area_id==area_id & 
                              all_trail_latlons$trail_id==trail_id & 
                              all_trail_latlons$segment_id==y+z ,"segment_id"]<<-y
    },df$segment_new,seg_count)
    
}

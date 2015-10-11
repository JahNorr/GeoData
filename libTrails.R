library(stringr) 
library(geosphere)

source("./libGeoData.R")
source("./libSaveTrailData.R")


save_trail_area<-function(area_name,rebuild_raw=T, ...) {
    
    
    if(rebuild_raw) get_raw_trails(name = area_name)
    
    
    trails_name<-paste(area_name,"trails",sep="_")
    latlons_name<-paste(area_name,"latlons",sep="_")
    
    if(!exists("all_trails")) load("./data/trails/all_trails_empty.RData",envir = .GlobalEnv)
    
    mirror_columns(to=trails_name,from="all_trails")
         
#        start_id<-max(all_trails$trail_id)
        all_trails<<-rbind(all_trails,get(x = trails_name) )
        all_trail_latlons<<-rbind(all_trail_latlons,get(x = latlons_name))
    
    
    save_trails(area_name)

    
    auto_store_trails(area_name, ...)
    
    rownames(df_trails_accepted)<-NULL
    df_trails_accepted$forest_id<-as.integer(df_trails_accepted$forest_id)
    
    save(df_trails_accepted,df_trail_coords_accepted, file=get_accepted_trail_data_file(),envir=.GlobalEnv)
    
}


getTrails<-function(key,filename) {
    forest_id<-getForestID(key)
    
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
    
    df_trails$forest_id<-forest_id
    df_trails$trail_id<-1:nrow(df_trails)
    df_trails$segment_id<-setSegments(df_trails$ID)
    df_trails$coords<-getNodes(txt,"coordinates",T)
    
    df_trails
}

get_trail_latlons<-function(forest_id,trail_num) {
    
    df_latlons<-all_trail_latlons
    
    lloi<-(df_latlons$trail_num==trail_num) & (df_latlons$forest_id==forest_id)
    latlons<-df_latlons[lloi,]
    latlons<-latlons[order(latlons$segment_id,as.integer(rownames(latlons))),]
    
}

getTrailLatLons<-function(key,filename,trailNames) {
    
    forest_id<-getForestID(key)
    
    txt<-readChar(filename, file.info(filename)$size)
    
    coords_v<-as.vector(getNodes(txt,"coordinates",T))
    coords<-data.frame(forest_id=forest_id,trail_num=trailNames,coords=coords_v)
  
    
    df_coords<-get_coords(coords)
    
}

getForestID<-function(key){

    yn_found<-grep(key,df_forests$FORESTNAME,ignore.case = T)
    if(length((yn_found))==0) {
        return (NA)
    } else {
        df_forests[yn_found,"forest_id"]
    }
    
}


getTrailAreaID_OLD<-function(key){
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
    ################################################################
    #
    #
    #  warning changed a lot of this .. if a problem get it back from
    #   github
    #
    #
    seg_ids<-unique(df$segment_id)
    seg_count<-length(seg_ids)
    
    startlats<-numeric(seg_count)
    endlats<-numeric(seg_count)
    startlons<-numeric(seg_count)
    endlons<-numeric(seg_count)
    
    for(i in 1:seg_count) {
        seg_id<-seg_ids[i]
        ids<-row.names(df[df$segment_id==seg_id,])
        lats<-df[df$segment_id==seg_id,"latitude"]
        lons<-df[df$segment_id==seg_id,"longitude"]
        startpt<-min(ids)
        endpt<-max(ids)
        startlats[i]<-df[startpt,"latitude"]
        endlats[i]<-df[endpt,"latitude"]
        startlons[i]<-df[startpt,"longitude"]
        endlons[i]<-df[endpt,"longitude"]
        
    }
    
    
    
    dfMinMax<-data.frame(segment_id=seg_ids,startlats=startlats,endlats=endlats,startlons=startlons,endlons=endlons)

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

mirror_columns<-function(to,from) {
    df_to<-get(to,envir = .GlobalEnv)
    df_from<-get(from,envir = .GlobalEnv)
    tnames<-colnames(df_to)
    fnames<-colnames(df_from)
    
    keep_cols<-which((tnames %in% fnames))
    
    add_cols<-fnames[!(fnames %in% tnames)]
    
    df_to<-df_to[,keep_cols]
    
    df_to[,add_cols]<-NA
    
    
    assign(x = to,value = df_to,envir = .GlobalEnv)
}

load_trails<-function(name) {
    filename_save<-paste("./data/trails/",name,".RData",sep="")
    
    load(file=filename_save,envir = .GlobalEnv)
}

segment_gaps<-function(df) {
    total_rows<-nrow(segs_in)
    p1<-data.frame(segs_in$endlons,segs_in$endlats)
    p1<-data.frame(segs_in$startlons,segs_in$startlats)
    
    sapply()
    dist<-distHaversine(p1, p2)
    
    mlats<-matrix(nrow =total_rows,ncol = total_rows )
    for (i in 1:total_rows) {
        for (j in 1:total_rows) {
            mlats[i,j]<-abs(df[i,"endlats"]-df[j,"startlats"])
            if(i==j)mlats[i,j]<-NA
        }
    }

    mins<-integer(total_rows)
    
    for (i in 1:total_rows) {
        mins[i]<-which.min(mlats[i,])
    }
#    mins<-sapply(mlats,function(x) which.min(x[1,]))
    mins
}

arrange_segments<-function(forest_id,trail_num) {
    segs_in<-get_segment_ends(get_trail_latlons(forest_id,trail_num))
    
    total_rows<-nrow(segs_in)
    
    if (total_rows==1) return (TRUE)
    
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
            
        } else if(roi[1,"startlons"]==segs_out[nrow(segs_out),"endlons"]) {
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
            in_seg<-x
            out_seg<-y+z
            #           print(paste(in_seg,"->",out_seg,sep=""))
            
            all_trail_latlons[all_trail_latlons$forest_id==forest_id & 
                                  all_trail_latlons$trail_num==trail_num & 
                                  all_trail_latlons$segment_id==in_seg ,"segment_id"]<<-out_seg
        },segs_out$segment_id,1:nrow(segs_out),total_rows)
        
        
        mapply(function(x,z) {
            all_trail_latlons[all_trail_latlons$forest_id==forest_id & 
                                  all_trail_latlons$trail_num==trail_num & 
                                  all_trail_latlons$segment_id==x+z ,"segment_id"]<<-x
        },1:nrow(segs_out),total_rows)
        #         
    }
    
    return (nrow(segs_out)==total_rows)
}

arrange_segments_closest2<-function(forest_id,trail_num) {
    segs_in<-get_segment_ends(get_trail_latlons(forest_id,trail_num))
    
    total_rows<-nrow(segs_in)
    
    if (total_rows==1) return (TRUE)
    
    segs_out<-segs_in[1,]
    segs_in<-segs_in[2:nrow(segs_in),]
    yndone = F
    nxt<-1
    changed<-F
    
    tolerance<-20
    
    repeat {
        
        roi<-segs_in[nxt,]
        soi_s<-c(roi[1,"startlons"],roi[1,"startlats"])
        soi_e<-c(roi[1,"endlons"],roi[1,"endlats"])
        
        top_s<-c(segs_out[1,"startlons"],segs_out[1,"startlats"])      
        top_e<-c(segs_out[1,"endlons"],segs_out[1,"endlats"])
        
        bot_s<-c(segs_out[nrow(segs_out),"startlons"],segs_out[nrow(segs_out),"startlats"])      
        bot_e<-c(segs_out[nrow(segs_out),"endlons"],segs_out[nrow(segs_out),"endlats"])
        
        if(distHaversine(soi_e,top_s)<tolerance) {
            segs_out<-rbind(roi,segs_out)
            if(nrow(segs_out)==total_rows) {
                yndone<-T
            } else {
                segs_in<-segs_in[!((1:nrow(segs_in)) %in% nxt),]
            }
            changed<-T
            
        } else if (distHaversine(soi_s,bot_e)<tolerance) {
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
            all_trail_latlons[all_trail_latlons$forest_id==forest_id & 
                                  all_trail_latlons$trail_num==trail_num & 
                                  all_trail_latlons$segment_id==x ,"segment_id"]<<-y+z
        },segs_out$segment_id,1:nrow(segs_out),total_rows)
        
        
        mapply(function(x,z) {
            all_trail_latlons[all_trail_latlons$forest_id==forest_id & 
                                  all_trail_latlons$trail_num==trail_num & 
                                  all_trail_latlons$segment_id==x+z ,"segment_id"]<<-x
        },1:nrow(segs_out),total_rows)
        #         
    }
    
    return (nrow(segs_out)==total_rows)
    
}

arrange_segments_closest<-function(forest_id,trail_num) {
    segs_in<-get_segment_ends(get_trail_latlons(forest_id,trail_num))
    
    total_rows<-nrow(segs_in)
    
    if (total_rows==1) return (TRUE)
    
    segs_out<-segs_in[1,]
    segs_in<-segs_in[2:nrow(segs_in),]
    yndone = F
    nxt<-1
    changed<-F
        
    tolerance<-20
    
    repeat {
        
        roi<-segs_in[nxt,]
        soi_s<-c(roi[1,"startlons"],roi[1,"startlats"])
        soi_e<-c(roi[1,"endlons"],roi[1,"endlats"])
        
        top_s<-c(segs_out[1,"startlons"],segs_out[1,"startlats"])      
        top_e<-c(segs_out[1,"endlons"],segs_out[1,"endlats"])
        
        bot_s<-c(segs_out[nrow(segs_out),"startlons"],segs_out[nrow(segs_out),"startlats"])      
        bot_e<-c(segs_out[nrow(segs_out),"endlons"],segs_out[nrow(segs_out),"endlats"])
        
        if(distHaversine(soi_e,top_s)<tolerance) {
            segs_out<-rbind(roi,segs_out)
            if(nrow(segs_out)==total_rows) {
                yndone<-T
            } else {
                segs_in<-segs_in[!((1:nrow(segs_in)) %in% nxt),]
            }
            changed<-T
            
        } else if (distHaversine(soi_s,bot_e)<tolerance) {
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
            all_trail_latlons[all_trail_latlons$forest_id==forest_id & 
                                  all_trail_latlons$trail_num==trail_num & 
                                  all_trail_latlons$segment_id==x ,"segment_id"]<<-y+z
        },segs_out$segment_id,1:nrow(segs_out),total_rows)
        
        
        mapply(function(x,z) {
            all_trail_latlons[all_trail_latlons$forest_id==forest_id & 
                                  all_trail_latlons$trail_num==trail_num & 
                                  all_trail_latlons$segment_id==x+z ,"segment_id"]<<-x
        },1:nrow(segs_out),total_rows)
        #         
    }
    
    return (nrow(segs_out)==total_rows)
    
}

arrange_segments_best<-function(forest_id,trail_num,segs_in) {
    
    if(missing("segs_in")) segs_in<-get_segment_ends(get_trail_latlons(forest_id,trail_num))
    
    total_rows<-nrow(segs_in)
    
    if (total_rows==1) return (TRUE)
    
    segs_out<-segs_in[1,]
    segs_in<-segs_in[2:nrow(segs_in),]
    yndone = F
    nxt<-1
    changed<-F

    repeat {
           
        roi<-segs_in[nxt,]
        segoi<-roi$segment_id
        if(roi[1,"endlons"]==segs_out[1,"startlons"]) {
            segs_out<-rbind(roi,segs_out)
            if(nrow(segs_out)==total_rows) {
                yndone<-T
            }
            segs_in<-segs_in[!((1:nrow(segs_in)) %in% nxt),]
            changed<-T
            
        } else if(roi[1,"startlons"]==segs_out[nrow(segs_out),"endlons"]) {
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
    
    my_latlons<-all_trail_latlons[all_trail_latlons$forest_id==forest_id & 
                                    all_trail_latlons$trail_num==trail_num &
                                      all_trail_latlons$segment_id %in% segs_out$segment_id  ,]
    
    other_latlons<-all_trail_latlons[all_trail_latlons$forest_id==forest_id & 
                                         all_trail_latlons$trail_num==trail_num &
                                         all_trail_latlons$segment_id %in% segs_in$segment_id  ,]
        
    if(nrow(segs_out)>0) {
        
        seg_off<-max(segs_out$segment_id)+1
        
        mapply(function(x,y,z) {
            in_seg<-x
            out_seg<-y+z
 #           print(paste(in_seg,"->",out_seg,sep=""))
                  
            my_latlons[my_latlons$segment_id==in_seg ,"segment_id"]<<-out_seg
        },segs_out$segment_id,1:nrow(segs_out),seg_off)
        

        mapply(function(x,z) {
            my_latlons[my_latlons$segment_id==x+z ,"segment_id"]<<-x
        },1:nrow(segs_out),seg_off)
#         
        my_latlons<-my_latlons[my_latlons$segment_id<=nrow(segs_out),]
    }

    list(ok=my_latlons,ok_segs=segs_out,not_ok=other_latlons,not_ok_segs=segs_in)
}


check_reverse<-function(forest_id,trail_num,seg_count) {
    if(seg_count==2) {
        ends<-get_segment_ends(get_trail_latlons(forest_id,trail_num),seg_count = seg_count)
        if(abs(ends[1,"startlons"]-ends[2,"endlons"])<abs(ends[2,"startlons"]-ends[1,"endlons"])) {
            return (TRUE)
        }
    } 
    return (FALSE)
}


reverse_segments<-function(forest_id,trail_num,seg_count) {
    
    df<-data.frame(forest_id=integer(seg_count),    
                   trail_num=character(seg_count),
                   segment_old=integer(seg_count),
                   segment_new=integer(seg_count),stringsAsFactors=F)
    
    #forest_id    trail_num	segment_old	segment_new
    
    for(i in 1:seg_count) {
        df[i,1]=forest_id
        df[i,2]=trail_num
        df[i,3]=i
        df[i,4]=seg_count+1-i
    }
   
    
    mapply(function(x,y,z) {
        all_trail_latlons[all_trail_latlons$forest_id==forest_id & 
                              all_trail_latlons$trail_num==trail_num & 
                              all_trail_latlons$segment_id==x ,"segment_id"]<<-y+z
    },df$segment_old,df$segment_new,seg_count)
    
    mapply(function(y,z) {
        all_trail_latlons[all_trail_latlons$forest_id==forest_id & 
                              all_trail_latlons$trail_num==trail_num & 
                              all_trail_latlons$segment_id==y+z ,"segment_id"]<<-y
    },df$segment_new,seg_count)
    
}

split_segments<-function(forest_id,trail_num,seg_count,index) {
    
   
    mapply(function(x,y) {
        all_trail_latlons[all_trail_latlons$forest_id==forest_id & 
                              all_trail_latlons$trail_num==trail_num & 
                              all_trail_latlons$segment_id==x ,"segment_id"]<<-x+y
    },1:index,seg_count)
    
    mapply(function(y,z) {
        all_trail_latlons[all_trail_latlons$forest_id==forest_id & 
                              all_trail_latlons$trail_num==trail_num & 
                              all_trail_latlons$segment_id==y+z ,"segment_id"]<<-y
    },1:seg_count,index)
    
}


auto_store_trails<-function(area_of_interest, max_trails=99999) {
    
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
    ynfound<-check_trails_accepted()
    if(ynfound) next_trail_id<-max(df_trails_accepted$trail_id)+1 else next_trail_id<-1
    trails_checked<-0
    repeat {
        fixed<-F
        
        if(ynfound) {
            area_trails_accepted<-as.character(df_trails_accepted[df_trails_accepted$forest_id==forest_id,"ID"])
        } else {
            
            area_trails_accepted<-character(0)
        }
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
        
        
        fixed<-arrange_segments(forest_id,trail_num)
        
        if(!fixed) fixed<-arrange_segments_closest(forest_id,trail_num)
        
        if(fixed) {
            print(paste("Saving trail ",trail_num," (",trail_name,") ",sep=""))
            latlons<-get_trail_latlons(forest_id,trail_num)
            
            accept_trail(forest_id,trail_num,latlons,next_trail_id,ynsave=F)
            next_trail_id<-next_trail_id+1
            ynfound<-TRUE
        }
        else  {
            add_skip(forest_id,trail_num)
        }
       
        trails_checked<-trails_checked+1
        if (length(trails_to_check)==1 || trails_checked>=max_trails) break
    }
    
    
    
}


remove_segment<-function(forest_id,trail_num,segment_id) {
    all_trail_latlons<<-all_trail_latlons[!(all_trail_latlons$forest_id==forest_id & 
                                                all_trail_latlons$trail_num==trail_num & 
                                                all_trail_latlons$segment_id==segment_id),]
    
}

check_skip<-function() {
    
    if(!exists("df_skip")) {
        df_skip<<-data.frame(forest_id=numeric(),trail_num=character(),stringsAsFactors=F)
    }
}


add_skip<-function(forest_id,trail_num) {
    df_skip_add<-data.frame(forest_id=forest_id,trail_num=trail_num)
    df_skip<<-rbind(df_skip,df_skip_add)
}

clean_trails_env<-function() {
    oldw <- getOption("warn")
    options(warn = -1)
    
    rm(df_trails_accepted,envir = .GlobalEnv)
    rm(df_trail_coords_accepted,envir = .GlobalEnv)
    rm(df_skip,envir = .GlobalEnv)
    rm(all_trails,envir = .GlobalEnv)
    rm(all_trail_latlons,envir = .GlobalEnv)
    
    remove_accepted_trail_data_file()
    
    
    options(warn = oldw)
}


build_empty_all_trails<-function(){
    
    all_trails<-all_trails[which(is.na(all_trails$ID)),]
    all_trail_latlons<-all_trail_latlons[which(is.na(all_trail_latlons$trail_num)),]
    save(all_trails,all_trail_latlons,file="./data/trails/all_trails_empty.RData")
    
}


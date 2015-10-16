

auto_store_trails<-function(area_name, max_trails=99999) {
    debug<-T
    
    forest_id<-getForestID(area_name)
    trails_name<-paste(area_name,"trails",sep="_")
    latlons_name<-paste(area_name,"latlons",sep="_")
    
    ########################################
    #
    #   get the trails for this area
    #
    trails<-get(x = trails_name,envir = .GlobalEnv) #all_trails[all_trails$forest_id==forest_id,]
    trail_latlons<-get(x = latlons_name,envir = .GlobalEnv)
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
    max_trail_id<-check_trails_accepted()
    next_trail_id<-max_trail_id+1
    trails_checked<-0
    repeat {
        fixed<-F
        
        if(next_trail_id>1) {
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
        
        latlons<- trail_latlons[trail_latlons$trail_num==trail_num,]
        
        if(debug) print(paste("Trying trail ",trail_num," (",trail_name,") ",sep=""))
        
        arranged<-arrange_segments(latlons)
        fixed<-arranged$success
        if(fixed) latlons<-arranged$latlons
        
        if(!fixed){
            
            arranged<-arrange_segments_closest(latlons)
            fixed<-arranged$success
            if(fixed) latlons<-arranged$latlons
        }
        
#         if (!fixed) {
#             arranged<-arrange_segments_best(latlons)
#             fixed<-arranged$success
#             if(fixed) latlons<-arranged$latlons
#             
#             
#         }
        
        if(fixed) {
            print(paste("Saving trail ",trail_num," (",trail_name,") ",sep=""))
    
            accept_trail(trails,forest_id,trail_num,latlons,next_trail_id,ynsave=F)
            next_trail_id<-next_trail_id+1
     
        }
        else  {
            add_skip(forest_id,trail_num)
        }
        
        trails_checked<-trails_checked+1
        if (length(trails_to_check)==1 || trails_checked>=max_trails) break
    }
    
    
    
}

arrange_segments<-function(latlons) {
    segs_in<-get_segment_ends(latlons)
    
    total_rows<-nrow(segs_in)
    
    if (total_rows==1) return (list(success=TRUE,latlons=latlons))
    
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
            
            latlons[latlons$segment_id==in_seg ,"segment_id"]<<-out_seg
        },segs_out$segment_id,1:nrow(segs_out),total_rows)
        
        
        mapply(function(x,z) {
            latlons[latlons$segment_id==x+z ,"segment_id"]<<-x
        },1:nrow(segs_out),total_rows)
        #    
        
        latlons<-latlons[order(latlons$segment_id,as.integer(rownames(latlons))),]
        latlons$index<-1:nrow(latlons)
        latlons$segment_id<-1

    }
    
    list(success=nrow(segs_out)==total_rows,latlons=latlons)
}

arrange_segments_closest<-function(latlons) {
        
    segs_in<-get_segment_ends(latlons)
        
    total_rows<-nrow(segs_in)
    
    if (total_rows==1) return (list(success=TRUE,latlons=latlons))
    
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
            latlons[latlons$segment_id==x ,"segment_id"]<<-y+z
        },segs_out$segment_id,1:nrow(segs_out),total_rows)
        
        
        mapply(function(x,z) {
            latlons[latlons$segment_id==x+z ,"segment_id"]<<-x
        },1:nrow(segs_out),total_rows)
        #    
        latlons<-latlons[order(latlons$segment_id,as.integer(rownames(latlons))),]
        latlons$index<-1:nrow(latlons)
        latlons$segment_id<-1
        
    }
    
    list(success=nrow(segs_out)==total_rows,latlons=latlons)
    
}


arrange_segments_best<-function(latlons,segs_in) {
    
    if(missing("segs_in")) segs_in<-get_segment_ends(latlons)
    
    total_rows<-nrow(segs_in)
    
    if (total_rows==1) {
      latlons<-latlons[as.integer(rownames(latlons)),]
      latlons$index<-1:nrow(latlons)
      latlons$segment_id<-1
      return (list(ok=latlons,ok_segs=segs_in,not_ok=NULL,not_ok_segs=NULL))
    }

    
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
    
    my_latlons<-latlons[latlons$segment_id %in% segs_out$segment_id  ,]
    
    other_latlons<-latlons[latlons$segment_id %in% segs_in$segment_id  ,]
    
    if(nrow(segs_out)>0) {
        
        seg_off<-max(segs_out$segment_id)+1
        
        mapply(function(x,y,z) {
            in_seg<-x
            out_seg<-y+z
            my_latlons[my_latlons$segment_id==in_seg ,"segment_id"]<<-out_seg
        },segs_out$segment_id,1:nrow(segs_out),seg_off)
        
        
        mapply(function(x,z) {
            my_latlons[my_latlons$segment_id==x+z ,"segment_id"]<<-x
        },1:nrow(segs_out),seg_off)
        #         
        my_latlons<-my_latlons[my_latlons$segment_id<=nrow(segs_out),]
    }
    
    my_latlons<-my_latlons[order(my_latlons$segment_id,as.integer(rownames(my_latlons))),]
    my_latlons$index<-1:nrow(my_latlons)
    my_latlons$segment_id<-1

    
    list(ok=my_latlons,ok_segs=segs_out,not_ok=other_latlons,not_ok_segs=segs_in)
}


arrange_segments_multi<-function(latlons) {
  
  fixed<-arrange_segments_best(latlons)
  
  save_ll<-fixed$ok
  seg<-1
  
  while(nrow(fixed$not_ok_segs)!=0){
    seg<-seg+1
    
    fixed<-arrange_segments_best(latlons,segs_in = fixed$not_ok_segs)
    
    save_ll_new<-fixed$ok
    save_ll_new$segment_id<-seg
    save_ll<-rbind(save_ll,save_ll_new)
    
  }
  save_ll
}


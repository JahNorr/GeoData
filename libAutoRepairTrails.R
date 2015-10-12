

auto_store_trails<-function(area_of_interest, max_trails=99999) {
    debug<-T
    
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
        
        latlons<- get_all_trail_latlons(forest_id,trail_num)
        
        if(debug) print(paste("Trying trail ",trail_num," (",trail_name,") ",sep=""))
        
        arranged<-arrange_segments(latlons)
        fixed<-arranged$all
        if(fixed) latlons<-arranged$latlons
        
        if(!fixed){
            
            arranged<-arrange_segments_closest(latlons)
            fixed<-arranged$all
            if(fixed) latlons<-arranged$latlons
        }
        
        if(fixed) {
            print(paste("Saving trail ",trail_num," (",trail_name,") ",sep=""))
            
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

arrange_segments<-function(latlons) {
    segs_in<-get_segment_ends(latlons)
    
    total_rows<-nrow(segs_in)
    
    if (total_rows==1) return (list(all=TRUE,latlons=latlons))
    
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
    }
    
    list(all=nrow(segs_out)==total_rows,latlons=latlons)
}

arrange_segments_closest<-function(latlons) {
        
    segs_in<-get_segment_ends(latlons)
        
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
            latlons[latlons$segment_id==x ,"segment_id"]<<-y+z
        },segs_out$segment_id,1:nrow(segs_out),total_rows)
        
        
        mapply(function(x,z) {
            latlons[latlons$segment_id==x+z ,"segment_id"]<<-x
        },1:nrow(segs_out),total_rows)
        #    
        latlons<-latlons[order(latlons$segment_id,as.integer(rownames(latlons))),]
        
    }
    
    list(all=nrow(segs_out)==total_rows,latlons=latlons)
    
}


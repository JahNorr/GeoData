

name<-"ALPINE 7"

trail<-all_trails[all_trails$NAME==name,]

forest_id<-trail[1,"forest_id"]
trail_num<-as.character(trail[1,"ID"])
fixed<-arrange_segments_best(forest_id,trail_num)

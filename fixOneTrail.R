source("./libPlot.R")

name<-"ALPINE 7"

trail<-all_trails[all_trails$NAME==name,]

forest_id<-trail[1,"forest_id"]
trail_num<-as.character(trail[1,"ID"])

fixed<-arrange_segments_best(forest_id,trail_num)

while(nrow(fixed$not_ok_segs)==0){

save_ll<-fixed$ok

fixed<-arrange_segments_best(forest_id,trail_num,segs_in = fixed$not_ok_segs)

}
plot_latlons(save_ll)
plot_latlons(fixed$ok)

#fixed<-arrange_segments_best(forest_id,trail_num)



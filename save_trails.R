

source("./libTrails.R")
source("./libPlot.R")

options(stringsAsFactors=F)

clean_trails_env()

save_trail_area("flathead",rebuild_raw = F)#, max_trails=15)
save_trail_area("kootenai",rebuild_raw = T)#, max_trails=200)

df_trails_accepted$forest_id<-as.integer(df_trails_accepted$forest_id)
df_trails_accepted$trail_id<-as.integer(1:(nrow(df_trails_accepted)))


trails_ids<-mapply(function(x,y) {
    df_trails_accepted[(df_trails_accepted$forest_id==x) & (df_trails_accepted$ID==y),"trail_id"]
}, df_trail_coords_accepted$forest_id,df_trail_coords_accepted$trail_num)



df_trail_coords_accepted$trail_id<-trails_ids

plot_random<-10

if(plot_random>0) {
    rnd<-sample(nrow(df_trails_accepted),plot_random)
    
    sapply(rnd,function(x) {
        forest_id<-df_trails_accepted[x,"forest_id"]
        plot_trail_saved(forest_id,df_trails_accepted[x,"ID"])
        return(T)
    })
    
}

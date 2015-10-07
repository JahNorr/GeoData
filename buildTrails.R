
source("./libGeoData.R")
source("./libTrails.R")


get_raw_trails(name = "flathead")
get_raw_trails(name = "kootenai")

add_missing_columns(to="flathead_trails",from="kootenai_trails")
add_missing_columns(from="flathead_trails",to="kootenai_trails")

save_trails("flathead")
save_trails("kootenai")

#load_trails("flathead")
#load_trails("kootenai")

all_trails<-rbind(flathead_trails,kootenai_trails)
all_trail_latlons<-rbind(flathead_latlons,kootenai_latlons)

auto_store_trails("flathead")

auto_store_trails("kootenai")

rownames(df_trails_accepted)<-NULL

save(df_trails_accepted,df_trail_coords_accepted, file=get_accepted_trail_data_file(),envir=.GlobalEnv)



source("./libTrails.R")

save_trail_area("flathead")



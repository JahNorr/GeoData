library(stringr)

source("./libPlot.R")

index<- sample(nrow(df_skip),1)
forest_id<-df_skip[index,"forest_id"]
trail_num<-df_skip[index,"trail_num"]

forest_name<-df_forests[df_forests$forest_id==forest_id,"FORESTNAME"]
abbrev<-str_trim(str_to_lower(gsub(pattern = "National Forest",replacement = "", forest_name)))
trails_df_name<-get_trail_data_name(abbrev)
latlons_df_name<-get_trail_latlons_data_name(abbrev)

trails<-get(x = trails_df_name)
latlons<-get(x = latlons_df_name)

latlons<-latlons[latlons$trail_num==trail_num,]

latlons_clean<-arrange_segments_multi(latlons)
plot_latlons(latlons_clean)


#fixed<-arrange_segments_best(forest_id,trail_num)



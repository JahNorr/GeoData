
source("./libForests.R")
source("./sqlFuncs.R")

region_filename<-"C:/Users/john/R_workspace/GeoData/data_raw/S_USA.AdministrativeRegion.kml"
df_regions<-getForestAdminRegions(region_filename)

forest_filename<-"C:/Users/john/R_workspace/GeoData/data_raw/S_USA.AdministrativeForest.kml"
df_forests<-getForests(forest_filename)

ranger_district_filename<-"C:/Users/john/R_workspace/GeoData/data_raw/S_USA.RangerDistrict.kml"
df_ranger_districts<-getRangerDistricts(ranger_district_filename)

save(df_regions,df_forests,df_ranger_districts,file="./data/forests/forests.RData")

write_forests_sqlite()

#write_trails_sqlite()


library("RSQLite")
library(sqldf)

write_trails_sqlite<-function() {
    db <- dbConnect(SQLite(),dbname="./output/trails.db")
    sqldf("attach './output/trails.db' as new")
    
    dbWriteTable(db,"trails",df_trails_accepted, overwrite=T)
    
    # dbListTables(db)              # The tables in the database
    # dbListFields(db, "trails")    # The columns in a table
    # dbReadTable(db, "trails")     # The data in a table
    # 
    
    dbDisconnect(db)
}

order_trail_pts<-function(area_id,trail_number,trail) {
    df_trails_accepted[order(df_trails_accepted$area_id,df_trails_accepted$trail_id),]
    
}

write_forests_sqlite<-function() {
    db <- dbConnect(SQLite(),dbname="./output/forests.db")
    sqldf("attach './output/forests.db' as new")
    
    dbWriteTable(db,"regions",df_regions, overwrite=T)
    dbWriteTable(db,"forests",df_forests, overwrite=T)
    dbWriteTable(db,"ranger_districts",df_ranger_districts, overwrite=T)
    
    # dbListTables(db)              # The tables in the database
    # dbListFields(db, "trails")    # The columns in a table
    # dbReadTable(db, "trails")     # The data in a table
    # 
    dbDisconnect(db)
    
}

library(sqldf)

db_name="./output/trails.db"
create_db_cmd<-paste("attach '",db_name,"' as new",sep="")
    
db <- dbConnect(SQLite(),dbname=db_name)#,dbname="trails.db")

if(!file.exists(db_name)) sqldf(create_db_cmd)

dbSendQuery(db,create_tbl2_cmd)

dbWriteTable(conn = db,name = "trails",value = df_trails_accepted,overwrite=T)
dbWriteTable(conn = db,name = "trail_latlons",value = df_trail_coords_accepted,overwrite=T)

dbDisconnect(db)


# dbListTables(db)              # The tables in the database
#


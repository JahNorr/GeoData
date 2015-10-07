library(sqldf)

get_mwa_db_dir<-function() {
    "C:/Users/john/StudioProjects/MontanaWildernessAssociation/app/src/main/assets/databases/"
}

get_mwa_forests()<-function() {
    
    mwa_db_dir=get_mwa_db_dir()
    db_name="mwa_trails.db"
    
    
}


create_db_cmd<-paste("attach '",db_name,"' as new",sep="")

db <- dbConnect(SQLite(),dbname=db_name)#,dbname="trails.db")




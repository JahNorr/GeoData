
library(stringr)

source("./libTrails.R")

getForestAdminRegions<-function(filename) {
    
    txt<-readChar(filename, file.info(filename)$size)
    txt<-gsub(pattern = "\t"    ,"",txt,fixed=T,)
    txt<-gsub(pattern = "\n"    ,"",txt,fixed=T,)
    
    regions<-getNodes(txt,"Placemark")
    
    regions_tmp<-regions
    
    df_regions<-data.frame()
    
    temp<-lapply(regions_tmp, function(region) {
        
        simple<-getNodes(region,"SimpleData",F)
        
        tmp<-gsub(pattern = "\"","",x = simple,fixed=T,)
        tmp<-gsub(pattern = "name=","",x = tmp,fixed=T,)
        
        df_simple<-as.data.frame(matrix(nrow = 0,ncol=2))
        colnames(df_simple)<-c("name","value")
        
        tst<-lapply(tmp,function(x){
            pos<-str_locate(x,">")[1]
            s1<-str_trim(substr(x,1,pos-1))
            s2<-str_trim(substr(x,pos+1,999))
            df<-data.frame(name=s1,value=s2)
            df_simple<<-rbind(df_simple,df)
        })
        
        
        rownames(df_simple)<-df_simple$name
        df_simple$name<-NULL
        df_final<-t(df_simple)
        df_regions<<-rbind(df_regions,df_final)
    })
    
    df_regions$region_id<-1:nrow(df_regions)
    #    df_regions$coords<-getNodes(txt,"coordinates",T)
    
    df_regions$REGIONNAME<-as.character(df_regions$REGIONNAME)
    rownames(df_regions)<-NULL
    
    col_id<-which(colnames(df_regions)=="region_id")
    col_other<-which(colnames(df_regions)!="region_id")
    
    df_regions<-df_regions[,c(col_id,col_other)]
    
    df_regions
}

getForests<-function(filename) {
    
    txt<-readChar(filename, file.info(filename)$size)
    txt<-gsub(pattern = "\t"    ,"",txt,fixed=T,)
    txt<-gsub(pattern = "\n"    ,"",txt,fixed=T,)
    
    forests<-getNodes(txt,"Placemark")
    
    forests_tmp<-forests
    
    df_forests<-data.frame()
    
    temp<-lapply(forests_tmp, function(forest) {
        
        simple<-getNodes(forest,"SimpleData",F)
        
        tmp<-gsub(pattern = "\"","",x = simple,fixed=T,)
        tmp<-gsub(pattern = "name=","",x = tmp,fixed=T,)
        
        df_simple<-as.data.frame(matrix(nrow = 0,ncol=2))
        colnames(df_simple)<-c("name","value")
        
        tst<-lapply(tmp,function(x){
            pos<-str_locate(x,">")[1]
            s1<-str_trim(substr(x,1,pos-1))
            s2<-str_trim(substr(x,pos+1,999))
            df<-data.frame(name=s1,value=s2)
            df_simple<<-rbind(df_simple,df)
        })
        
        
        rownames(df_simple)<-df_simple$name
        df_simple$name<-NULL
        df_final<-t(df_simple)
        df_forests<<-rbind(df_forests,df_final)
    })
    
    df_forests$forest_id<-1:nrow(df_forests)
    #    df_forests$coords<-getNodes(txt,"coordinates",T)
    
    df_forests$FORESTNAME<-as.character(df_forests$FORESTNAME)
    df_forests$REGION<-as.character(df_forests$REGION)
    df_forests$region_id<-sapply(df_forests$REGION, function(x) {
        df_regions[df_regions$REGION==x,"region_id"]
    })
    
    col_id<-which(colnames(df_forests)=="forest_id")
    col_regid<-which(colnames(df_forests)=="region_id")
    col_others<-which(!((1:ncol(df_forests)) %in% c(col_id,col_regid)))
     
    df_forests<-df_forests[,c(col_id,col_regid,col_others)]
    
    rownames(df_forests)<-NULL
    df_forests
}

getRangerDistricts<-function(filename) {
    
    
    txt<-readChar(filename, file.info(filename)$size)
    txt<-gsub(pattern = "\t"    ,"",txt,fixed=T,)
    txt<-gsub(pattern = "\n"    ,"",txt,fixed=T,)
    
    ranger_districts<-getNodes(txt,"Placemark")
    
    ranger_districts_tmp<-ranger_districts
    
    df_ranger_districts<-data.frame()
    
    temp<-lapply(ranger_districts_tmp, function(ranger_district) {
        
        simple<-getNodes(ranger_district,"SimpleData",F)
        
        tmp<-gsub(pattern = "\"","",x = simple,fixed=T,)
        tmp<-gsub(pattern = "name=","",x = tmp,fixed=T,)
        
        df_simple<-as.data.frame(matrix(nrow = 0,ncol=2))
        colnames(df_simple)<-c("name","value")
        
        tst<-lapply(tmp,function(x){
            pos<-str_locate(x,">")[1]
            s1<-str_trim(substr(x,1,pos-1))
            s2<-str_trim(substr(x,pos+1,999))
            df<-data.frame(name=s1,value=s2)
            df_simple<<-rbind(df_simple,df)
        })
        
        
        rownames(df_simple)<-df_simple$name
        df_simple$name<-NULL
        df_final<-t(df_simple)
        df_ranger_districts<<-rbind(df_ranger_districts,df_final)
    })
    
    df_ranger_districts$ranger_district_id<-1:nrow(df_ranger_districts)
    #    df_ranger_districts$coords<-getNodes(txt,"coordinates",T)
    
     df_ranger_districts$FORESTNUMB<-as.character(df_ranger_districts$FORESTNUMB)
#     df_ranger_districts$REGION<-as.character(df_ranger_districts$REGION)

     df_ranger_districts$forest_id<-mapply( function(x,y) {
         df_forests[df_forests$REGION==x & df_forests$FORESTNUMB==y,"forest_id"]
     },df_ranger_districts$REGION,df_ranger_districts$FORESTNUMB)

    
     col_id<-which(colnames(df_ranger_districts)=="ranger_district_id")
     col_forid<-which(colnames(df_ranger_districts)=="forest_id")
     col_others<-which(!((1:ncol(df_ranger_districts)) %in% c(col_id,col_forid)))
     
     df_ranger_districts<-df_ranger_districts[,c(col_id,col_forid,col_others)]
    
    rownames(df_ranger_districts)<-NULL
    df_ranger_districts
}

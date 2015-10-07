





filename<-"C:/Users/john/Documents/Geodata/Trails/FS_TRAIL_Flathead.kml"

flathead_trails<-getTrails("Flathead",filename)
flathead_latlons<-getLatLons(flathead_trails)

key="Flathead"


flathead_latlons[flathead_latlons$trail_num=="10",]

which(coords$trail_num=="10")

coords[396,]

flathead_trails[396,]





txt<-readChar(filename, file.info(filename)$size)
txt<-gsub(pattern = "\t"    ,"",txt,fixed=T,)
txt<-gsub(pattern = "\n"    ,"",txt,fixed=T,)

nodes<-getNodes(txt,"coordinates",T)
coords_v<-as.vector()

segs<-setSegments(flathead_trails$ID)
df_segs<-data.frame(ids=flathead_trails$ID,segs=segs)

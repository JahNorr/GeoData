library("XML")

filename<-"C:/Users/john/Documents/Geodata/Trails/FS_TRAIL_Flathead.kml"

doc <- xmlParse(filename)
rt<-xmlRoot(doc)


l<-xmlToList(rt)


doc <- xmlTreeParse(filename)

rt<-xmlRoot(doc)

xmlSize(rt)

d<-  xmlChildren(rt)
fldr<-d[["Folder"]]

pmark<-fldr[["Placemark"]]

xmlName(fldr)

nodes<-xmlChildren(fldr)


#trails<-nodes[3:(length(nodes)-1)]
trails<-nodes[3:10]


test<-lapply(trails,function(x) {
    xmlName(x)
    xmlSize(x)
    
    y<-x[[1]]$ExtendedData
    
    xmlName(y)
})

trails[1]

df<-xmlToDataFrame(nodes = trails)





kml<-doc$doc$children$kml

getChildrenStrings(kml,encoding = )

docP <- xmlParse("C:/Users/john/Documents/Geodata/Trails/FS_TRAIL_Flathead.kml")

xpathApply(docP,'//Placemark')







doc1<- xmlInternalTreeParse("C:/Users/john/Documents/Geodata/Trails/FS_TRAIL_Flathead.kml")

nodes<-getNodeSet(doc = doc1,path = "//Placemark" )

xml_data <- xmlToList(data)


names(xml_data[1][2])


df<-xmlToDataFrame(kml,)

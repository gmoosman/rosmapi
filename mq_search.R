require(jsonlite)
require(stringr)
require(gtools)
search.query<-"boots"

location.search.mq<-function(search.query,country.code="gb",wait=0,jump=1,key){
  url.1<-"http://open.mapquestapi.com/nominatim/v1/search.php?key="
  url.2<-"&format=json&limit=50&q="
  url.cc<-paste0("&countrycodes=",country.code)
  url.q<-str_replace(search.query," ","+")
  south<-49.570001
  north<-58.400001
  east<-1.460001
  west<-(-6.130001)
  SToN<-c(south,seq(south+1, north,jump),north)
  for(j in 1:(length(SToN)-1)){
    lonlat<-paste(west,SToN[j+1],east,SToN[j],sep=",")
    url.bound<-paste0("&bounded=1&viewbox=",lonlat)
    json.url.1 <- paste0(url.1,key,url.2,url.q,url.cc,url.bound)
    df1<-fromJSON(json.url.1)
    df2<-df1
    if(length(df1)>0){
    while (nrow(df1)==50){
      Sys.sleep(wait)
      excl_ids<-paste0(df2$place_id,collapse=",")
      json.url.2<-paste0(json.url.1,"&exclude_place_ids=",excl_ids)
      df1<-fromJSON(json.url.2)
      suppressWarnings(df2<-smartbind(df2,df1))
    }
    if("df3" %in% objects()){suppressWarnings(df3<-smartbind(df3,df2))}else{df3<-df2}
      print(paste0("Number of locations found: ",nrow(df3)))
    }
  }
  return(df3)

  }
  boots<-location.search.mq("boots",key=key)
  
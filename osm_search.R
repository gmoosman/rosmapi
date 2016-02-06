require(jsonlite)
require(stringr)
require(gtools)



location.search.osm<-function(search.query,country.code="gb",wait=1){
url.1<-"http://nominatim.openstreetmap.org/search?&format=json&addressdetails=1&limit=50&q="
url.cc<-paste0("&countrycodes=",country.code)
url.q<-str_replace(search.query," ","+")
json.url.1 <- paste0(url.1,url.q,url.cc)
df1<-fromJSON(json.url.1)
df2<-df1
print(paste0("Number of locations found: ",nrow(df2)))
while (nrow(df1)==50){
  Sys.sleep(wait)
  excl_ids<-paste0(df2$place_id,collapse=",")
  json.url.2<-paste0(json.url.1,"&exclude_place_ids=",excl_ids)
  df1<-fromJSON(json.url.2)
  suppressWarnings(df2<-smartbind(df2,df1))
  print(paste0("Number of locations found: ",nrow(df2)))
}
return(df2)
}

aldis<-location.search.osm("Aldi")

warning
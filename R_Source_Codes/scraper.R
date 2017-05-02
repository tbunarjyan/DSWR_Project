library(xml2)
library(rvest) 
library(stringr) 
library(dplyr)
library(curl)


df<-data.frame(price=character(),
               district=character(),
               street=character(),
               num_rooms=character(),
               floor=character(),
               max_floor=character(),
               area=character(),
               num_bathrooms=character(),
               building_type=character(),
               ceiling_height=character(),
               condition=character(),
               url=character(),stringsAsFactors = FALSE)
base<-"http://www.myrealty.am/en/item/"
count=0
for(i in 10000:99999){
  URL<-paste(base,toString(i),sep = "")
  page<-read_html(curl(URL, handle = new_handle("useragent" = "Mozilla/5.0")))
  homedata<-html_nodes(page,css = ".tt3")
  homedata<-html_text(homedata)
  if(!(length(homedata) == 0) && (typeof(homedata) == "character")){
    rent<-html_nodes(page,css = "h5")
    rent<-html_text(rent)
    rent<-str_split(rent,"\n")
    rent<-str_trim(rent[[1]][3],side = "both")
    if(rent=="FOR SALE"&&homedata[3]=="Yerevan"){
      price<-str_sub(homedata[2],start =2 - nchar(homedata[2]))
      district<-homedata[4]
      street<-homedata[5]
      num_rooms=homedata[6]
      floor_and_max=str_split(homedata[7],"/")
      n_floor=floor_and_max[[1]][1]
      max_floor=floor_and_max[[1]][2]
      area<-str_split(homedata[8]," ")[[1]][1]
      num_bath=homedata[9]
      build_type=str_trim(homedata[10],side = "both")
      ceiling_h=str_split(homedata[11]," ")[[1]][1]
      condition=str_trim(homedata[12],side = "both")
      new_row=c(price,district,street,num_rooms,n_floor,max_floor,area,num_bath,build_type,ceiling_h,condition,URL)
      df[nrow(df)+1,]<-new_row
      count<-count+1
    }
  }
  if(count==10000){
    break
  }
}
df<-na.omit(df)
write.csv(df,file = "houseData.csv")
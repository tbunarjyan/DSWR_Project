library(stringr)
houseData<-read.csv("yerevan_april_9.csv") # loading dataset houseData
houseData$price<-str_replace(houseData$price,",","")
houseData$price<-as.numeric(houseData$price)
houseData$num_rooms<-as.numeric(houseData$num_rooms)
houseData$num_bathrooms<-as.numeric(houseData$num_bathrooms)
houseData$floor<-as.numeric(houseData$floor)
houseData$max_floor<-as.numeric(houseData$max_floor)
houseData$ceiling_height<-as.numeric(houseData$ceiling_height)
houseData$district<-as.numeric(houseData$district)
houseData$condition<-as.numeric(houseData$condition)
houseData$street<-as.numeric(houseData$street)
houseData<-na.omit(houseData)

# results of polynomial regression from degree 1 to 3 using only features: area 
for(j in 1:3){
  scores_ar=vector(length = 5)#score for only area regression
  scores_nr=vector(length = 5)#score for area and number of rooms
  scores_nb=vector(length = 5)#score for area, number of rooms,number of bathrooms
  scores_f=vector(length = 5)#score for area, number of rooms,number of bathrooms, floor
  scores_mf=vector(length = 5)#score for area, number of rooms,number of bathrooms, floor,max floor
  scores_ch=vector(length = 5)#score for area, number of rooms,number of bathrooms, floor,max floor,ceiling height
  scores_d=vector(length = 5)#score for area, number of rooms,number of bathrooms, floor,max floor,ceiling height,district
  scores_c=vector(length = 5)#score for area, number of rooms,number of bathrooms, floor,max floor,ceiling height,district,condition
  scores_str=vector(length = 5)#score for area, number of rooms,number of bathrooms, floor,max floor,ceiling height,district,condition,street
  for(i in 1:5){
    valData<-houseData[sample(nrow(houseData), nrow(houseData)*0.1), ]
    trainData <- houseData[-which(houseData$X%in%valData$X), ]
    PArea<-poly(trainData$area, degree = j,raw = T)
    PNumRooms<-polym(trainData$area,trainData$num_rooms, degree = j,raw = T)
    PNumBathrooms<-polym(trainData$area,trainData$num_rooms,trainData$num_bathrooms, degree = j,raw = T)
    PFloor<-polym(trainData$area,trainData$num_rooms,trainData$num_bathrooms,trainData$floor, degree = j,raw = T)
    PMaxFloor<-polym(trainData$area,trainData$num_rooms,trainData$num_bathrooms,trainData$floor,trainData$max_floor, degree = j,raw = T)
    PCeilHeight<-polym(trainData$area,
             trainData$num_rooms,
             trainData$num_bathrooms,
             trainData$floor,
             trainData$max_floor,
             trainData$ceiling_height,
             degree = j,raw = T)
    PDistrict<-polym(trainData$area,
             trainData$num_rooms,
             trainData$num_bathrooms,
             trainData$floor,
             trainData$max_floor,
             trainData$ceiling_height,
             trainData$district,
             degree = j,raw = T)
    PCondition<-polym(trainData$area,
             trainData$ceiling_height,
             trainData$district,
             trainData$condition,
             degree = j,raw = T)
    PStreet<-polym(trainData$area,
                      trainData$ceiling_height,
                      trainData$district,
                      trainData$condition,
                      trainData$street,
                      degree = j,raw = T)
    lm_model_ar<-lm(price~PArea, data = trainData)
    lm_model_nr<-lm(price~PNumRooms, data = trainData)
    lm_model_nb<-lm(price~PNumBathrooms, data = trainData)
    lm_model_f<-lm(price~PFloor, data = trainData)
    lm_model_mf<-lm(price~PMaxFloor, data = trainData)
    lm_model_ch<-lm(price~PCeilHeight, data = trainData)
    lm_model_d<-lm(price~PDistrict, data = trainData)
    lm_model_c<-lm(price~PCondition, data = trainData)
    lm_model_str<-lm(price~PStreet, data = trainData)
    weights_ar<-lm_model_ar$coefficients
    weights_nr<-lm_model_nr$coefficients
    weights_nb<-lm_model_nb$coefficients
    weights_f<-lm_model_f$coefficients
    weights_mf<-lm_model_mf$coefficients
    weights_ch<-lm_model_ch$coefficients
    weights_d<-lm_model_d$coefficients
    weights_c<-lm_model_c$coefficients
    weights_str<-lm_model_str$coefficients
    w_ar<-t(t(weights_ar[2:length(weights_ar)]))
    w_nr<-t(t(weights_nr[2:length(weights_nr)]))
    w_nb<-t(t(weights_nb[2:length(weights_nb)]))
    w_f<-t(t(weights_f[2:length(weights_f)]))
    w_mf<-t(t(weights_mf[2:length(weights_mf)]))
    w_ch<-t(t(weights_ch[2:length(weights_ch)]))
    w_d<-t(t(weights_d[2:length(weights_d)]))
    w_c<-t(t(weights_c[2:length(weights_c)]))
    w_str<-t(t(weights_str[2:length(weights_str)]))
    err_ar<-poly(valData$area,degree = j,raw = T)%*%w_ar+weights_ar[1]-valData$price
    err_nr<-polym(valData$area,valData$num_rooms,degree = j,raw = T)%*%w_nr+weights_nr[1]-valData$price
    err_nb<-polym(valData$area,valData$num_rooms,valData$num_bathrooms,degree = j,raw = T)%*%w_nb+weights_nb[1]-valData$price
    err_f<-polym(valData$area,
                 valData$num_rooms,
                 valData$num_bathrooms,
                 valData$floor,
                 degree = j,raw = T)%*%w_f+weights_f[1]-valData$price
    err_mf<-polym(valData$area,
                  valData$num_rooms,
                  valData$num_bathrooms,
                  valData$floor,
                  valData$max_floor,
                  degree = j,raw = T)%*%w_mf+weights_mf[1]-valData$price
    err_ch<-polym(valData$area,
                  valData$num_rooms,
                  valData$num_bathrooms,
                  valData$floor,
                  valData$max_floor,
                  valData$ceiling_height,
                  degree = j,raw = T)%*%w_ch+weights_ch[1]-valData$price
    err_d<-polym(valData$area,
                 valData$num_rooms,
                 valData$num_bathrooms,
                 valData$floor,
                 valData$max_floor,
                 valData$ceiling_height,
                 valData$district,
                 degree = j,raw = T)%*%w_d+weights_d[1]-valData$price
    err_c<-polym(valData$area,
                 valData$ceiling_height,
                 valData$district,
                 valData$condition,
                 degree = j,raw = T)%*%w_c+weights_c[1]-valData$price
    err_str<-polym(valData$area,
                 valData$ceiling_height,
                 valData$district,
                 valData$condition,
                 valData$street,
                 degree = j,raw = T)%*%w_str+weights_str[1]-valData$price
    scores_ar[i]<-sqrt(t(err_ar)%*%err_ar/nrow(valData))
    scores_nr[i]<-sqrt(t(err_nr)%*%err_nr/nrow(valData))
    scores_nb[i]<-sqrt(t(err_nb)%*%err_nb/nrow(valData))
    scores_f[i]<-sqrt(t(err_f)%*%err_f/nrow(valData))
    scores_mf[i]<-sqrt(t(err_mf)%*%err_mf/nrow(valData))
    scores_ch[i]<-sqrt(t(err_ch)%*%err_ch/nrow(valData))
    scores_d[i]<-sqrt(t(err_d)%*%err_d/nrow(valData))
    scores_c[i]<-sqrt(t(err_c)%*%err_c/nrow(valData))
    scores_str[i]<-sqrt(t(err_str)%*%err_str/nrow(valData))
  }
  line<-c("Polynomial degree: ", j, "RMSE in dolars: ",
          "area: ", mean(scores_ar),
          "number of rooms added: ", mean(scores_nr),
          "number of bathrooms added: ", mean(scores_nb),
          "floor added: ", mean(scores_f),
          "max floor added: ", mean(scores_mf),
          "ceiling height added: ", mean(scores_ch),
          "district added: ", mean(scores_d),
          "condition added: ", mean(scores_c),
          "street added: ", mean(scores_str))
  print(line)
}


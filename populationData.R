#Population
##Data source: https://www.worldpop.org/project/categories?id=18
library(raster)
library(rgdal)
library(dplyr)

shape_dists <- readOGR("C:/Users/dms228/Downloads/Yemen Data/yem_admbnda_adm2_govyem_cso_20191002.shp")
dist_names <- shape_dists$ADM2_EN
dist_govs <- shape_dists$ADM1_EN
govs <- unique(shape_dists$ADM1_EN)
dist_df <- data.frame(District = dist_govs, District = dist_names)

pop_rasters <- vector(mode = "list", length = 4)
popDens_rasters <- vector(mode = "list", length = 4)
pop_byDist <- data.frame()
popDens_byDist <- data.frame()
for(i in 1:4){
  pop <- paste("C:/Users/dms228/OneDrive - University of Exeter/PhD/data/Yemen Data/Population/yem_ppp_", 2016+i ,"_1km_Aggregated_UNadj.tif", sep = "")
  popDens <- paste("C:/Users/dms228/OneDrive - University of Exeter/PhD/data/Yemen Data/Population/yem_pd_", 2016+i ,"_1km_UNadj.tif", sep = "")
  pop_rasters[[i]] <- raster(pop)
  popDens_rasters[[i]] <- raster(popDens)
}

#Population Count
i = 1
j = 1
k = 1
pop_byDist <- vector()
for(i in 1:4){
  pop_rast <- pop_rasters[[i]]
  pop_byDist_1year <- vector()
  for(j in 1:22){
    gov_map <- shape_dists[shape_dists$ADM1_EN == govs[j],]
    gov_dists <- gov_map$ADM2_EN
    district_pop <- matrix(nrow = length(gov_dists), ncol = 3)
    for(k in 1:length(gov_dists)){
      dist_map <- gov_map[k,]
      extent_square <- crop(pop_rast, dist_map)
      dist_shaped <- mask(extent_square, dist_map)
      dist_ar <- rasterToPoints(dist_shaped)
      district_pop[k,3] <- sum(dist_ar[,3])
      district_pop[k,1] <- govs[j]
      district_pop[k,2] <- dist_map$ADM2_EN
    }
    pop_byDist_1year <- rbind(pop_byDist_1year, district_pop)
  }
  pop_byDist <- cbind(pop_byDist, pop_byDist_1year)
}

yemen_pop <- pop_byDist %>% as.data.frame() %>% dplyr::select(c(1,2,3,6,9,12))
colnames(yemen_pop) <- c("Governorate", "District", "2017", "2018", "2019", "2020")
yemen_pop[3:6] <- apply(yemen_pop[3:6], 2,function(x) as.numeric(as.character(x)))

##########Change spelling so it's the same as WHO_weekly ######
yemen_pop[1:2] <- gsub("[^[:alnum:]]", "", as.matrix(yemen_pop[1:2]))
yemen_pop$Governorate[yemen_pop$Governorate == "AdDali"] <- "AlDhalee"
yemen_pop$Governorate[yemen_pop$Governorate == "SanaaCity"] <- "AmanatAlAsimah"
yemen_pop$Governorate[yemen_pop$Governorate == "Taiz"] <- "Taizz"
yemen_pop$Governorate[yemen_pop$Governorate == "AlHodeidah"] <- "AlHudaydah"
yemen_pop$Governorate[yemen_pop$Governorate == "Sadah"] <- "Saada"
yemen_pop$Governorate[yemen_pop$Governorate == "Sadah"] <- "Saada"

yemen_pop$District[yemen_pop$District == "AlWadi"] <- "AlWadea"
yemen_pop$District[yemen_pop$District == "Khanfar"] <- "Khanfir"
yemen_pop$District[yemen_pop$District == "Rassd"] <- "Rasad"
yemen_pop$District[yemen_pop$District == "Zinjibar"] <- "Zingibar"
yemen_pop$District[yemen_pop$District == "AlBurayqah"] <- "AlBuraiqeh"
yemen_pop$District[yemen_pop$District == "AlMansurah"] <- "AlMansura"
yemen_pop$District[yemen_pop$District == "AshShaykhOthman"] <- "AshShaikhOuthman"
yemen_pop$District[yemen_pop$District == "AtTawahi"] <- "Attawahi"
yemen_pop$District[yemen_pop$District == "KritarSirah"] <- "Craiter"
yemen_pop$District[yemen_pop$District == "AlMalajim"] <- "AlMalagim"
yemen_pop$District[yemen_pop$District == "Numan"] <- "Naman"
yemen_pop$District[yemen_pop$District == "Radman"] <- "RadmanAlAwad"
yemen_pop$District[yemen_pop$District == "AlMutun"] <- "AlKhalq"
yemen_pop$District[yemen_pop$District == "Numan"] <- "AlMaton"
yemen_pop$District[yemen_pop$District == "BaratAlAnan"] <- "BartAlAnan"
yemen_pop$District[yemen_pop$District == "AlGhaydhah"] <- "AlGhaydah"
yemen_pop$District[yemen_pop$District == "Haswin"] <- "Huswain"
yemen_pop$District[yemen_pop$District == "AlMahwit"] <- "AlMahwait"
yemen_pop$District[yemen_pop$District == "AlMahwitCity"] <- "AlMahwaitCity"
yemen_pop$District[yemen_pop$District == "Melhan"] <- "Milhan"
yemen_pop$District[yemen_pop$District == "AlWehdah"] <- "AlWahdah"
yemen_pop$District[yemen_pop$District == "AsSabin"] <- "AsSabain"
yemen_pop$District[yemen_pop$District == "AsSafiyah"] <- "Assafiyah"
yemen_pop$District[yemen_pop$District == "AthThawrah"] <- "Aththaorah"
yemen_pop$District[yemen_pop$District == "Azaal"] <- "Azzal"
yemen_pop$District[yemen_pop$District == "Main"] <- "Maain"
yemen_pop$District[yemen_pop$District == "Shuub"] <- "Shuaub"
yemen_pop$District[yemen_pop$District == "QaflatOdhar"] <- "AlQaflah"
yemen_pop$District[yemen_pop$District == "BaniSurim"] <- "BaniSuraim"
yemen_pop$District[yemen_pop$District == "Dhibain"] <- "DhiBin"
yemen_pop$District[yemen_pop$District == "DhulaymatHabur"] <- "HaburZulaymah"
yemen_pop$District[yemen_pop$District == "EyalSurayh"] <- "IyalSurayh"
yemen_pop$District[yemen_pop$District == "JabalEyalYazid"] <- "JabalIyalYazid"
yemen_pop$District[yemen_pop$District == "Ans"] <- "Anss"
yemen_pop$District[yemen_pop$District == "DawranAnis"] <- "DawranAness"
yemen_pop$District[yemen_pop$District == "MaghribAns"] <- "MaghiribAns"
yemen_pop$District[yemen_pop$District == "Otmah"] <- "Utmah"
yemen_pop$District[yemen_pop$District == "WusabAlAali"] <- "WusabAlAli"
yemen_pop$District[yemen_pop$District == "AflahAshSham"] <- "AflahAshShawm"
yemen_pop$District[yemen_pop$District == "Ashshaghadirah"] <- "AshShaghadirah"
yemen_pop$District[yemen_pop$District == "Aslam"] <- "Aslem"
yemen_pop$District[yemen_pop$District == "BaniQays"] <- "BaniQais"
yemen_pop$District[yemen_pop$District == "Harad"] <- "Haradh"
yemen_pop$District[yemen_pop$District == "KuhlanAfar"] <- "KuhlanAffar"
yemen_pop$District[yemen_pop$District == "QaflShammar"] <- "QaflShamer"
yemen_pop$District[yemen_pop$District == "Wadrah"] <- "Wadhrah"
yemen_pop$District[yemen_pop$District == "AlOdayn"] <- "AlUdayn"
yemen_pop$District[yemen_pop$District == "AsSaiyani"] <- "AsSayyani"
yemen_pop$District[yemen_pop$District == "FarAlOdayn"] <- "FarAlUdayn"
yemen_pop$District[yemen_pop$District == "HazmAlOdayn"] <- "HazmAlUdayn"
yemen_pop$District[yemen_pop$District == "Hobeish"] <- "Hubaysh"
yemen_pop$District[yemen_pop$District == "AlMadaribahWaAlAarah"] <- "AlMadaribahWaAlArah"
yemen_pop$District[yemen_pop$District == "AlMaflahi"] <- "AlMaflahy"
yemen_pop$District[yemen_pop$District == "AlMalah"] <- "AlMilah"
yemen_pop$District[yemen_pop$District == "AlQubaytah"] <- "AlQabbaytah"
yemen_pop$District[yemen_pop$District == "Halmin"] <- "Halimayn"
yemen_pop$District[yemen_pop$District == "Yafi"] <- "Yafaa"
yemen_pop$District[yemen_pop$District == "Bidbadah"] <- "BidBidah"
yemen_pop$District[yemen_pop$District == "MadghalAlJidan"] <- "Madkhal"
yemen_pop$District[yemen_pop$District == "AlJafariyyah"] <- "AlJafariyah"
yemen_pop$District[yemen_pop$District == "AsSalafiyyah"] <- "AsSalafiyah"
yemen_pop$District[yemen_pop$District == "BiladAttaam"] <- "BiladAtTaam"
yemen_pop$District[yemen_pop$District == "BiladAttaam"] <- "BiladAtTaam"
yemen_pop$District[yemen_pop$District == "AlHissn"] <- "AlHusn"
yemen_pop$District[yemen_pop$District == "AtTyal"] <- "Attyal"
yemen_pop$District[yemen_pop$District == "BaniDabyan"] <- "BaniDhabyan"
yemen_pop$District[yemen_pop$District == "Khawlan"] <- "Khwlan"
yemen_pop$District[yemen_pop$District == "SanhanwaBaniBahlul"] <- "Sanhan"
yemen_pop$District[yemen_pop$District == "Ayn"] <- "Ain"
yemen_pop$District[yemen_pop$District == "Khawlan"] <- "Khwlan"
yemen_pop$District[yemen_pop$District == "Armaa"] <- "Arma"
yemen_pop$District[yemen_pop$District == "Attalih"] <- "Attalih"
yemen_pop$District[yemen_pop$District == "Dahr"] <- "Dhar"
yemen_pop$District[yemen_pop$District == "MarkhahAsSufla"] <- "MarkhahAssufla"
yemen_pop$District[yemen_pop$District == "Mayfaah"] <- "Mayfaa"
yemen_pop$District[yemen_pop$District == "MarkhahAlOlya"] <- "MerkhahAlUlya"
yemen_pop$District[yemen_pop$District == "Radum"] <- "Rudum"
yemen_pop$District[yemen_pop$District == "Osaylan"] <- "Usaylan"
yemen_pop$District[yemen_pop$District == "AlMakha"] <- "AlMukha"
yemen_pop$District[yemen_pop$District == "AlWaziyah"] <- "AlWaziiyah"
yemen_pop$District[yemen_pop$District == "JabalHabashi"] <- "JabalHabashy"
yemen_pop$District[yemen_pop$District == "MashrahWaHadnan"] <- "MashraaWaHadnan"
yemen_pop$District[yemen_pop$District == "Salah"] <- "Salh"
yemen_pop$District[yemen_pop$District == "Sami"] <- "Sama"
yemen_pop$District[yemen_pop$District == "AlHasayn"] <- "AlHussein"
yemen_pop$District[yemen_pop$District == "Qatabah"] <- "Qaatabah"
yemen_pop$District[yemen_pop$District == "AdDohi"] <- "AdDahi"
yemen_pop$District[yemen_pop$District == "AlJarrahi"] <- "AlGarrahi"
yemen_pop$District[yemen_pop$District == "AlHujjaylah"] <- "AlHajjaylah"
yemen_pop$District[yemen_pop$District == "AlKhukhah"] <- "AlKhawkhah"
yemen_pop$District[yemen_pop$District == "Alluhayah"] <- "Alluheyah"
yemen_pop$District[yemen_pop$District == "AtTuhayta"] <- "AtTuhayat"
yemen_pop$District[yemen_pop$District == "BaytAlFaqih"] <- "BaytAlFaqiah"
yemen_pop$District[yemen_pop$District == "KhabwaAshShaf"] <- "KhabbwaashShaaf"
yemen_pop$District[yemen_pop$District == "MayfaatAns"] <- "MayfaatAnss"
yemen_pop$District[yemen_pop$District == "AdhDhahir"] <- "AlDhihar"
yemen_pop$District[yemen_pop$District == "AlHashwah"] <- "AlHishuah"
yemen_pop$District[yemen_pop$District == "KitafwaAlBoqa"] <- "kitafwaAlBoqee"
yemen_pop$District[yemen_pop$District == "Sadah"] <- "Saadah"
yemen_pop$District[yemen_pop$District == "Saqin"] <- "Saqayn"
yemen_pop$District[yemen_pop$District == "AlMawasit"] <- "AlMawasid"

######################################

#Aggregated at governorate level
yemen_pop_govAgg <- yemen_pop %>% group_by(Governorate) %>% dplyr::select(-District) %>% summarise(across(`2017`:`2020`,sum))

####POPULATION DENSITY ##################
#Units: Number of people per square kilometer
popDens_byDist <- vector()
for(i in 1:4){
  pop_rast <- popDens_rasters[[i]]
  pop_byDist_1year <- vector()
  for(j in 1:22){
    gov_map <- shape_dists[shape_dists$ADM1_EN == govs[j],]
    gov_dists <- gov_map$ADM2_EN
    district_pop <- matrix(nrow = length(gov_dists), ncol = 3)
    for(k in 1:length(gov_dists)){
      dist_map <- gov_map[k,]
      extent_square <- crop(pop_rast, dist_map)
      dist_shaped <- mask(extent_square, dist_map)
      dist_ar <- rasterToPoints(dist_shaped)
      district_pop[k,3] <- mean(dist_ar[,3])
      district_pop[k,1] <- govs[j]
      district_pop[k,2] <- dist_map$ADM2_EN
    }
    pop_byDist_1year <- rbind(pop_byDist_1year, district_pop)
  }
  popDens_byDist <- cbind(popDens_byDist, pop_byDist_1year)
}
yemen_popDens <- popDens_byDist %>% as.data.frame() %>% dplyr::select(c(1,2,3,6,9,12))
colnames(yemen_popDens) <- c("Governorate", "District", "2017", "2018", "2019", "2020")
yemen_popDens[3:6] <- apply(yemen_popDens[3:6], 2,function(x) as.numeric(as.character(x)))

#####Area of Each District in km2############
#Here we count the number of population pixels included in each region. Each pixel is 1km^2 resolution, so 
rast17 <- popDens_rasters[[1]]
area_byDist <- vector()
for(j in 1:22){
  gov_map <- shape_dists[shape_dists$ADM1_EN == govs[j],]
  gov_dists <- gov_map$ADM2_EN
  district_area <- vector(length = length(gov_dists))
  district_area <- matrix(nrow = length(gov_dists), ncol = 3)
  for(k in 1:length(gov_dists)){
    dist_map <- gov_map[k,]
    extent_square <- crop(rast17, dist_map)
    dist_shaped <- mask(extent_square, dist_map)
    district_area[k,1] <- govs[j]
    district_area[k,2] <- gov_dists[k]
    district_area[k,3] <- rasterToPoints(dist_shaped) %>% nrow()
  }
  area_byDist <- rbind(area_byDist, district_area)
}
area_byDist <- area_byDist %>% as.data.frame() %>% set_colnames(c("Governorate", "District", "Area"))
area_byDist$Area <- area_byDist$Area %>% as.numeric()

##Get density manually
#By District (just to check the theory)
pop_area_byDist <- full_join(yemen_pop, area_byDist, by = c("Governorate", "District"))
popDens_byDist <- cbind(pop_area_byDist[,1:2],matrix(nrow = 335, ncol = 4))
for(i in 3:6){
  popDens_byDist[,i] <- pop_area_byDist[,i] / pop_area_byDist[,7]
}

popDens_man_byDist[,4] <- pop_area_byDist[,4] / pop_area_byDist[,7]

#By Governorate (because taking the mean of the districts wouldn't be accurate?)
pop_area_byGov <- full_join(yemen_pop_govAgg, area_byGov, by = "Governorate")
popDens_byGov <- cbind(pop_area_byGov[,1],matrix(nrow = nrow(pop_area_byGov), ncol = 4))
for(i in 2:5){
  popDens_byGov[,i] <- pop_area_byGov[,i] / pop_area_byGov[6]
}

colnames(popDens_byGov) <- c("Governorate", "2017", "2018", "2019", "2020")
#################Change area_byDist spellings############
area_byDist[1:2] <- gsub("[^[:alnum:]]", "", as.matrix(area_byDist[1:2]))
area_byDist$Governorate[area_byDist$Governorate == "AdDali"] <- "AlDhalee"
area_byDist$Governorate[area_byDist$Governorate == "SanaaCity"] <- "AmanatAlAsimah"
area_byDist$Governorate[area_byDist$Governorate == "Taiz"] <- "Taizz"
area_byDist$Governorate[area_byDist$Governorate == "AlHodeidah"] <- "AlHudaydah"
area_byDist$Governorate[area_byDist$Governorate == "Sadah"] <- "Saada"
area_byDist$Governorate[area_byDist$Governorate == "Sadah"] <- "Saada"

#Change spellings of districts
area_byDist$District[area_byDist$District == "AlWadi"] <- "AlWadea"
area_byDist$District[area_byDist$District == "Khanfar"] <- "Khanfir"
area_byDist$District[area_byDist$District == "Rassd"] <- "Rasad"
area_byDist$District[area_byDist$District == "Zinjibar"] <- "Zingibar"
area_byDist$District[area_byDist$District == "AlBurayqah"] <- "AlBuraiqeh"
area_byDist$District[area_byDist$District == "AlMansurah"] <- "AlMansura"
area_byDist$District[area_byDist$District == "AshShaykhOthman"] <- "AshShaikhOuthman"
area_byDist$District[area_byDist$District == "AtTawahi"] <- "Attawahi"
area_byDist$District[area_byDist$District == "KritarSirah"] <- "Craiter"
area_byDist$District[area_byDist$District == "AlMalajim"] <- "AlMalagim"
area_byDist$District[area_byDist$District == "Numan"] <- "Naman"
area_byDist$District[area_byDist$District == "Radman"] <- "RadmanAlAwad"
area_byDist$District[area_byDist$District == "AlMutun"] <- "AlKhalq"
area_byDist$District[area_byDist$District == "Numan"] <- "AlMaton"
area_byDist$District[area_byDist$District == "BaratAlAnan"] <- "BartAlAnan"
area_byDist$District[area_byDist$District == "AlGhaydhah"] <- "AlGhaydah"
area_byDist$District[area_byDist$District == "Haswin"] <- "Huswain"
area_byDist$District[area_byDist$District == "AlMahwit"] <- "AlMahwait"
area_byDist$District[area_byDist$District == "AlMahwitCity"] <- "AlMahwaitCity"
area_byDist$District[area_byDist$District == "Melhan"] <- "Milhan"
area_byDist$District[area_byDist$District == "AlWehdah"] <- "AlWahdah"
area_byDist$District[area_byDist$District == "AsSabin"] <- "AsSabain"
area_byDist$District[area_byDist$District == "AsSafiyah"] <- "Assafiyah"
area_byDist$District[area_byDist$District == "AthThawrah"] <- "Aththaorah"
area_byDist$District[area_byDist$District == "Azaal"] <- "Azzal"
area_byDist$District[area_byDist$District == "Main"] <- "Maain"
area_byDist$District[area_byDist$District == "Shuub"] <- "Shuaub"
area_byDist$District[area_byDist$District == "QaflatOdhar"] <- "AlQaflah"
area_byDist$District[area_byDist$District == "BaniSurim"] <- "BaniSuraim"
area_byDist$District[area_byDist$District == "Dhibain"] <- "DhiBin"
area_byDist$District[area_byDist$District == "DhulaymatHabur"] <- "HaburZulaymah"
area_byDist$District[area_byDist$District == "EyalSurayh"] <- "IyalSurayh"
area_byDist$District[area_byDist$District == "JabalEyalYazid"] <- "JabalIyalYazid"
area_byDist$District[area_byDist$District == "Ans"] <- "Anss"
area_byDist$District[area_byDist$District == "DawranAnis"] <- "DawranAness"
area_byDist$District[area_byDist$District == "MaghribAns"] <- "MaghiribAns"
area_byDist$District[area_byDist$District == "Otmah"] <- "Utmah"
area_byDist$District[area_byDist$District == "WusabAlAali"] <- "WusabAlAli"
area_byDist$District[area_byDist$District == "AflahAshSham"] <- "AflahAshShawm"
area_byDist$District[area_byDist$District == "Ashshaghadirah"] <- "AshShaghadirah"
area_byDist$District[area_byDist$District == "Aslam"] <- "Aslem"
area_byDist$District[area_byDist$District == "BaniQays"] <- "BaniQais"
area_byDist$District[area_byDist$District == "Harad"] <- "Haradh"
area_byDist$District[area_byDist$District == "KuhlanAfar"] <- "KuhlanAffar"
area_byDist$District[area_byDist$District == "QaflShammar"] <- "QaflShamer"
area_byDist$District[area_byDist$District == "Wadrah"] <- "Wadhrah"
area_byDist$District[area_byDist$District == "AlOdayn"] <- "AlUdayn"
area_byDist$District[area_byDist$District == "AsSaiyani"] <- "AsSayyani"
area_byDist$District[area_byDist$District == "FarAlOdayn"] <- "FarAlUdayn"
area_byDist$District[area_byDist$District == "HazmAlOdayn"] <- "HazmAlUdayn"
area_byDist$District[area_byDist$District == "Hobeish"] <- "Hubaysh"
area_byDist$District[area_byDist$District == "AlMadaribahWaAlAarah"] <- "AlMadaribahWaAlArah"
area_byDist$District[area_byDist$District == "AlMaflahi"] <- "AlMaflahy"
area_byDist$District[area_byDist$District == "AlMalah"] <- "AlMilah"
area_byDist$District[area_byDist$District == "AlQubaytah"] <- "AlQabbaytah"
area_byDist$District[area_byDist$District == "Halmin"] <- "Halimayn"
area_byDist$District[area_byDist$District == "Yafi"] <- "Yafaa"
area_byDist$District[area_byDist$District == "Bidbadah"] <- "BidBidah"
area_byDist$District[area_byDist$District == "MadghalAlJidan"] <- "Madkhal"
area_byDist$District[area_byDist$District == "AlJafariyyah"] <- "AlJafariyah"
area_byDist$District[area_byDist$District == "AsSalafiyyah"] <- "AsSalafiyah"
area_byDist$District[area_byDist$District == "BiladAttaam"] <- "BiladAtTaam"
area_byDist$District[area_byDist$District == "BiladAttaam"] <- "BiladAtTaam"
area_byDist$District[area_byDist$District == "AlHissn"] <- "AlHusn"
area_byDist$District[area_byDist$District == "AtTyal"] <- "Attyal"
area_byDist$District[area_byDist$District == "BaniDabyan"] <- "BaniDhabyan"
area_byDist$District[area_byDist$District == "Khawlan"] <- "Khwlan"
area_byDist$District[area_byDist$District == "SanhanwaBaniBahlul"] <- "Sanhan"
area_byDist$District[area_byDist$District == "Ayn"] <- "Ain"
area_byDist$District[area_byDist$District == "Khawlan"] <- "Khwlan"
area_byDist$District[area_byDist$District == "Armaa"] <- "Arma"
area_byDist$District[area_byDist$District == "Attalih"] <- "Attalih"
area_byDist$District[area_byDist$District == "Dahr"] <- "Dhar"
area_byDist$District[area_byDist$District == "MarkhahAsSufla"] <- "MarkhahAssufla"
area_byDist$District[area_byDist$District == "Mayfaah"] <- "Mayfaa"
area_byDist$District[area_byDist$District == "MarkhahAlOlya"] <- "MerkhahAlUlya"
area_byDist$District[area_byDist$District == "Radum"] <- "Rudum"
area_byDist$District[area_byDist$District == "Osaylan"] <- "Usaylan"
area_byDist$District[area_byDist$District == "AlMakha"] <- "AlMukha"
area_byDist$District[area_byDist$District == "AlWaziyah"] <- "AlWaziiyah"
area_byDist$District[area_byDist$District == "JabalHabashi"] <- "JabalHabashy"
area_byDist$District[area_byDist$District == "MashrahWaHadnan"] <- "MashraaWaHadnan"
area_byDist$District[area_byDist$District == "Salah"] <- "Salh"
area_byDist$District[area_byDist$District == "Sami"] <- "Sama"
area_byDist$District[area_byDist$District == "AlHasayn"] <- "AlHussein"
area_byDist$District[area_byDist$District == "Qatabah"] <- "Qaatabah"
area_byDist$District[area_byDist$District == "AdDohi"] <- "AdDahi"
area_byDist$District[area_byDist$District == "AlJarrahi"] <- "AlGarrahi"
area_byDist$District[area_byDist$District == "AlHujjaylah"] <- "AlHajjaylah"
area_byDist$District[area_byDist$District == "AlKhukhah"] <- "AlKhawkhah"
area_byDist$District[area_byDist$District == "Alluhayah"] <- "Alluheyah"
area_byDist$District[area_byDist$District == "AtTuhayta"] <- "AtTuhayat"
area_byDist$District[area_byDist$District == "BaytAlFaqih"] <- "BaytAlFaqiah"
area_byDist$District[area_byDist$District == "KhabwaAshShaf"] <- "KhabbwaashShaaf"
area_byDist$District[area_byDist$District == "MayfaatAns"] <- "MayfaatAnss"
area_byDist$District[area_byDist$District == "AdhDhahir"] <- "AlDhihar"
area_byDist$District[area_byDist$District == "AlHashwah"] <- "AlHishuah"
area_byDist$District[area_byDist$District == "KitafwaAlBoqa"] <- "kitafwaAlBoqee"
area_byDist$District[area_byDist$District == "Sadah"] <- "Saadah"
area_byDist$District[area_byDist$District == "Saqin"] <- "Saqayn"
area_byDist$District[area_byDist$District == "AlMawasit"] <- "AlMawasid"

#################
area_byGov <- area_byDist %>% group_by(Governorate) %>% summarise(Area = sum(Area))
saveRDS(area_byDist, "C:/Users/dms228/OneDrive - University of Exeter/R Scripts/saved_data/area_byDist.RDS")
saveRDS(area_byGov, "C:/Users/dms228/OneDrive - University of Exeter/R Scripts/saved_data/area_byGov.RDS")





#Aggregated at District level
yemen_popDens_govAgg <- yemen_popDens %>% group_by(District) %>% dplyr::select(-District) %>% summarise(across(`2017`:`2020`,mean))

saveRDS(yemen_pop, "C:/Users/dms228/OneDrive - University of Exeter/R Scripts/saved_data/yemen_popData.RDS")
#yemen_pop <- readRDS("C:/Users/dms228/OneDrive - University of Exeter/R Scripts/saved_data/yemen_popData.RDS")
saveRDS(yemen_popDens, "C:/Users/dms228/OneDrive - University of Exeter/R Scripts/saved_data/yemen_popDensData.RDS")
#yemen_popDens <- readRDS("C:/Users/dms228/OneDrive - University of Exeter/R Scripts/saved_data/yemen_popDensData.RDS")
saveRDS(yemen_pop_govAgg,"C:/Users/dms228/OneDrive - University of Exeter/R Scripts/saved_data/yemen_pop_govAgg.RDS")
#yemen_pop_govAgg <- readRDS("C:/Users/dms228/OneDrive - University of Exeter/R Scripts/saved_data/yemen_pop_govAgg.RDS")

saveRDS(popDens_byGov, "C:/Users/dms228/OneDrive - University of Exeter/R Scripts/saved_data/yemen_popDens_gov.RDS")

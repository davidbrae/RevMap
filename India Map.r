#-----------------------------------------------------------------------------------------------------------
#-----------------------------------------------------------------------------------------------------------
#-----------------------------------------------------------------------------------------------------------
library(tidyverse)
library(sf)
library(RColorBrewer)
library(ggrepel)
library(gridExtra)
library(grid)

#-----------------------------------------------------------------------------------------------------------
#-----------------------------------------------------------------------------------------------------------
#-----------------------------------------------------------------------------------------------------------

#
temp <- tempdir()


download.file(url = 'https://github.com/justinelliotmeyers/INDIA_2018_DISTRICTS.git',destfile = paste0(temp,'/IndiaDis.zip'))

# unzip the .zip file
unzip(zipfile = paste0(temp,'/IndiaDis.zip'))

india <- st_read("DISTRICTS_2018.shp")


plain_map <- function(){
  
  theme(legend.position = "none", plot.background = element_rect(fill = "white",
      colour = "white"), panel.grid.major = element_line(colour = "transparent"),
      panel.grid.minor = element_blank(), panel.border = element_blank(),
      panel.background = element_blank(), axis.title.y = element_blank(),
      axis.title.x = element_blank(), axis.ticks = element_blank(),
      axis.text = element_blank(), legend.title = element_blank(),
      legend.text = element_text(size = 12))
}


indstate <- india %>%
	group_by(S_NAME) %>%
	summarize()
	
indstate <- indstate %>%
	filter(!(S_NAME %in% c("ANDAMAN & NICOBAR","LAKSHADWEEP")))
	
indstate$pro <- ifelse(indstate$S_NAME %in% 
c("ANDHRA PRADESH", "UTTAR PRADESH", "MADHYA PRADESH", "MAHARASHTRA", "HIMACHAL PRADESH"),"pro","empty") 

indstate <- indstate %>%
 mutate(
    CENTROID = map(geometry, st_centroid),
    COORDS = map(CENTROID, st_coordinates),
    COORDS_X = map_dbl(COORDS, 1),
    COORDS_Y = map_dbl(COORDS, 2)
  ) 

indstate$pro[indstate$S_NAME=="TELANGANA"] <-"Tela"

cbbPalette <- c("white",brewer.pal(9,'Blues')[3],"grey")

metros <- 
data.frame(village = c("Delhi", "Bangalore", "Hyderabad"),
lat = c(28.61,12.983333,17.37),
lon = c(77.23,77.583333,78.48)) 

 
  

cities <- data.frame(
village = c("Visakhapatnam","Amravati",	
"Saidanpur", "Dharni", "Guna","Shivpuri", 	
"Harisal","Dharamhala","Vijayawada"),
lat = c(17.704167,16.541,26.955295,21.782579,24.65, 		
25.43,21.531845,32.218,16.5193),
lon = c(83.297778,80.515,81.392865,78.166161,77.32,
77.65,77.10741,76.32,80.6305)) 


cities <- st_as_sf(cities,coords=c("lon","lat"))%>%
 mutate(
    CENTROID = map(geometry, st_centroid),
    COORDS = map(CENTROID, st_coordinates),
    COORDS_X = map_dbl(COORDS, 1),
    COORDS_Y = map_dbl(COORDS, 2)
  ) 


st_crs(cities) <-4326

metros <- st_as_sf(metros,coords=c("lon","lat")) %>%
 mutate(
    CENTROID = map(geometry, st_centroid),
    COORDS = map(CENTROID, st_coordinates),
    COORDS_X = map_dbl(COORDS, 1),
    COORDS_Y = map_dbl(COORDS, 2)
  ) 

st_crs(metros) <-4326


p <- ggplot(indstate) +
	geom_sf(aes(fill=pro),colour="black") + 
	geom_sf(data =  cities, aes(),colour = "#A80C35", size = 3, inherit.aes=FALSE) +
	geom_sf(data =  metros, aes(),colour = "black",size=4 ,inherit.aes=FALSE) +
	scale_fill_manual(values=cbbPalette,name="") +
#	geom_text_repel( data=cities,mapping = aes(x = COORDS_X, y = COORDS_Y, label = village)) +
#	geom_text_repel( data=metros,mapping = aes(x = COORDS_X, y = COORDS_Y, label = village)) +
	geom_text_repel( data=filter(indstate,S_NAME %in% c("TELANGANA","ANDHRA PRADESH", "UTTAR PRADESH", "MADHYA PRADESH", "MAHARASHTRA", "HIMACHAL PRADESH"))
	,mapping = aes(x = COORDS_X, y = COORDS_Y, label = S_NAME), colour = "#002868") +
	plain_map()+
	NULL
	
ggsave(filename="~/Desktop/Rev Map 1.png",plot= p,width=12,height=10,type="cairo-png" )





#-----------------------------------------------------------------------------------------------------------
#-----------------------------------------------------------------------------------------------------------
#-----------------------------------------------------------------------------------------------------------
#-----------------------------------------------------------------------------------------------------------

APCoord <- indstate %>%
  filter(S_NAME == 'ANDHRA PRADESH') %>%
  st_coordinates() %>%
  data.frame()
APbbox <- c(min(APCoord[,1]),min(APCoord[,2]),max(APCoord[,1]),max(APCoord[,2])) 
names(APbbox) <- c('xmin', 'ymin', 'xmax','ymax')

#zoom out a bit
APbbox[1:2] <- APbbox[1:2] * .99
APbbox[3:4] <- APbbox[3:4] * 1.01


AP <- indstate %>%
  st_crop( APbbox)

APcities <- cities[c(1,nrow(cities)),]

APvillages <- data.frame(
  village = c("Pamarru","Nimmakuru","Munnaluru","Battinapadu","Paderu"),
  lat = c(16.327, 16.2703,16.6197431,16.6496693,18.0833),
  lon = c(80.961,80.9967, 80.3264093, 80.3808054, 82.6667)
)
 
APvillages <- st_as_sf(APvillages,coords=c("lon","lat"))%>%
  mutate(
    CENTROID = map(geometry, st_centroid),
    COORDS = map(CENTROID, st_coordinates),
    COORDS_X = map_dbl(COORDS, 1),
    COORDS_Y = map_dbl(COORDS, 2)
  ) 




st_crs(APvillages) <-4326



AP$COORDS_X[1] <- 78.8
AP$COORDS_Y[1] <- 15


APmetros <- metros [3,]
APmetros$COORDS_Y <- 17.1

AP <- AP %>%
  mutate(highlight = ifelse(S_NAME %in% c("TELANGANA","ANDHRA PRADESH"),'Sampled','No'))

provpal <- rev(c(brewer.pal(9,'Blues')[3],'#dddddd'))

APmap <- ggplot() +
  geom_sf(data = AP, aes(fill =highlight),colour="black") +	
  scale_fill_manual(values=provpal, name = "") +
  geom_sf(data =  APmetros, aes(),colour = "black",size=4 ,inherit.aes=FALSE) +
  geom_sf(data =  APcities, aes(),colour = "#A80C35", size = 3, inherit.aes=FALSE) +
  geom_sf(data =  APvillages, aes(),colour = "#A80C35", size = 3, inherit.aes=FALSE) +
  geom_text(data=filter(AP,S_NAME %in% c("TELANGANA","ANDHRA PRADESH")),mapping = aes(x = COORDS_X, y = COORDS_Y, label =S_NAME))+
  geom_text(data=APmetros,mapping = aes(x = COORDS_X, y = COORDS_Y, label =village),size=3)+
  plain_map()+
NULL

inset <- ggplot() +
  geom_sf(data = indstate, aes(),colour="black") +
  geom_rect(data=data.frame(t(APbbox)), mapping=aes(xmin=xmin, xmax=xmax, ymin=ymin, ymax=ymax), color="black", alpha=0)+
  plain_map()+
  theme(panel.border = element_rect(colour = "black", fill=NA, size=2))+
  NULL

png(file="~/Desktop/Rev Map AP.png",w=1800,h=1800, res=300)
grid.newpage()
v1<-viewport(width = 1, height = 1, x = 0.5, y = 0.5) #plot area for the main map
v2<-viewport(width = 0.3, height = 0.3, x = .75, y = .30) #plot area for the inset map
#v3<-viewport(width = 0.39, height = 0.39, x = 0.18, y = 0.78) #plot area for the inset map
print(APmap,vp=v1) 
print(inset,vp=v2)
dev.off()







#-----------------------------------------------------------------------------------------------------------
#-----------------------------------------------------------------------------------------------------------
#-----------------------------------------------------------------------------------------------------------
#-----------------------------------------------------------------------------------------------------------

NWvillages <- data.frame(
  village = c("Harisal","Dharni","Guna","Shivpuri","Saidanpur","Dharamshala"),
  lat = c(21.5318647,21.7800746,24.6466825,25.4333819,26.9555605,32.2168136),
  lon = c(77.1073889,78.1633387, 77.2770491, 77.6289433, 81.3913268,76.3016127)
)
NWvillages <- st_as_sf(NWvillages,coords=c("lon","lat"))%>%
  mutate(
    CENTROID = map(geometry, st_centroid),
    COORDS = map(CENTROID, st_coordinates),
    COORDS_X = map_dbl(COORDS, 1),
    COORDS_Y = map_dbl(COORDS, 2)
  ) 

st_crs(NWvillages) <-4326


NWCoord <- indstate %>%
  filter(S_NAME %in% c('MAHARASHTRA','MADHYA PRADESH','UTTAR PRADESH','HIMACHAL PRADESH')) %>%
  summarize() %>%
  st_coordinates() %>%
  data.frame()


NWbbox <- c(min(NWCoord[,1]),min(NWCoord[,2]),max(NWCoord[,1]),max(NWCoord[,2])) 
names(NWbbox) <- c('xmin', 'ymin', 'xmax','ymax')

#manually adjust the box to remove parts of maharastra
NWbbox[2]<- 20




NWind <- indstate %>%
  st_crop( NWbbox)

NWind <- NWind %>%
  mutate(highlight = ifelse(S_NAME %in% c('MAHARASHTRA','MADHYA PRADESH','UTTAR PRADESH','HIMACHAL PRADESH'),'Sampled','No'))

NWind$COORDS_Y [13] <- 20.5

NWmetros <- filter(metros,village == "Delhi")
NWmetros$COORDS_X <- 78

NWmap <- ggplot() +
  geom_sf(data = NWind, aes(fill =highlight),colour="black") +	
  scale_fill_manual(values=provpal, name = "") +
  geom_sf(data =  NWmetros, aes(),colour = "black",size=4 ,inherit.aes=FALSE) +
  #geom_sf(data =  APcities, aes(),colour = "#A80C35", size = 3, inherit.aes=FALSE) +
  geom_sf(data =  NWvillages, aes(),colour = "#A80C35", size = 3, inherit.aes=FALSE) +
  geom_text(data=filter(NWind,S_NAME %in% c('MAHARASHTRA','MADHYA PRADESH','UTTAR PRADESH','HIMACHAL PRADESH')),mapping = aes(x = COORDS_X, y = COORDS_Y, label =S_NAME))+
  geom_text(data=NWmetros,mapping = aes(x = COORDS_X, y = COORDS_Y, label =village),size=3)+
  plain_map()+
  NULL

NWinset <- ggplot() +
  geom_sf(data = indstate, aes(),colour="black") +
  geom_rect(data=data.frame(t(NWbbox)), mapping=aes(xmin=xmin, xmax=xmax, ymin=ymin, ymax=ymax), color="black", alpha=0)+
  plain_map()+
  theme(panel.border = element_rect(colour = "black", fill=NA, size=2))+
  NULL

png(file="~/Desktop/Rev Map NW.png",w=1800,h=1800, res=300)
grid.newpage()
v1<-viewport(width = 1, height = 1, x = 0.5, y = 0.5) #plot area for the main map
v2<-viewport(width = 0.28, height = 0.28, x = .8, y = .8) #plot area for the inset map
#v3<-viewport(width = 0.39, height = 0.39, x = 0.18, y = 0.78) #plot area for the inset map
print(NWmap,vp=v1) 
print(NWinset,vp=v2)
dev.off()






  


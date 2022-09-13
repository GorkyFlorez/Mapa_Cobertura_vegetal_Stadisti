


library(sf)
library(ggplot2)
library(ggspatial)
library(raster)
library(ggplot2)
library(ggrepel)
library(tidyverse)
library(dplyr)
library(sf)
library(ggplot2)
library(ggspatial)
library(raster)

col=c("#008000", # Inicio
      "#582f0e", 
      "#f72585", 
      "#006400", 
      "#70e000", 
      "#656d4a", 
      "#bc3908",
      "#9a031e",
      "#fcf6bd", 
      "#caf0f8",
      "#3f37c9", 
      "#3c096c", 
      "#588157", 
      "#d4a373", 
      "#BC782F", 
      "#FF0D00", 
      "#2a9d8f", 
      "#3a86ff"  # Ultimo
)


Peru  <- getData('GADM', country='Peru', level=2) %>% st_as_sf()
Per  <- getData('GADM', country='Peru', level=0) %>% st_as_sf()
Arequipa =  subset(Peru , NAME_1 == "Cusco")

Urrubamba =  subset(Peru , NAME_2 == "Urubamba")

CobVeg_18061 = st_read("SHP/Urubamba_cober.geojson")  %>% st_as_sf()
Cob  <- st_transform(CobVeg_18061 ,crs = st_crs("+proj=longlat +datum=WGS84 +no_defs"))

SurAmerica = st_read("SHP/SurAmerica.geojson")  %>% st_as_sf()
SurAmeric  <- st_transform(SurAmerica  ,
                           crs = st_crs("+proj=longlat +datum=WGS84 +no_defs"))
SurA= ggplot()+
  geom_sf(data = SurAmeric, fill="white", color="black", size=0.01)+
  geom_sf(data = Per , fill="gray", color="black", size=0.01)+
  geom_sf(data = Urrubamba, fill="black", color="black", size=0.5)+
  theme(panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        panel.margin = unit(c(0,0,0,0), "cm"),
        plot.margin = unit(c(0,0,0,0), "cm"),
        axis.title = element_blank(),
        axis.text = element_blank(),
        axis.ticks = element_blank(),
        legend.position = "none", 
        
        panel.background = element_rect(fill = "#a2d2ff"),
        panel.border = element_rect( color = "grey20", fill = NA, size = 1))+
  annotate(geom = "text", x = -60, y = 10, hjust = 0, vjust = 1, 
           label = "a) Sur America",size = 3, family="serif", color = 
             "black",  fontface="italic", face = "bold")+
  annotate(geom = "text", x = -80, y = -40, hjust = 0, vjust = 1, 
           label = "Pacific ocean",size = 3, family="serif", color = 
             "black",  fontface="italic", angle=90)+
  annotate(geom = "text", x = -55, y = -50, hjust = 0, vjust = 1, 
           label = "Atlantic ocean",size = 3, family="serif", color = 
             "black",  fontface="italic")+
  annotate(geom = "text", x = -70, y = -10, hjust = 0, vjust = 1, 
           label = "Peru",size = 3, family="serif", color = 
             "black",  fontface="italic")
SurA
SurA.grob  <- ggplotGrob(SurA)


library(elevatr)
elev = get_elev_raster(Urrubamba, z=13)
plot(elev)
Poligo_alt    <- crop(elev, Urrubamba)                           #   
Poligo_alt   <- Poligo_alt <- mask(Poligo_alt, Urrubamba)
plot(Poligo_alt)

slopee    = terrain(Poligo_alt  , opt = "slope") 
aspecte    = terrain(Poligo_alt, opt = "aspect")
hille     = hillShade(slopee, aspecte, angle = 40, direction = 270)

hill.p        <-  rasterToPoints(hille)
hill.pa_      <-  data.frame(hill.p)
colores = c( 
  "#8e9aaf",#celeste
  "#dda15e", # maroon 
  "#faedcd")#amarillo pastel


Geo_data       <-  rasterToPoints(elev)
Geo_data_frame <-  data.frame(Geo_data)
colnames(Geo_data_frame) <- c("x","y", "alt")

library(ggnewscale) 

eleve = get_elev_raster(Poligono, z=10)
slope    = terrain(eleve  , opt = "slope") 
aspect    = terrain(eleve, opt = "aspect")
hill     = hillShade(slope, aspect, angle = 40, direction = 270)

hill.        <-  rasterToPoints(hill)
hi           <-  data.frame(hill.)
colores = c( 
  "#8e9aaf",#celeste
  "#dda15e", # maroon 
  "#faedcd")#amarillo pastel


Mapa =ggplot()+
  geom_raster(data = hi, aes(x,y, fill = layer), show.legend = F)+
  scale_fill_gradientn(colours=grey(1:100/100))+
  new_scale_fill()+
  geom_sf(data = Peru, fill=NA, color="black", size=0.4)+
  geom_raster(data = hill.pa_, aes(x,y, fill = layer), show.legend = F)+
  scale_fill_gradientn(colours=grey(1:100/100))+
  new_scale_fill()+
  geom_sf(data = Cob, aes(fill=CobVeg2013), alpha=0.6, color=NA)+
  scale_fill_manual(values = col,name='Cobertura Vegetal')+
  geom_sf_text(data = Peru, aes(label=NAME_2 ),family="serif", color="black", alpha=0.6, size =5 )+
  coord_sf(xlim = c(-72.69, -71.83 ), ylim = c(-13.55  ,-13.06)) +
  theme_classic()+
  theme(legend.position = c(0.35, 0.17),
        axis.text.x  = element_text(face="bold", color="black", size=10,
                                    family="serif"),
        axis.text.y  = element_text(angle = 90,face="bold", color="black",
                                    family="serif",size=10),
        axis.title = element_text(face="bold", color="black"),
        legend.background = element_rect(fill = "white"),
        legend.text=element_text(size=9, family="serif"),
        legend.title = element_text(size=9, family="serif"),
        legend.key.size = unit(0.2, "cm"), #alto de cuadrados de referencia
        legend.key.width = unit(0.5,"cm"), #ancho de cuadrados de referencia 
        panel.background = element_rect(fill = "#a9def9"),
        panel.border = element_rect( color = "grey20", fill = NA, size = 0.5))+
  labs(title = '', fill = 'Densidad \n(miles)',  x = 'Longitud', y = 'Latitud')+
  guides(fill = guide_legend(title.position = "top",direction = "vertical"))+
  annotation_north_arrow(location="tl",which_north="true",style=north_arrow_fancy_orienteering ())+
  annotation_scale(location = "br",bar_cols = c("grey60", "white"), text_family = "ArcherPro Book")+
  annotation_custom(SurA.grob, xmin = -72, xmax = -71.75, ymin =-13.3, ymax=-13.05)+
  annotate(geom = "text", x = -75, y = -17, hjust = 0, vjust = 1, angle=45,
           label = "Gorky Florez Castillo            Gorky Florez Castillo        Gorky Florez Castillo",
           size = 7, family="serif", color = "grey20",
           alpha=0.2)

ggsave(plot=Mapa ,"Mapa/Mapa de Covertura cusco1.png",units = "cm",width = 29, #alto
       height = 21, #ancho
       dpi=1200)



CobVeg_18061 = st_read("SHP/Urubamba_cober.geojson")  %>% st_as_sf()
Cob  <- st_transform(CobVeg_18061 ,crs = st_crs("+proj=longlat +datum=WGS84 +no_defs"))


Ha_total = sum(Cob$Shape_Area/10000)

df2  = dplyr::select(Cob, CobVeg2013 , Shape_Area)
df2$Porcentaje =  (df2$Shape_Area/10000)*100/Ha_total

Resu_porcentaje = df2 %>%
  as_tibble %>%
  group_by(CobVeg2013)%>%
  summarize (
             Cantidad =n(),
             Por =sum(Porcentaje))

Resu_area = df2 %>%
  as_tibble %>%
  group_by(CobVeg2013)%>%
  summarize (
             Cantidad =n(),
             area =sum(Shape_Area))

summ <- Resu_porcentaje %>%
  mutate(CobVeg2013 = fct_reorder(CobVeg2013 , Por, .desc = TRUE))

Esta_Porcentaje= ggplot(data = summ, aes(x=CobVeg2013, y= Por, fill=CobVeg2013))+
  scale_fill_manual(values = col)+
  geom_col(alpha = 0.7)+
  geom_text(data = summ, aes(label=round(Por,2)), vjust=0.5, color="black", size=2)+
  theme_bw()+
  theme(panel.grid.major = element_blank(),
        legend.position = c(0.75, 0.75),
        legend.text =element_text(size=8, family="serif"),
        legend.title = element_text(size=8, family="serif"),
        axis.title.x = element_text(color="black", size=11,family="serif",face="bold"),
        axis.title.y = element_text(color="black", size=11,family="serif",face="bold"),
        axis.text.y  = element_text(color="black", size=10, family="serif"),
        axis.text.x  = element_text(color="black", size=10, family="serif", hjust=1),
        legend.key.size = unit(0.3, "cm"), #alto de cuadrados de referencia
        legend.key.width = unit(0.6,"cm"), #ancho de cuadrados de referencia 
        panel.grid.minor = element_blank(),
        axis.text = element_text(size = 11))+
  labs(x= "Cobertura Vegetal de Urubamba",
       y= "Porcentaje (%)",
       fill="Tipo de Cobertura")+
  coord_flip()

Esta_Porcentaje

Resu_area$ha= Resu_area$area/10000

sum_Area <- Resu_area %>%
  mutate(CobVeg2013 = fct_reorder(CobVeg2013 , ha, .desc = TRUE))

Esta_Area= ggplot(data = sum_Area, aes(x=CobVeg2013, y= ha, fill=CobVeg2013))+
  scale_fill_manual(values = col)+
  geom_col(alpha = 0.7)+
  theme_bw()+
  theme(panel.grid.major = element_blank(),
        legend.position = c(0.75, 0.75),
        legend.text =element_text(size=8, family="serif"),
        legend.title = element_text(size=8, family="serif"),
        axis.title.x = element_text(color="black", size=11,family="serif",face="bold"),
        axis.title.y = element_text(color="black", size=11,family="serif",face="bold"),
        axis.text.y  = element_text(color="black", size=10, family="serif"),
        axis.text.x  = element_text(color="black", size=10, family="serif", hjust=1),
        legend.key.size = unit(0.3, "cm"), #alto de cuadrados de referencia
        legend.key.width = unit(0.6,"cm"), #ancho de cuadrados de referencia 
        panel.grid.minor = element_blank(),
        axis.text = element_text(size = 11))+
  geom_text(data = sum_Area, aes(label=round(ha,2)), vjust=0.5, color="black", size=2)+
  labs(x= "Cobertura Vegetal de Urubamba",
       y= "Hectárea (ha)",
       fill="Tipo de Cobertura")+
  coord_flip()

Esta_Area

GG_Por_Area = ggpubr::ggarrange(Esta_Porcentaje,Esta_Area, labels = c("A", "B"), 
                      ncol = 2, nrow = 1, common.legend = T, legend="bottom")+
  theme(panel.background = element_rect(fill = "white"))

ggsave(plot=GG_Por_Area ,"Mapa/Mapa de porcentaje_area.png",units = "cm",width = 31, #alto
       height = 19, #ancho
       dpi=1200)


library(cowplot)
Final=ggdraw() +
  coord_equal(xlim = c(0, 29), ylim = c(0, 17), expand = FALSE) +
  draw_plot(Esta_Porcentaje , width = 19, height = 19,x = -3, y = 1)+
  draw_plot(Esta_Area, width = 16, height = 18,x = 13, y = 1)+
  
  theme(panel.background = element_rect(fill = "white"),
        panel.border = element_rect( color = "black", fill = NA, size = 1))+
  annotate(geom = "text", x = 8, y = 3, hjust = 0, vjust = 1, angle=45,
           label = "Gorky Florez Castillo            Gorky Florez Castillo        Gorky Florez Castillo",
           size = 7, family="serif", color = "grey20",
           alpha=0.2)


Final

dtafram = data.frame(Porcentaje =summ$Por,
                      Hectarias = sum_Area$ha,
                      Cobertura = Resu_area$CobVeg2013)

dtaframe <-dtafram %>%
  mutate(Cobertura = fct_reorder(Cobertura, Hectarias , .desc = TRUE))

Factor = 20000

A= ggplot()+
  geom_bar(data =dtaframe, aes(x= Cobertura, y=  Porcentaje, fill=Cobertura), stat="identity",alpha = 0.7)+
  geom_line(data=dtaframe, aes(x=Cobertura, y=Hectarias/1400, group=1, fill="red", linetype = "dashed"),
            show.legend = F)+
  geom_point(data =dtaframe, aes(x= Cobertura, y=Hectarias/1400), color="red")+
  scale_fill_manual(values = col)+
  theme_bw()+
  theme(panel.grid.major = element_blank(),
        legend.position = c(0.75, 0.65),
        legend.text =element_text(size=8, family="serif"),
        legend.title = element_text(size=8, family="serif"),
        axis.title.x = element_text(color="black", size=11,family="serif",face="bold"),
        axis.title.y = element_text(color="black", size=11,family="serif",face="bold"),
        axis.text.y  = element_text(color="black", size=10, family="serif", angle=90),
        axis.text.x  = element_text(color="black", size=10, family="serif",angle = 90, hjust=1),
        legend.key.size = unit(0.3, "cm"), #alto de cuadrados de referencia
        legend.key.width = unit(0.6,"cm"), #ancho de cuadrados de referencia 
        panel.grid.minor = element_blank(),
        axis.text = element_text(size = 11))+
  scale_y_continuous(sec.axis = sec_axis(trans= ~.*Factor, name="Hectárea (ha)"))+
  labs(x= "Cobertura Vegetal de Urubamba",
       y= "Porcentaje (%)",
       fill="Tipo de Cobertura")

A


library(scales)
B = ggplot(Resu_porcentaje ,aes(x=2,y=Por, fill=CobVeg2013))+
  geom_bar(stat = "identity",
           color="white")+
  geom_text(aes(label=percent(round(Por,2)/100)),
            position=position_stack(vjust=0.5),color="white",size=4)+
  coord_polar(theta = "y")+
  scale_fill_manual(values = col,name='Cobertura Vegetal')+
  theme_void()+
  theme(panel.background = element_rect(fill = "white"))+
  labs(title="Gráfico de Dona")+
  xlim(0.5,2.5)

library(ggpubr)
library(ggplot2)
library(webr)
library(dplyr)
require(moonBook)
require(webr)

dtaframe$Porcentaje =round(dtaframe$Porcentaje,2)
B= PieDonut(dtaframe,aes(Cobertura, Porcentaje),
            title = "Coberturas",
            explode = 1, explodeDonut=T,
            color = "white")


MM= ggpubr::ggarrange(A,B, labels = c("A", "B"), 
                      ncol = 2, nrow = 1, common.legend = T, legend="bottom")+
  theme(panel.background = element_rect(fill = "white"))


ggsave(plot=MM ,"Mapa/Mapa de estadistica.png",units = "cm",width = 29, #alto
       height = 21, #ancho
       dpi=1200)





library('plotly')
library('dplyr')

data <- data.frame(c('cat', 'dog', 'deer','chicken', 'cat', 'dog','duck', 'monkey', 'fish','cow','horse','dog'),c('US', 'US', 'US','US', 'UK', 'UK','UK', 'UK','China','China','China','China'),c(15,70,120,55,47,300,89,62,40,27,103,8))
colnames(data) <- c('animal', 'country', 'total_num')

p <- plot_ly(data) %>% add_pie(labels = ~animal, values = ~total_num, type = 'pie', hole = 0.7, sort = F) %>% add_pie(data, labels = ~country, values = ~total_num, domain = list(x = c(0.15, 0.85),y = c(0.15, 0.85)),sort = F)

p





ggpubr::ggarrange(p,p, labels = c("A", "B"), 
                  ncol = 2, nrow = 1, common.legend = T, legend="bottom")+
  theme(panel.background = element_rect(fill = "white"))


























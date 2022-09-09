#load packages
library(sf) # Simple Features for R
library(rnaturalearth) # World Map Data from Natural Earth
library(here) # A Simpler Way to Find Your Files
library(stars) # Spatiotemporal Arrays, Raster and Vector Data Cubes
library(dplyr) # A Grammar of Data Manipulation
library(ggplot2) # Create Elegant Data Visualisations Using the Grammar of Graphics
library(ggnewscale) # Multiple Fill and Color Scales in 'ggplot2'
library(scico) # Colour Palettes Based on the Scientific Colour-Maps
library(raster) 
library(rgdal)
library(memisc)
library(shiny)
library(tmap)
Sys.setlocale(category = "LC_ALL", locale = "Greek")



setwd(dirname(rstudioapi::getActiveDocumentContext()$path))

# fortwnw to ratser arxeio gia ta upsometrika dedomena ths mesogeiou
altitude_mediterranean_data <- raster("30n000e_20101117_gmted_mea300.tif")


#fortwnw ta dedomena gia tous nomous ths Elladas

perfectures_greece<- shapefile("GRC_ADM2/GRC_ADM2.shp")
perfectures_greece_sf<-st_as_sf(perfectures_greece)


#fortwnw ta dedomena gia tis prwtevouses twn nomwn ths Elladas
capitals <- shapefile("poleis/poleis.shp")
capitals_sf<-st_as_sf(capitals)



#to rater arxeio gia ta upsometrika dedomena ths Elladas
greece_raster<- st_transform(perfectures_greece_sf, projection(altitude_mediterranean_data))
greece_cropped = crop(altitude_mediterranean_data, greece_raster)


#oi prwtevouses twn nomwn ths elladas me ta upsometra tous
capitals1<-capitals_sf
capitals1$altitude<-raster::extract(greece_cropped,capitals_sf)


#briskw ta mesa upsometra twn nomwn ths Elladas kai tis tupikes apokliseis twn upsometrwn tous

altitude_perfectures_values = raster::extract(x =altitude_mediterranean_data , y = perfectures_greece_sf, df = TRUE) 


mean_sd_alt_perfecture<-group_by(altitude_perfectures_values, ID) %>% 
  summarize(mean_alt = mean(X30n000e_20101117_gmted_mea300, na.rm = TRUE),sd_alt= sd(X30n000e_20101117_gmted_mea300, na.rm = TRUE))


perfectures_greece_sf1<-perfectures_greece_sf
perfectures_greece_sf1$ID<-as.numeric(perfectures_greece_sf1$feature_id)+1

perfecture_with_mean_sd_altitude<-merge(perfectures_greece_sf1,mean_sd_alt_perfecture )
summary(perfecture_with_mean_sd_altitude)
perfecture_with_mean_sd_altitude$meso_upsometro<-round(perfecture_with_mean_sd_altitude$mean_alt,4)
#perfecture_with_mean_sd_altitude[which(perfecture_with_mean_sd_altitude$meso_upsometro==293.9629),]
perfecture_with_mean_sd_altitude$sd_upsometro<-round(perfecture_with_mean_sd_altitude$sd_alt,4)

#################################################shiny app############################################################################3


ui <- fluidPage(
  titlePanel("Map of Creece"),
  h3(""),
  "Oi nomoi pou ikanopoioun tous periorismous emfanizontai me prasino xrwma, alliws emfanizontai me gkri xrwma.",
  br(),
  "Sto xarth emfanizontai mono oi poleis pou to upsometro tous vrisketai entos twn oriwn.",
  sidebarLayout(
    sidebarPanel(
      sliderInput("upsometro_prwtevousas","upsometro prwtevousas",value =c(min(capitals1$altitude),max(capitals1$altitude)),min = min(capitals1$altitude),max = max(capitals1$altitude),step=1),
      sliderInput("meso_upsometro_nomou","meso upsometro nomou",value =c(min(perfecture_with_mean_sd_altitude$meso_upsometro),max(perfecture_with_mean_sd_altitude$meso_upsometro)),min = min(perfecture_with_mean_sd_altitude$meso_upsometro),max = max(perfecture_with_mean_sd_altitude$meso_upsometro),step=0.0001),
      sliderInput("tupikh_apoklish_nomou","tupikh apoklish nomou",value = c(min(perfecture_with_mean_sd_altitude$sd_upsometro),max = max(perfecture_with_mean_sd_altitude$sd_upsometro)),min = min(perfecture_with_mean_sd_altitude$sd_upsometro),max = max(perfecture_with_mean_sd_altitude$sd_upsometro),step=0.0001)),
    mainPanel(
      tabsetPanel(
        tabPanel("Map",tmapOutput("map"))
        
      )
    )
  )
)


server <- function(input, output) {
  output[["map"]] <- renderTmap({
    #to d1 elegxei poioi nomoi exoun meso upsometro kai tupikh apoklish
    #metaksu twn epitreptwn oriwn pou orizontai apo ta sliders
    d1 <- filter(perfecture_with_mean_sd_altitude,meso_upsometro >=input[['meso_upsometro_nomou']][1] & meso_upsometro <=input[['meso_upsometro_nomou']][2] & sd_upsometro >=input$tupikh_apoklish_nomou[1] & sd_upsometro <=input$tupikh_apoklish_nomou[2])
    
    #entoles gia na eleksoun an to d1 exei empty units h oxi
    check <- st_is_empty(d1)
    total_check<-all(check)
    
    d2<-filter(capitals1,altitude>=input$upsometro_prwtevousas[1] & altitude<=input$upsometro_prwtevousas[2])
    
    #dhmiourgw 2 xartes analoga me to an an uparxei toulaxiston enas nomos pou na brisketai metaksu
    #twn epithumhtwn oriwn, auto to elegxw me to an to d1 exei empty units h oxi mesw twn entolwn check kai total_check
    
    #o xarths o paraktw emfanizetai otan to d1 exei empty units ousistika den xrwmatizetai kanenas nomos all emfanizontai sto
    #xarth mono oi prwtevouses pou einai entos twn epitreptwn oriwn apo to slider gia to upsometro thw prwtevousas
    
    if (all(total_check)){
      tm_shape(perfecture_with_mean_sd_altitude)+tm_polygons( midpoint = 0, border.col="black")+tm_fill("grey")+
        tm_shape(d2) + tm_dots( col="black")+tm_text("ONOMA", just = "left", xmod = 0.5, size =0.5 )+ tm_layout(legend.show = FALSE)
    }
    
    #o paraktw xarths emfanizetai mono otan to d1  exei toulaxiston 1 unit, tote emfanizontai me prasino xrwma ekeinoi oi nomoi
    #pou exoun meso upsometro kai tupikh apoklish pou orizontai apo ta sliders kai oi prwtevouses emfanizontai me koukides
    #otan ta upsometra tous vriskontai entos twn epitreptwn oriwn pou orizontai apo to prwto slider
    
    else{
      tm_shape(perfecture_with_mean_sd_altitude)+tm_polygons( midpoint = 0, border.col="black")+tm_fill("grey")+
        tm_shape(d1) +tm_fill("darkolivegreen3")+
        tm_polygons(border.col="white")+tm_shape(d2) + tm_dots( col="black")+tm_text("ONOMA", just = "left", xmod = 0.5, size =0.5 )+ tm_layout(legend.show = FALSE)
      
    }
  })
}
shinyApp(ui = ui, server = server)








rm(list = ls()) 

library(leaflet)
library(rgdal)
library(RColorBrewer)
library(shiny)
library(ggplot2)
library(BBmisc)
library(magick)
library(pdftools)
library(imager)
library(tidyverse)



title <- magick::image_read_pdf("Intro.pdf")
plot(title)
LME_hist <- read.csv("LME_hist.csv")
LME_hist.info()

mol <- load.image("Mol_img.png")

hist <- read.csv("LME_hist.csv")
df <- as_tibble(hist)
colnames(df)

names(df)[1] <- "Crustaceans"
names(df)[3] <- "Other_inverts"

write.csv(df, "C:/Users/stuar/OneDrive/Documents/UNI leeds/Marine/Invert_database/Interview_dashboard\\hist_final.csv")
LME_hist <- read.csv("hist_final.csv")
LME_hist

hist %>% 
  rename(
    ï..Crustaceans = Crustaceans)

LME <- readOGR("LME_ave.shp")
LME_DF <-LME@data

completeFun <- function(data, desiredCols) {
  completeVec <- complete.cases(data[, desiredCols])
  return(data[completeVec, ])
}
LME_DF <- completeFun(LME_DF, "commercial")
LME_DF

LME_test2 <- LME_DF[LME_DF$commercial  == 'Crustaceans',]
LME_test2



LME_type <- unique(LME_DF$commercial)




LME2 <- LME[LME$Commercial  == 'Other fishes & inverts']
LME2

test <- countries$Population


countries <- readOGR("Joined_country_values.shp")
#LME <- readOGR("LME66/LMEs66.shp")
All_points <- readOGR("All_points.shp")
Bright_spots <- readOGR("bright_spots.shp")
LME <- readOGR("LME_fin.shp")
LME_DF <-LME@data
df_Bright <- Bright_spots@data

df_bright_norm <- normalize(df_Bright$Density, method = "standardize", range = c(0, 1), margin = 1L, on.constant = "quiet")

completeFun <- function(LME_DF, commercial) {
  completeVec <- complete.cases(LME_DF[, commercial])
  return(LME_DF[completeVec, ])
}



LME_2 <- completeFun(LME_DF, "commercial")

lst.measures <- unique(LME_DF$commercial)


df_all <- All_points@data

# 1st map is the 66 LME's
m <- leaflet() %>%
  addTiles() %>%
  addPolygons(data = LME,
              weight = 1,
              smoothFactor = 1) 
m



# 2nd map countries variables

# I want a feature where you can filter by variable
# and another feature that greates graphs depending on what country you select


# countries population
# simple chloro
m <- leaflet(countries)%>% addTiles()  %>% setView( lat=10, lng=0 , zoom=2) %>%
  addPolygons( stroke = FALSE, fillOpacity = 0.5, smoothFactor = 0.5, color = ~colorQuantile("YlOrRd", Population)(Population) )
m



# Final Map pop map
mypalette <- colorQuantile( palette="YlOrBr", domain=countries@data$Population, na.color="transparent")


mytext <- paste(
  "Country: ", countries@data$CNTRY_NAME,"<br/>", 
  "Population: ", round(countries@data$Population, 2), 
  sep="") %>%
  lapply(htmltools::HTML)


m <- leaflet(countries) %>% 
  addTiles()  %>% 
  setView( lat=10, lng=0 , zoom=2) %>%
  addPolygons( 
    fillColor = ~mypalette(Population), 
    stroke=TRUE, 
    fillOpacity = 0.9, 
    color="white", 
    weight=0.3,
    label = mytext,
    labelOptions = labelOptions( 
      style = list("font-weight" = "normal", padding = "3px 8px"), 
      textsize = "13px", 
      direction = "auto"
    ),
    highlightOptions = highlightOptions(bringToFront = TRUE, opacity = 1, weight = 5, sendToBack = FALSE, color = "white")
    
  ) %>%
  addLegend( pal=mypalette, values=~Population, opacity=0.9, title = "Population", position = "bottomleft" )


m  




## lme test

mypalette <- colorQuantile( palette="YlOrBr", domain=countries@data$Population, na.color="transparent")


mytext <- paste(
  "Country: ", countries@data$CNTRY_NAME,"<br/>", 
  "Population: ", round(countries@data$Population, 2), 
  sep="") %>%
  lapply(htmltools::HTML)


m <- leaflet(countries) %>% 
  addTiles()  %>% 
  setView( lat=10, lng=0 , zoom=2) %>%
  addPolygons( 
    fillColor = ~mypalette(Population), 
    stroke=TRUE, 
    fillOpacity = 0.9, 
    color="white", 
    weight=0.3,
    label = mytext,
    labelOptions = labelOptions( 
      style = list("font-weight" = "normal", padding = "3px 8px"), 
      textsize = "13px", 
      direction = "auto"
    ),
    highlightOptions = highlightOptions(bringToFront = TRUE, opacity = 1, weight = 5, sendToBack = FALSE, color = "white")
    
  ) %>%
  addLegend( pal=mypalette, values=~Population, opacity=0.9, title = "Population", position = "bottomleft" )


m  






















leaflet(countries) %>%
  addTiles()  %>% 
  setView(lng = 00.00, lat = 00.00, zoom = 3) %>%
  addProviderTiles("Esri.WorldImagery") %>%
  addPolygons( 
    fillColor = ~mypalette(population), 
    stroke=TRUE, 
    fillOpacity = 0.9, 
    color="white", 
    weight=0.3,
    label = mytext,
    labelOptions = labelOptions( 
      style = list("font-weight" = "normal", padding = "3px 8px"), 
      textsize = "13px", 
      direction = "auto"
    )
  ) %>%
  addLegend( pal=mypalette, values=~data2, opacity=0.9, title = country_values, position = "bottomleft" )




leaflet(countries) %>%
  addTiles()  %>% 
  setView(lng = 00.00, lat = 00.00, zoom = 3) %>%
  addProviderTiles("Esri.WorldImagery") %>%
  addPolygons( 
    fillColor = ~mypalette(population), 
    stroke=TRUE, 
    fillOpacity = 0.9, 
    color="white", 
    weight=0.3,
    label = mytext,
    labelOptions = labelOptions( 
      style = list("font-weight" = "normal", padding = "3px 8px"), 
      textsize = "13px", 
      direction = "auto"
    )
  ) %>%
  addLegend( pal=mypalette, values=~data2, opacity=0.9, title = country_values, position = "bottomleft" )


# add oceans base map


# 3rd map points 



# mypalette <- colorFactor(domain=All_points@data$Phylum, na.color="transparent")
# all points has no phylum

mytext <- paste(
  "Density: ", round(All_points@data$den, 2), 
  sep="") %>%
  lapply(htmltools::HTML)



leaflet(All_points) %>% 
      addTiles()  %>% 
      setView( lat=0, lng=0 , zoom=2) %>%
      addProviderTiles("Esri.WorldImagery") %>%
      addCircleMarkers( 
        fillColor = "red",
        stroke=TRUE, 
        fillOpacity = 0.9, 
        color="white", 
        radius = ~ sqrt(den),
        weight=0.3,
        label = mytext,
        labelOptions = labelOptions( 
          style = list("font-weight" = "normal", padding = "3px 8px"), 
          textsize = "13px", 
          direction = "auto"),
       
      )


# correlation plot

df_all <- All_points@data

ggplot(df_all, aes(x=den, y=depth)) + 
  geom_point()+
  geom_smooth(method=lm, se=FALSE)


ggplot(df_all, aes(x=depth, y=den)) +
  geom_point()


Bright_spots <- readOGR("bright_spots.shp")
df_Bright <- Bright_spots@data


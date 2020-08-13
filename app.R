#
# version 2
# To do list:
# switch out countries quantiles for better numbers
# refine the population numbers
# color for the species type (make it a toggle button)
# investigate proxy for processing times
# add icons to buttons and space approriatly
# put something below country map
# make side bar selction only appear when relevent
# think about merging country and reef
# find solution beyond sqrts for reef sites or make all the values smaller to begin with
# fix x axis of reef graphs
# maybe try out something like an activate bright spots button
# add country labels to spots so

# home page my data story 
# don't know if base map works if published to server


# for radius size have two sep dfs one that's strdized one that's actual values
# toggle color for species / phylum
# make map fill panels
# def start looking into proxy
# density

# THE ISSUE
# CREATE A SLIDE WHAT CAN WE DO ABOUT IT
# CREATE A SLIDE MODEL OR MAKE SAME SLIDE
# EXPLAIN MORE ABOUT THE GRAPHICS
# PERRCENT TO TONNES

# HOW DOES IT SHOW SUCCESS ON A REEF

#START WITH WORLDS CORAL REEFS SITE MAP
# THERE'S ALOT OF OTHER THREATS TO CORAL REEFS LIKE TOURISM

# TALK ABOUT HOW INVERTEBRATES ARE IMPORTANT TO CORAL REEFS -- EDUCATE SHOW PASSION

# Make data1.1 that y axis into actual name
#PUSH BOTH GRAPHS DOWN AND OFF THE PAGE



# make blue outline red

# frame issue first


#rm(list = ls()) 
library(shiny)
library(shinydashboard)
library(leaflet)
library(rgdal)
library(RColorBrewer)
library(rintrojs)
library(shinyBS)
library(magick)
library(pdftools)
library(imager)


title <- magick::image_read_pdf("Intro.pdf")
mol <- magick::image_read_pdf("mol.pdf")
crus <- magick::image_read_pdf("crus.pdf")
other <- magick::image_read_pdf("other.pdf")
Solution <- magick::image_read_pdf("Solution.pdf")
Model <- magick::image_read_pdf("Model.pdf")
countries <- readOGR("Joined_country_values.shp")
LME <- readOGR("LME_ave.shp")
LME_hist <- read.csv("hist_final.csv")
Bright_spots <- readOGR("bright_spots.shp")
All_points <- readOGR("All_points.shp")
df_all <- All_points@data
df_Reef <- All_points@data
df_Bright <- Bright_spots@data
# df_Reef_strd <-
LME_DF <-LME@data





vars_LME <- c(
  "Molluscs",
  "Crusteans",
  "Other Invertebrates"
)


vars <- c(
    "Population",
    "HDI",
    "Peoples Voice",
    "Tourism"
)

site_vars <- c(
    "Density",
    "Depth",
    "Gravity",
    "Regional Population Growth",
    "MPA Score",
    "Productivity"
)

Bright_vars <- c(
    "Deviation From Expectations",
    "Density",
    "Depth",
    "Gravity",
    "Regional Population Growth",
    "MPA Score",
    "Productivity"
)



# -------------------------------------- UI -----------------------------------------------------

ui <- dashboardPage(
    skin = "blue",
    dashboardHeader(title = "My Data Story"),
    
    dashboardSidebar(
        selectInput("Phylum", "1. select a Phylum:", vars_LME
        ),
        selectInput("country_values", "2. Choose a Country Variable:", vars
        ),
        selectInput("Reef_values", "3. Choose a Site Variable:", site_vars
        ),
        selectInput("Bright_values", "4. Choose a Bright Variable:", Bright_vars
        )
    ),
    
    dashboardBody(
        tabsetPanel(type = "tabs",
                    
                    
                    tabPanel("Intro", 
                    plotOutput("Title1", height = 600, width = "100%") # look in the rendered text section
                    ),
                    
                    # Reefs Tab
                    tabPanel("Reefs", 
                             # "Reef cluster locations", 
                             # textOutput("where in the world reefs are"), 
                             p(),
                             leafletOutput("Reefs", height=800),
                             plotOutput("Reefs_cor")
                    ),
                    
                    tabPanel("Fisheries", 
                    leafletOutput("LME_ave",  height=500),
                    splitLayout(
                      style = "border: 1px solid silver;",
                      cellWidths = 800,
                      cellArgs = list(style = "padding: 6px"),
                      plotOutput("Year_VS_Phylum"),
                      plotOutput("species_img")
                    )
                    ),
                    
                    
                    tabPanel("Model", 
                             plotOutput("Solution", height = 600, width = "100%"),
                             br(),
                             br(),
                             br(),
                             br(),

                             plotOutput("Models", height = 600, width = "100%") # look in the rendered text section
                    ),
                    

                    
                    # Countries Tab
                    tabPanel("Countries", 
                              #"Country Viewer", 
                             # textOutput("Country Viewer"), 
                             #fluidRow(box(width = 13, height = 50, leafletOutput("country_map")))
                             leafletOutput("country_map",  height=800)
                    ),
                    
                    # Bright spots Tab
                    tabPanel("Bright Spots", 
                             # "Bright Spot locations", 
                             # textOutput("where in the world reefs are"), 
                             p(),
                             leafletOutput("Brights", height=800),
                             plotOutput("Brights_cor")
                    )
                    
        ),
        
        
        # Countries map
        # textOutput("selected_var"),
        # fluidRow(box(width = 12, height = 22, leafletOutput("country_map")))
    )
)









# ----------------------------------------- Server --------------------------------------------




server <- function(input, output) {
    
  
    output$Title1 <- renderPlot({
      plot(title)
    })
  
  
    
# -----------------------------------------   LME -----------------------------------------------------
    #output$country_map <- renderText()
  
    
    # histogram will be x <- year y <- input$Phylum
      # maybe make it clickable if possible so based on polygon clicked
    # on right have a photo of the animal
    
    
    
    output$LME_ave <- renderLeaflet({

        Phylum <- input$Phylum
      
        data1 <- switch(input$Phylum, 
                          "Molluscs" = LME$Ave_.Mollu,
                          "Crusteans" = LME$Ave_Crusta,
                          "Other Invertebrates" = LME$Other_inve,
        )
        
         mytext <- paste(
            "LME: ", LME@data$LME_NAME,"<br/>", 
            "Number of tonnes", ": ", data1, 
            sep="") %>%
            lapply(htmltools::HTML)
        
         
        #binpal <- colorBin("Blues", domain=data1, 6, na.color="transparent", pretty = FALSE) 
        bins <- c(0, 30000, 100000, 800000)
       mypalette <- colorBin(palette="Reds", domain=data1, bins =bins, na.color="transparent")
       
        leaflet(LME) %>%
            addTiles()  %>% 
            setView(lng = 00.00, lat = 00.00, zoom = 3) %>%
            addProviderTiles("Esri.WorldImagery") %>%
            addPolygons( 
                fillColor = ~mypalette(data1), 
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
            addLegend( pal=mypalette, values=~data1, opacity=0.9, title = "tonnes", position = "bottomleft")
        
    })
    
    
    # ------- hist -----------
    
    output$Year_VS_Phylum <- renderPlot ({
      
      Phylum <- input$Phylum
      
      data1.1 <- switch(input$Phylum, 
                      "Molluscs" = LME_hist$Molluscs,
                      "Crusteans" = LME_hist$Crustaceans,
                      "Other Invertebrates" = LME_hist$Other_inverts,
      )
      
      
      p <- ggplot(data=LME_hist, aes(x=Year, y=data1.1)) +
        geom_bar(stat="identity", fill="steelblue")+
        theme_minimal()
      p + ylab("Tonnes")
      
    })
    
    
    # ----------------- image
    
    output$species_img <- renderPlot({
      Phylum <- input$Phylum
      
      data1.2 <- switch(input$Phylum, 
                        "Molluscs" = mol,
                        "Crusteans" = crus,
                        "Other Invertebrates" = other,
      )
      
      
      plot(data1.2)
    })
    
    
    
    # ------------------------- model -----------------------------------
    
    output$Solution <- renderPlot({
      plot(Solution)
    })
    
    output$Models <- renderPlot({
      plot(Model)
    })
    
    
    
    
    
    
   # -------------------------------------------   country -------------------------------------------------------
#output$country_map <- renderText()
    output$selected_var <- renderText({
        paste("you are viewing", input$country_values)
    })
    
    output$country_map <- renderLeaflet({
        
        country_values <- input$country_values
        
        data2 <- switch(input$country_values, 
                       "Population" = countries$Population,
                       "HDI" = countries$HDI,
                       "Peoples Voice" = countries$Voice,
                       "Tourism" = as.numeric(countries$Tourism)
                       )
        mytext <- paste(
            "Country: ", countries@data$CNTRY_NAME,"<br/>", 
            country_values, ": ", data2, 
            sep="") %>%
            lapply(htmltools::HTML)
        
        
        
        mypalette <- colorQuantile(palette="YlOrBr", domain=data2, na.color="transparent")
        
        leaflet(countries) %>%
            addTiles()  %>% 
            setView(lng = 00.00, lat = 00.00, zoom = 3) %>%
            addProviderTiles("Esri.WorldImagery") %>%
            addPolygons( 
                fillColor = ~mypalette(data2), 
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
            addLegend( pal=mypalette, values=~data2, opacity=0.9, title = country_values, position = "bottomleft" )
        
    })
    
# ---------------------------------------------- Reef --------------------------------------------------------------
    output$Reefs <- renderLeaflet({
    
    Reef_values <- input$Reef_values
        
    
    data3 <- switch(input$Reef_values, 
                    "Density" = All_points$den,
                    "Depth" = All_points$depth,
                    "Gravity" = as.numeric(All_points$Grav_tot),
                    "Regional Population Growth" = as.numeric(All_points$reg_pop_gr),
                    "MPA Score" = as.numeric(All_points$MPA_SCORE),
                    "Productivity" = as.numeric(All_points$Productivi)
                    )
                    
    mytext_reef <- paste(
        Reef_values, ": ", data3,
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
            radius = ~ sqrt(data3),
            weight=0.3,
            label = mytext_reef,
            labelOptions = labelOptions( 
                style = list("font-weight" = "normal", padding = "3px 8px"), 
                textsize = "13px", 
                direction = "auto")
        ) 
    })
    
    output$Reefs_cor <- renderPlot({
        
        Reef_values <- input$Reef_values
        
        data3 <- switch(input$Reef_values, 
                        "Density" = All_points$den,
                        "Depth" = All_points$depth,
                        "Gravity" = as.numeric(All_points$Grav_tot),
                        "Regional Population Growth" = as.numeric(All_points$reg_pop_gr),
                        "MPA Score" = as.numeric(All_points$MPA_SCORE),
                        "Productivity" = as.numeric(All_points$Productivi)
        )
        
        ggplot(df_all, aes(x=data3, y=den)) +
        geom_point()
    })
    
    
# ------------------------------------------   Bright Spots -------------------------------------------------
   
    output$Brights <- renderLeaflet({
        
        Bright_values <- input$Bright_values
        
        
        data4 <- switch(input$Bright_values, 
                        "Deviation From Expectations"= Bright_spots$median_vla,
                        "Density" = Bright_spots$Density,
                        "Depth" = Bright_spots$depth,
                        "Gravity" = as.numeric(Bright_spots$Grav),
                        "Regional Population Growth" = as.numeric(Bright_spots$reg_pop_gr),
                        "MPA Score" = as.numeric(Bright_spots$Rating),
                        "Productivity" = as.numeric(Bright_spots$Productivi)
        )
        
        mytext_Bright <- paste(
            Bright_values, ": ", data4,
            sep="") %>%
            lapply(htmltools::HTML)
        
        
        leaflet(Bright_spots) %>% 
            addTiles()  %>% 
            setView( lat=0, lng=0 , zoom=2) %>%
            addProviderTiles("Esri.WorldImagery") %>%
            addCircleMarkers( 
                fillColor = "yellow",
                stroke=TRUE, 
                fillOpacity = 0.9, 
                color="white", 
                radius = ~ sqrt(data4),
                weight=0.3,
                label = mytext_Bright,
                labelOptions = labelOptions( 
                    style = list("font-weight" = "normal", padding = "3px 8px"), 
                    textsize = "13px", 
                    direction = "auto")
            ) 
    })
    
    output$Brights_cor <- renderPlot({
        
        Bright_values <- input$Bright_values
        
        data4 <- switch(input$Bright_values, 
                        "Deviation From Expectations"= Bright_spots$median_vla,
                        "Density" = Bright_spots$Density,
                        "Depth" = Bright_spots$depth,
                        "Gravity" = as.numeric(Bright_spots$Grav),
                        "Regional Population Growth" = as.numeric(Bright_spots$reg_pop_gr),
                        "MPA Score" = as.numeric(Bright_spots$Rating),
                        "Productivity" = as.numeric(Bright_spots$Productivi)
        )
        
        ggplot(df_Bright, aes(x=data4, y=Density)) +
            geom_point()
    })
    
    
    
    
}



    

# Run the application 
shinyApp(ui = ui, server = server)

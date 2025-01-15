###########################
##   NB Aquaculture EA   ##
###########################

require(dplyr)
require(ggmap)
require(leaflet)
require(leaflet.extras)
require(leafpop)
require(mapedit)
require(miniUI)
require(readr)
require(sf)
require(shiny)
require(shinyWidgets)
require(stringr)
require(tidyr)
require(tricky)
require(DT)


#######################################
##  load in the geom_split function  ##
#######################################

# geom_split <- function(x){
#   
#   p <- as.data.frame(str_split_fixed(x, ",", 2))
#   
#   names(p) <- c("lon", "lat")
#   p$lon <- as.numeric(gsub("c//(", "", p$lon))
#   p$lat <- as.numeric(gsub("//)", "", p$lat))
#   p
# }
##########################################################

#setwd("C:/Users/englishm/Documents/Lobster Bay/app/")


##################################
##   load in  study area data   ##
##################################

#create polygon out of point coordinates and buffer:


# # #West Bay NS near Parrsboro
# # 
# df <- data.frame(
#   lat = 45.399852,
#   lon = -64.384008
# )
# 
# df.sf <- df %>%
#   st_as_sf(coords = c("lon", "lat"))%>%
#   st_set_crs(4326)
# 
# df.utm <- st_transform(df.sf, 32620) #CRS 32620 for this
# 
# #buffer by 3000m for Higgins Shore or 150m for Cape Edgmont
# df.3000 <- st_buffer(df.utm, dist = 3000)
# 
# #turn to polygon:
# df.3000 <- df.3000 %>%
#   summarise(geometry = st_combine(geometry)) %>%
#   st_cast("POLYGON")
# 
# #transform back to lat/lon
# df.3000 <- st_transform(df.3000, 4326) #CRS 4326 for this
# 
# 
# study.site <- df.3000


study.site <- st_read(dsn = "C:/Users/englishm/Documents/EA/2025/2025 Strait Shores NB/StraitShores.kml")

study.site <- st_transform(study.site, 4326)

#drop the 3D field from the google earth shapefile
study.site <- st_zm(study.site, drop = T, what = "ZM")


#################
##   CH DATA   ##
#################

ch <- st_read(dsn = "Q:/GW/EC1121SAR_EEP_Ops/ATL_CWS_SAR/Critical_Habitat/Regional_Flatten.gdb")


ch <- st_transform(ch, 4326)

ch <- st_cast(ch, "MULTIPOLYGON")

ckClass <- function(x) "MULTISURFACE" %in% class(x)
mts <- unlist(lapply(ch$SHAPE, ckClass))
if(TRUE %in% mts){
  ch<- st_cast(ch, "MULTIPOLYGON")    
}

ch <- st_make_valid(ch)

#str(ch)

ch.atl <- st_intersection(ch, atl)


ch.study.site <- st_intersection(ch, cw[cw$BLOC %in% cw.int$BLOC,])


###################
##   EWS PLOTS   ##
###################

ews.nb <- st_read(dsn = "C:/Users/EnglishM/Documents/EWS/Maritimes/Shiny App/app/data/EWS_Maritimes_Plots.gdb")

ews.nb <- st_transform(ews.nb, 4326)


##################
##   Ag Plots   ##
##################

#Read in AG plots shapefile
nb.ag <- st_read("C:/Users/englishm/Documents/EWS/Agricultural Plot Survey/Ag Plot Shape Files/nb_surveyplots2008.shp")

nb.ag <- st_transform(nb.ag, crs = "+proj=longlat +datum=WGS84")


#####################
##   BBS Surveys   ##
#####################

#ns surveys
bbs.nb <- st_read("Q:/GW/EC1130MigBirds_OiseauxMig/ATL_CWS_Landbirds/Breeding Bird Survey_ATL/NB_Active BBS routes.gdb")

bbs.nb <- st_transform(bbs.nb, 4326)

bbs.nb <- st_zm(bbs.nb, drop = T, what = "ZM")



#################################
##   coastal block waterfowl   ##
#################################

#read in coastal blocks shapefile

cw <- st_read(dsn = "Q:/GW/EC1130MigBirds_OiseauxMig/ATL_CWS_Waterfowl/Coastal Block Areas.shp")

cw <- st_transform(cw, 4326)

sf_use_s2(FALSE) #turn off spherical geometry

cw <- st_make_valid(cw)

cw.int <- st_intersection(cw, study.site)

#read in coastal block data
cw.data <- st_read(dsn = "Q:/GW/EC1140WH_Con_HF/ATL_CWS_MarineAreas/Waterfowl/Coastal Survey Blocks/Coastal_survey_blocks_maxcounts/CoastalBlockMaxCount_NB.shp")

#Filter data by coastal blocks
cw.data <- filter(cw.data, BLOC %in% cw.int$BLOC)

#cw.data.17 <- filter(cw.data, BLOC == 17)


#write.csv(cw.data, "Coastal_Block_MalpequeBay.csv")


################################################
##   load in atlantic coastal bird database   ##
################################################

colonies <- read_csv(file = "C:/Users/englishm/Documents/EA/Data/Colonial_Waterbird_Colonies_2024.csv")

censuses <- read_csv(file = "C:/Users/englishm/Documents/EA/Data/Colonial_Waterbird_Census_2024.csv")

#spatial colonies, subset census based on colonies in coastal block

colonies <- set_standard_names(colonies)

colonies <- colonies %>%
  st_as_sf(coords = c("londec", "latdec"),
           na.fail = F,
           remove = F) %>%
  st_set_crs(4326)

#Filter to coastal block
colonies.cw <- st_intersection(colonies, cw[cw$BLOC %in% cw.int$BLOC,])


## Filter to study site if you need to
# colonial.birds.study.site <- st_intersection(colonial.birds, study.site)

#subset censuses based on coastal block
censuses.cw <- filter(censuses,
                      ColonyId %in% unique(colonies.cw$colonyid))

names(colonies.cw)[names(colonies.cw) == 'colonyid'] <- 'ColonyId'

#order by ColonyId, then Species_code
censuses.cw <- censuses.cw[
  with(censuses.cw, order(ColonyId, Species_code)),
]

## Create some summary columns:
## most_recent_year and max_size
censuses.cw <- censuses.cw %>%
  group_by(ColonyId, Species_code) %>%
  dplyr::mutate(most_recent_year = max(Census_Year, na.rm=T),
                max_size = max(Colony_size, na.rm=T))

## most_recent_year_count
censuses.sum <- censuses.cw %>%
  group_by(ColonyId, Species_code) %>%
  mutate(most_recent_year_count = ifelse(Census_Year == most_recent_year, Colony_size, NA)) %>%
  fill(most_recent_year_count, .direction = 'downup')

## max_count_year
censuses.sum <- censuses.sum %>%
  group_by(ColonyId, Species_code) %>%
  mutate(max_count_year = ifelse(Colony_size == max_size, Census_Year, NA)) %>%
  fill(max_count_year, .direction = 'downup')


censuses.sum <- censuses.sum[order(censuses.sum$ColonyId),]

#subset columns
censuses.sum <- select(censuses.sum,
                       CensusId,
                       ColonyId,
                       Species_code,
                       Census_Year,
                       Colony_size,
                       Colony_size_units,
                       most_recent_year,
                       most_recent_year_count,
                       max_count_year,
                       max_size)

#join both tables by ColonyId
censuses.sum <- left_join(colonies.cw, censuses.sum, by = "ColonyId")

## write CSV
#write.csv(censuses.sum, "CWS_Atlantic_Waterbird_Colonies_2025.csv", row.names = F)


############
##  ACSS  ##
############

require(readxl)

acss <- read_xlsx("Q:/GW/EC1140WH_Con_HF/ATL_CWS_MarineAreas/Shorebirds/ACSS data 1971-2022 30_10_2023.xlsx", 1)


acss <- set_standard_names(acss)

str(acss)

acss.sf <- acss %>%
  st_as_sf(coords = c("longdec", "latdec"), 
           crs = 4326, 
           agr = "constant", 
           remove = FALSE,
           na.fail = F)


acss.cw <- st_transform(cw[cw$BLOC %in% cw.int$BLOC,], "+proj=utm +zone=20 +datum=WGS84") #NB is split between zones 20 (east) and 19 (west)

acss.study.site <- st_intersection(acss.sf, study.site)

#lb.out <- st_cast(lb.out,"MULTILINESTRING")

#make the 5000m buffer
acss.cw.5000 <- st_buffer(acss.cw, dist = 5000)

acss.cw.5000 <- st_transform(acss.cw.5000, "+proj=longlat +datum=WGS84")

acss.filter <- st_intersection(acss.sf, acss.cw.5000)

#acss.filter <- filter(acss.sf, site_code %in% c("MBAR", "DABA", "DAPO"))

range(as.numeric(acss.filter$obcount), na.rm=T)

unique(acss.filter$species)

###################
##   BAGO data   ##
###################

bago.inc <- st_read(dsn = "Q:/GW/EC1130MigBirds_OiseauxMig/ATL_CWS_Waterfowl/BAGO/Data/BAGO_2017.gdb", layer = "BAGO_incidental")

bago.cws <- st_read("Q:/GW/EC1130MigBirds_OiseauxMig/ATL_CWS_Waterfowl/BAGO/Data/CWS Surveys/CWS_BAGO_Surveys_1981-2010.gdb")

bago.cws <- filter(bago.cws, Species_Code_EN == "BAGO")


#Check intersection with Coastal Block

bago.cws <- st_intersection(bago.cws, cw[cw$BLOC %in% cw.int$BLOC,])

bago.inc <- st_intersection(bago.inc, cw[cw$BLOC %in% cw.int$BLOC,])


#write.csv(bago, "Maces_Bay_BAGO_data.csv")

########################
##   Harlequin Data   ##
########################

hard <- read_xlsx("C:/Users/englishm/Documents/Harlequins/HADU_PUSA_Compilation_2024-10-04.xlsx", 2)

# #rename columns
# names(hard) <- c("survey_id", "year", "month", "day", "time", "waypoint_id", "latitude", "longitude", "province", "region", "location", "notes", "HARD_M", "HARD_F", "HARD_unkn", "species", "total")


#convert to a SF object
hard.sf <- st_as_sf(hard, 
                    coords = c("longitude", "latitude"), 
                    crs = 4326, 
                    agr = "constant", 
                    remove = FALSE,
                    na.fail = F)

hard.sf <- st_intersection(hard.sf, cw[cw$BLOC %in% cw.int$BLOC,])


#write.csv(hard.sf, "Harlequin_Duck_Purple_Sandpiper_Maces_Bay.csv")


###########################
##   seaduck key sites   ##
###########################

sd <- st_read(dsn = "C:/Users/englishm/Documents/EA/seaDuckKeyHabitatSites_20220316.shp")

sd <- st_transform(sd, 4326)

sd <- filter(sd, label == "South Shore Nova Scotia")


###############################
##   Create user interface   ##
###############################

ui <- miniPage(
  
  #This supresses warning messages in the app. Needed because of how the plot is initially rendered
  tags$style(type="text/css",
             ".shiny-output-error { visibility: hidden; }",
             ".shiny-output-error:before { visibility: hidden; }"
  ),
  
  #change the app title to whatever you like
  
  gadgetTitleBar("mydata App", left = NULL, right = NULL),
  
  #change the icons to whatever you like. icons can be found here: https://fontawesome.com/icons?d=gallery
  
  miniTabstripPanel(
    miniTabPanel("ReadMe", icon = icon("file-alt"),
                 miniContentPanel(htmlOutput("text"))),
    
    miniTabPanel("Map", icon = icon("map-o"),
                 miniContentPanel(
                   editModUI("mymap", height = "100%", width = "100%"),
                   
                   #these lines control the position and aesthetics of the panel that shows the summary data
                   
                   absolutePanel(id = "controls", class = "panel panel-default", fixed = TRUE, 
                                 draggable = TRUE, top = 120, left = "auto", right = 20, bottom = "auto", 
                                 width = 380, height = "auto",
                                 style = "z-index: 1000; opacity: 0.925",
                                 
                                 h4("Summary data"),                #the title for the panel
                                 
                                 plotOutput("plot"),                #shows the plot
                                 dataTableOutput("table"),          #shows the table
                                 uiOutput("downloadDataButton"))))  #shows the download button
    
    
  ))

#############################
##   Create Shiny server   ##
#############################


#######################
##   create server   ##
#######################

server <- function(input, output, session) {
  
  
  #########################
  ##   leaflet mapping   ##
  #########################
  
  # #create colour palette
  # pal <- colorFactor(
  #   palette = c('#7f3b08','#b35806','#e08214','#fdb863','#fee0b6','#d8daeb','#b2abd2','#8073ac','#542788','#2d004b', '#FFFFFF'), # '#CCCCCC'), # , ' "#000000"
  #   domain = cpcad.sub$NAME_E)
  # 
  
  # Setup namespace for the leaflet map. 
  ns <- NS("mymap")
  
  lf <- leaflet(options = leafletOptions (minZoom = 5, maxZoom = 14)) %>%  #you can adjust your zoom range as necessary
    #addProviderTiles("Esri.OceanBasemap",group="OceanBasemap")  %>%  #we select these three basemaps but you can select the ones you like.
    addProviderTiles("Esri.WorldImagery",group="WorldImagery")  %>%
    addProviderTiles("Esri.WorldTopoMap", group = "TopoMap") %>%
    setView(-62.654, 49.373, zoom = 5) %>%                           #this sets where the map is first centered, so adjust as necessary.
    
    addLayersControl(baseGroups = c("WorldImagery","TopoMap"),
                     position = "topleft",
                     options = layersControlOptions(collapsed = FALSE)) %>%
    
    
    # addPolygons(data = ch,
    #             color = "blue",
    #             fillOpacity = 0.15,
    #             opacity = 1,
    #             weight = 1,
    #             #group = "Dataset",
    #             popup = popupTable(ch, zcol = c("Name"), row.numbers = F, feature.id = F)) %>%
    # 
    addPolygons(data = cw[cw$BLOC %in% cw.int$BLOC,],
                color = "grey",
                fillOpacity = 0.15,
                opacity = 1,
                weight = 1,
                #group = "Dataset",
                popup = popupTable(cw.int, zcol = c("BLOC", "NAME", "DESCRIPTIO"), row.numbers = F, feature.id = F)) %>%

    
  addPolygons(data = study.site,
              color = "grey",
              fillOpacity = 0.25,
              opacity = 1,
              weight = 1) %>% #,
    #group = "Dataset",
    #popup = popupTable(mb, zcol = c("Area_ha"), row.numbers = F, feature.id = F)) %>%
    
    addPolygons(data = ews.nb,
                color = "purple",
                fillOpacity = 0.15,
                opacity = 1,
                weight = 1,
                #group = "Dataset",
                popup = popupTable(ews.nb, zcol = c("desc_", "plot_id"), row.numbers = F, feature.id = F)) %>%
    
    addPolygons(data = nb.ag,
                color = "green",
                fillOpacity = 0.15,
                opacity = 1,
                weight = 1,
                #group = "Dataset",
                popup = popupTable(nb.ag, zcol = c("LABEL"), row.numbers = F, feature.id = F)) %>%
    
    
  addPolylines(data = bbs.nb,
               color = "red",
               fillOpacity = 0.15,
               opacity = 1,
               weight = 1,
               #group = "Dataset",
               popup = popupTable(bbs.nb, zcol = c("Name"), row.numbers = F, feature.id = F)) %>%
    
  addCircleMarkers(data = censuses.sum,
                   #radius = ~log(coei$Total),
                   lng = censuses.sum$londec,
                   lat = censuses.sum$latdec,
                   fillOpacity = 0.6,
                   radius = 3,
                   # fillColor = ~pal(Year), #this calls the colour palette we created above
                   color = "blue",
                   weight = 1,
                   #group = as.character(mydata.sf.m$Year),
                   popup = popupTable(censuses.sum, zcol = c("colony_name", "most_recent_year", "most_recent_year_count", "max_size"), row.numbers = F, feature.id = F)) %>%
    
    
    addDrawToolbar(targetGroup='Selected',
                   polylineOptions=FALSE,
                   markerOptions = FALSE,
                   circleOptions = FALSE,
                   circleMarkerOptions = FALSE,
                   polygonOptions = drawPolygonOptions(shapeOptions=drawShapeOptions(fillOpacity = 0.2,
                                                                                     color = 'blue',
                                                                                     weight = 3)),
                   rectangleOptions = drawRectangleOptions(shapeOptions=drawShapeOptions(fillOpacity = 0.2,
                                                                                         color = 'blue',
                                                                                         weight = 3)),
                   editOptions = editToolbarOptions(edit = FALSE, selectedPathOptions = selectedPathOptions()))
  
  # edits <- callModule(editMod, "mymap", leafmap = lf)
  
  lf
  
  
  #setup a reactive dataset that only looks at the data in the drawn polygon
  #you can make the dataframe contain whatever you like.
  
  
  selectedLocations <- reactive({
    req(edits()$finished)          #again, edits() is a reactive object, so it needs the closed parentheses
    df <- as.data.frame(st_intersection(edits()$finished, coei))
    #df <- dplyr::select(df, spp, mal, fem, unk)
    # df <- df %>%
    #   group_by(Year) %>%
    #   summarise(Total = sum(Total, na.rm = T))
    df <- as.data.frame(df)
  })
  
  
  ##########################
  ##   Create datatable   ##
  ##########################
  
  #this can be whatever kind of table you like
  
  output$table <- renderDataTable({
    datatable(as.data.frame(selectedLocations()), #make sure you use the reactive dataset
              options = list(pageLength = 5,
                             searching = F))
  })
  
  
  # Make the download button only appear when data is selected.
  output$downloadDataButton <- renderUI({
    if(!is.null(selectedLocations())) {
      downloadButton("downloadData", "Download.csv")
    }
  })
  
  # Downloadable csv of selected reactive dataset
  output$downloadData <- downloadHandler(
    filename = function() {
      paste("Shiny_App_data", Sys.Date(),".csv",sep="")  #This creates the filename. Adjust as necessary
    },
    content = function(file) {
      write.csv(selectedLocations(), file, row.names = FALSE) #this writes the reactive dataset to a .CSV file
    })
  
  
  output$text <- renderUI({
    HTML(paste("<h4> This is a Shiny application to explore marine bird use and critical habitat in Lobster Bay, NS. </h4> <hr>",
               "Data sources include:",
               "<b> Protected Areas </b>: Accessed from the CPCAD database (https://www.canada.ca/en/environment-climate-change/services/national-wildlife-areas/protected-conserved-areas-database.html",
               "<b> Wetlands </b>: Accessed from the National Topographic System (https://www.nrcan.gc.ca/earth-sciences/geography/topographic-information/maps/national-topographic-system-maps/9767",
               "<b> Roseate Tern Critical Habitat (CH) </b>: Accessed from https://open.canada.ca/data/en/dataset/0c469c0e-8afd-4c62-9420-c50af2d34f97",
               "<b> Roseate Tern Habitat Suitability Index (HSI) </b>: Three layers: 25%, 50% and 75% thresholds. Internal data submitted for publication.",
               "<b> Eastern Baccharis Critical Habitat (CH) </b>: Internal data. <b>DRAFT ONLY</b>",
               "<b> Mudflat locations </b>: Internal data from the Maritime Wetlands Inventory",
               "<b> Colonial Seabird Data </b>: Accessed from https://open.canada.ca/data/en/dataset/87bf8597-4be4-4ec2-9ee3-797f5eafbd97",
               "<b> Harlequin Duck (HADU) Observations </b>: Internal data",
               "<b> Common Edier (COEI) Observations </b>: Internal data",
               "<b> Purple Sandpiper (PUSA) Observations </b>: Internal data",
               "Click the <b> Map </b> tab at the bottom to get started.",
               sep = "<br/>"))
  })
  
}

shinyApp(ui, server)

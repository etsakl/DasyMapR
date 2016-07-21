library(knitr)
library(classInt)
library(RColorBrewer)
library(leaflet)
library(htmlwidgets)
library(GISTools)
require(rgdal)
require(sp)
require(purrr)
require(devtools)
require(shiny)
require(DasyMapR)
require(raster)
require(leaflet)




ui <- function(input, output) {
fluidPage(

      "Δασυμετρική Επιφάνεια - Διαδραστικός Χάρτης",


        leafletOutput('leafletmap')

    )


}

# SERVER ------------------------------------------------------------------

server <- function(input, output, session) {



   # input surface -----------------------------------------------------------
  if (dir.exists("Public")) {
    load("Public/dasymetric.surface.RData")

  }

  srf<-ds
  qpal <- colorNumeric("Reds", srf[['DASYCELL']], n = 5)
  srf<- spTransform(srf,CRS("+init=epsg:4326"))
  value.popup<-paste0("<strong>Value: </strong>",srf[['DASYCELL']])
    leaflet(srf) %>% addTiles() %>% addProviderTiles("Stamen.Toner") %>%
    addPolygons(stroke = FALSE ,smoothFactor = 0.2 ,color= ~qpal(srf[['DASYCELL']]),fillOpacity = .9,popup=value.popup
    ) %>%
    addLegend("bottomright", pal=qpal,values =~srf[['DASYCELL']],title = "HOU Applicants", opacity = .9)


  output$leafletmap <- renderLeaflet({
    leaflet(srf) %>% addTiles() %>% addProviderTiles("Stamen.Toner") %>%
      addPolygons(stroke = FALSE ,smoothFactor = 0.2 ,color= ~qpal(srf[['DASYCELL']]),fillOpacity = .9,popup=value.popup
      ) %>%
      addLegend("bottomright", pal=qpal,values =~srf[['DASYCELL']],title = "HOU Applicants", opacity = .9)
  })


}




options = list(shiny.maxRequestSize = 10 * 1024 ^ 2, height = 500)

shinyApp(ui = ui, server = server)

require(rgdal)
require(sp)
require(purrr)
require(devtools)
require(shiny)
require(DasyMapR)
require(raster)


ui <- function(input, output) {
  fixedPage(
    includeCSS("bootstrap.min.css"),
    # Vertical Layout ---------------------------------------------------------
    navbarPage(
      "Δασυμετρικοί Υπολογισμοί",
      # Δασυμετρικοί Υπολογισμοί -------------------------------------------------
      tabPanel(
        "Εκτέλεση Δασυμετρικών Υπολογισμών",
        p(
          "Έχετε μεταφορτωσει την επιφάνεια πηγή και την βοηθητική επιφάνεια και τώρα μποείτε να εκτελέσετε του δασυμετρικούς υπολογισμούς"
        ),
        br(),
        actionButton('dasy_con', label = "Συνέχεια", icon = icon("fa-calculator")),
        mainPanel(
          plotOutput('dasymetric.surface'),
          br(),
          tableOutput(outputId = 'DasymetricSurfaceTable')
        )
      )
    )

  )
}

# SERVER ------------------------------------------------------------------

server <- function(input, output, session) {
  # input surface -----------------------------------------------------------


  if (dir.exists("Public")){
    load("Public/source.surface.RData")
    load("Public/ancillary.surface.RData")
  }


  wd <- getwd()
  setwd("Public")
  dasymetric.surface <- eventReactive(input$dasy_con, {

    withProgress(message = "working", value = 0, {
      ds <<- do.call(
        EtrsDasymetricSurface,
        list(
          input.surface.grided = ss,
          ancillary.grided = anc,
          actuall.value = FALSE
        )
      )
    })
    save(ds, file = "source.surface.RData")

    ds
  })

 ds.raster<-reactive({etrsDasymetric2Raster(dasymetric.surface())})


  setwd(wd)

  output$dasymetric.surface <- renderPlot({
    plot(dasymetric.surface())})

  output$DasymetricSurfaceTable <-
    renderTable(head(dasymetric.surface()@data, 5))

}




options = list(shiny.maxRequestSize = 10 * 1024 ^ 2, height = 500)

shinyApp(ui = ui, server = server)

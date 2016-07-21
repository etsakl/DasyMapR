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
          "Αν έχετε μεταφορτωσει την επιφάνεια πηγή και την βοηθητική επιφάνεια και τώρα μπορείτε να εκτελέσετε του δασυμετρικούς υπολογισμούς"
        ),
        br(),
        actionButton('dasy_con', label = "Συνέχεια", icon = icon("fa-calculator")),
        mainPanel(
          plotOutput('dasymetricplot'),
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







  dasymetric.surface <- eventReactive(input$dasy_con, {

    if (dir.exists("Public")){
      load("Public/source.surface.RData")
      load("Public/ancillary.surface.RData")
    }

     withProgress(message = "working", value = 0, {
      ds <<- do.call(
        EtrsDasymetricSurface,
        list(
          input.surface.grided = ss,
          ancillary.grided = anc
                )
      )
    })
    save(ds, file = "Public/dasymetric.surface.RData")

    ds
  })



  output$dasymetricplot <- renderPlot({
    plot(dasymetric.surface())
    })

  output$DasymetricSurfaceTable <-
    renderTable(head(dasymetric.surface()@data, 5))

}




options = list(shiny.maxRequestSize = 10 * 1024 ^ 2, height = 500)

shinyApp(ui = ui, server = server)

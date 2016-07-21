require(rgdal)
require(sp)
require(purrr)
require(devtools)
require(shiny)
require(DasyMapR)

# Ancillary Surface -------------------------------------------------------


ui <- function(input, output) {
  fixedPage(
    includeCSS("bootstrap.min.css"),
            # Vertical Layout ---------------------------------------------------------
            navbarPage(
              "Βoηθητική Επιφάνεια",
              # Μεταφόρτωσε Βοηθητική επιφάνεια ----------------------------------------------
              tabPanel(
                "Φόρτωση Β/κής Επιφάνειας",
                sidebarLayout(
                  sidebarPanel(
                    p(
                      "Μεταφορτώστε ένα shape file που περιέχει την βοηθητική επιφάνεια που θα χρησιμοποιηθεί για του δασυμετρικούς υπολογισμούς"
                    ),
                    br(),
                    fileInput(
                      inputId =   "shp_file_anc",
                      label = "Μεταφόρτωσε ένα SHAPE file",
                      accept = c(".shp", ".dbf", ".sbn", ".sbx", ".shx", ".prj"),
                      multiple = TRUE,
                      width = "100%"
                    ),
                    br(),
                    p("χρειάζεστε κάποια δεδομένα για την δοκιμή?"),
                    downloadLink('downloadData','Download'),
                    textOutput(outputId = "Okancillary"),
                    tags$header(
                      tags$style(
                        "#Okancillary{color: red;font-size: 20px;font-style: italic}"
                      )
                    )
                  ),
                  mainPanel(
                    textOutput(outputId = 'Titlegeo_anc'),
                    br(),
                    plotOutput(height = 300, outputId = 'ancillarySurface'),
                    textOutput(outputId = 'Titledf'),
                    br(),
                    tableOutput(outputId = 'ancillarySurfaceTable')


                  )
                )
              ),
              # Προετοιμασία Επιφάνειας -------------------------------------------------
              tabPanel(
                "Προετοιμασία Β/κής Επιφάνειας",
                sidebarLayout(
                  sidebarPanel(
                    numericInput(
                      inputId = "surface.value.col.anc",
                      label = "Αριθμός στήλης χαρ/κού",
                      value = 2
                    ),
                    selectInput(
                      inputId = "over.method.type",
                      label = "Μέθοδος Απονομής Τιμής στο κελί",
                      choices = c("MaxArea", "PropCal"),
                      selected = "PropCal",
                      multiple = FALSE,
                      width = "100%"
                    ),
                    selectInput(
                      inputId = "cell.size.anc",
                      label = "Μέγεθος κελιού",
                      choices = c(
                        "0.5",
                        "1",
                        "2.5",
                        "5",
                        "10",
                        "25",
                        "50",
                        "100",
                        "250",
                        "500",
                        "1000",
                        "2500",
                        "5000",
                        "10000",
                        "25000",
                        "50000",
                        "100000"
                      ),
                      selected = "1000",
                      multiple = FALSE

                    ),
                    selectInput(
                      inputId = "binary",
                      label = "0.5>?",
                      choices = c("FALSE", "TRUE"),
                      selected = "FALSE",
                      multiple = FALSE,
                      width = "100%"
                    ),


                    br(),
                    actionButton('surface_con_anc', label = "Συνέχεια")
                  ),
                  mainPanel(
                    plotOutput('ancillary.surface'),
                    br(),
                    tableOutput(outputId = 'sourceAncillaryTableg')
                    )
                )
              )



            ))

}


# SERVER ------------------------------------------------------------------

server <- function(input, output, session) {
  # input surface -----------------------------------------------------------

   output$downloadData<-downloadHandler(
    filename = function(){paste("anc_data","zip",sep = ".")},
    content = function(file){file.copy(system.file("extdata","anc.zip",package = "DasyMapR"),file)},
    contentType = "application/zip"
  )


   userFile <- reactive({
    validate(need(input$shp_file_anc, message = FALSE))
    input$shp_file_anc
  })

  input.surface <- reactive({
    req(input$shp_file_anc)
    if (!is.data.frame(userFile()))
      return()
    infiles <- userFile()$datapath
    dir <- unique(dirname(infiles))
    outfiles <- file.path(dir, userFile()$name)
    purrr::walk2(infiles, outfiles, ~ file.rename(.x, .y))

    ancillarySurface <-
      try(readOGR(dir, strsplit(userFile()$name[1], "\\.")[[1]][1], encoding = "UTF8"), TRUE)
    if (class(input.surface) == "try-error")
      NULL
    else
      ancillarySurface
    save(ancillarySurface, file = "Public/input.anc.surface.RData")
    ancillarySurface
  })



  observeEvent(input$shp_file_anc, {
    output$Titlegeo_anc <- renderText({
      "Βοηθητική Επιφάνεια: Γεωμετρία"
    })
  })

  output$ancillarySurface <- renderPlot(height = 300, {
    plot(input.surface())
  })

  observeEvent(input$shp_file_anc, {
    output$Titledf <-
      renderText({
        "Βοηθητική Επιφάνεια: Περιγραφικά Δεδομένα"
      })
  })

  output$ancillarySurfaceTable <-
    renderTable(head(input.surface()@data))

  observeEvent(input$shp_file_anc, {
    output$Okancillary <-
      renderText({
        "Η βοηθητική επιφάνεια φορτώθηκε. Συνεχίστε στην προετοιμασία της επιφάνειας"
      })
  })



  ancillary.surface <- eventReactive(input$surface_con_anc, {
    insrf <- input.surface()
    withProgress(message = "Working",value = .5,{
    anc <<- do.call(
      etrsAncillarySurface,
      list(
        input.surface = insrf,
        over.method.type = input$over.method.type,
        surface.value.col = input$surface.value.col.anc,
        cell.size = as.numeric(input$cell.size.anc),
        binary = as.logical(input$binary)
      )
    )})
    save(anc, file = "Public/ancillary.surface.RData")
    anc
  })


  output$ancillary.surface <- renderPlot({
    plot(ancillary.surface())
  })

  output$sourceAncillaryTableg <-
    renderTable(head(ancillary.surface()@data,5))


}




options = list(shiny.maxRequestSize = 10 * 1024 ^ 2, height = 500)

shinyApp(ui = ui, server = server)

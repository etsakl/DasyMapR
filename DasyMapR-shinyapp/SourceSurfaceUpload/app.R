require(rgdal)
require(sp)
require(purrr)
require(devtools)
require(shiny)
require(DasyMapR)

# Source Surface -------------------------------------------------------

ui <- function(input, output) {
  fixedPage(
    includeCSS("bootstrap.min.css"),

       # Vertical Layout ---------------------------------------------------------


    navbarPage(
         "Επιφάνεια Πηγή",
      # Μεταφόρτωσε επιφάνεια πηγή ----------------------------------------------
      tabPanel(
        "Φόρτωση Επιφάνειας Πηγής",
        sidebarLayout(
          sidebarPanel(
            p(
              "Μεταφορτώστε ένα shape file που περιέχει την επιφάνεια πηγή που θα χρησιμοποιηθεί για του δασυμετρικούς υπολογισμούς"
            ),
            br(),
            fileInput(
              inputId =   "shp_file",
              label = "Μεταφόρτωσε ένα SHAPE file",
              accept = c(".shp", ".dbf", ".sbn", ".sbx", ".shx", ".prj"),
              multiple = TRUE,
              width = "100%"
            ),
            br(),
            p("χρειάζεστε κάποια δεδομένα για την δοκιμή?"),
            downloadLink('downloadData','Download')
            ,
            textOutput(outputId = "Oksource"),
            tags$header(
              tags$style("#Oksource{color: red;font-size: 20px;font-style: italic}")
            )
          ),
          mainPanel(
            textOutput(outputId = 'Titlegeo'),
            br(),
            plotOutput(height = 300, outputId = 'inputSurface'),
            textOutput(outputId = 'Titledf'),
            br(),
            tableOutput(outputId = 'sourceSurfaceTable')


          )
        )
      ),
      # Προετοιμασία Επιφάνειας -------------------------------------------------
      tabPanel(
        "Προετοιμασία Επιφάνειας Πηγής",
        sidebarLayout(
          sidebarPanel(
            numericInput(
              inputId = "surface.value.col",
              label = "Αριθμός στήλης χαρ/κού",
              value = 5
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
              inputId = "cell.size",
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
            br(),
            checkboxInput("actual",label = "Αναγωγή της τιμής εισόδου στην επιφάνεια",value = TRUE),
            br(),
            actionButton('surface_con', label = "Συνέχεια")
          ),

          mainPanel(
            plotOutput('source.surface'),
            br(),
            tableOutput(outputId = 'sourceSurfaceTableg')
          )
        )
      )



    )
  )

}

# SERVER ------------------------------------------------------------------

server <- function(input, output, session) {

output$downloadData<-downloadHandler(
  filename = function(){paste("source_data","zip",sep = ".")},
  content = function(file){file.copy(system.file("extdata","source.zip",package = "DasyMapR"),file)},
  contentType = "application/zip"
)

  # input surface -----------------------------------------------------------
  userFile <- reactive({
    validate(need(input$shp_file, message = FALSE))
    input$shp_file
  })

  if(!dir.exists("Public"))
    dir.create("Public", showWarnings = TRUE, recursive = FALSE, mode = "0777")

  input.surface <- reactive({
    req(input$shp_file)
    if (!is.data.frame(userFile()))
      return()
    infiles <- userFile()$datapath
    dir <- unique(dirname(infiles))
    outfiles <- file.path(dir, userFile()$name)
    purrr::walk2(infiles, outfiles, ~ file.rename(.x, .y))

    inputSurface <-
      try(readOGR(dir, strsplit(userFile()$name[1], "\\.")[[1]][1], encoding = "UTF8"), TRUE)
    if (class(input.surface) == "try-error")
      NULL
    else
      inputSurface
    save(inputSurface, file = "Public/input.surface.RData")
    inputSurface
  })



  observeEvent(input$shp_file, {
    output$Titlegeo <- renderText({
      "Επιφάνεια Πηγή: Γεωμετρία"
    })
  })

  output$inputSurface <- renderPlot(height = 300, {
    plot(input.surface())
  })

  observeEvent(input$shp_file, {
    output$Titledf <-
      renderText({
        "Επιφάνεια Πηγή: Περιγραφικά Δεδομένα"
      })
  })

  output$sourceSurfaceTable <-
    renderTable(head(input.surface()@data))

  observeEvent(input$shp_file, {
    output$Oksource <-
      renderText({
        "Η επιφάνεια πηγή φορτώθηκε. Συνεχίστε στην προετοιμασία της επιφάνειας"
      })
  })

  # Source Surface

wd<-getwd()
setwd("Public")
  source.surface <- eventReactive(input$surface_con, {
    insrf <- input.surface()
    withProgress(message = "working", value = 0, {



       if(input$actual==TRUE) {
         cell.area<-as.numeric(input$cell.size)
         cell.area<-cell.area^2
         insrf.act <- ActuallVal2Density(insrf,as.numeric(input$surface.value.col),cell.area)
         surface.value.col<-input$surface.value.col+2
       }else{insrf.act<-insrf
       surface.value.col<-input$surface.value.col
       }

       ss <<- do.call(
          etrsSourceSurface,
        list(
          input.surface = insrf.act,
          over.method.type = input$over.method.type,
          surface.value.col = as.numeric(surface.value.col),
          cell.size = as.numeric(input$cell.size)
        )
      )
    })
    save(ss, file = "Public/source.surface.RData")
    ss
  })
setwd(wd)

  output$source.surface <- renderPlot({
    plot(source.surface())
  })

  output$sourceSurfaceTableg <-
    renderTable(head(source.surface()@data,5))

}




options = list(shiny.maxRequestSize = 10 * 1024 ^ 2, height = 500)

shinyApp(ui = ui, server = server)

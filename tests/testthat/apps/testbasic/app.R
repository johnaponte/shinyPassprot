# Test app

library(DBI)
library(pool)
library(shiny)
library(shinyBS)
library(shinyjs)
library(shinythemes)
library(shinyPassprot)
library(repana)


# Setup database ####
con <- get_pool()
salt <- config::get("salt")
if (!dbExistsTable(con, "users")) {
  create_users_table(con, salt)
}

if (!exists_user(con, "test")) {
  add_user(con, salt, "test", "test")
}

# Setup app modules ####
# UI part of the app. all ids in UI should be with ns
myappUI <- function(id) {
  ns <- NS(id)
  tagList(sidebarLayout(
    sidebarPanel(numericInput(ns("num"), "Numero", 0),
                 bsButton(ns("btn_sim"), "SIMULATE", disabled = T)),
    mainPanel(plotOutput(ns("plot1")))
  ))
}

# Server part of the app. only IDs defined are available
myapp <- function(input, output, session) {

  ns <- session$ns

  data <- reactive({
    input$btn_sim
    print("generating data\n")
    isolate(rnorm(input$num, 0, 1))
  })

  observeEvent(
    input$num,
    {
      if (input$num <= 0) {
        updateButton(session,ns("btn_sim"), disabled = T)
      }
      else {
        updateButton(session,ns("btn_sim"), disabled = F)
      }
    }
  )

  output$plot1 <- renderPlot({
    if (length(data()) == 0) return(NULL)
    hist(data())
  })

}


# Instanciate the application ####
ui <- passprotUI("Test App", "cerulean", myappUI)
server <- passprotServer(con, salt, myapp)
shinyApp(ui, server)

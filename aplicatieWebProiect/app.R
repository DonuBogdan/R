library(shiny)
library(RPostgreSQL)

# Define UI
ui <- fluidPage(
  
  # App title
  titlePanel("DM117 (The most powerful team on the earth.)"),
  
  # Sidebar layout with input and output definitions
  sidebarLayout(
    
    # Sidebar panel for inputs
    sidebarPanel(
      
      # Input: Select a dataset
      selectInput("dataset", "Choose a dataset:",
                  choices = c("alcatuire_disc", "discografie", "discuri_clasamente", "formatii",
                              "muzicieni", "realizare_disc", "tari", "vanzari_totale_tari")),
      
      # Input: Specify the number of observations to view
      numericInput("obs", "Number of observations to view:", 10),
      
      # Include clarifying text
      helpText("Select what table do you want to see, and on the right of the 
               screen will be displayed a brief summary of the variables
               and corresponding observations of the table"),
      
      # Input: actionButton() to defer the rendering of output
      # until the user explicitly clicks the button (rather than
      # doing it immediately when inputs change).
      actionButton("update", "Update View")
      
    ),
    
    # Main panel for displaying outputs
    mainPanel(
      
      # Output: Header + summary of distribution
      h4("Summary"),
      verbatimTextOutput("summary"),
      
      # Output: Header + table of distribution
      h4("Observations"),
      tableOutput("view")
    )
    
  )
)

# Define server logic to summarize and view selected dataset
server <- function(input, output) {
  
  ###########################
  ###########################
  ###### DATA BASE PART #####
  ###########################
  ###########################
  
  # Loads the PostgreSQL driver
  drv <- dbDriver("PostgreSQL")
  
  # Creates a connection to the postgres database
  con <- dbConnect(drv, dbname = "rock2019ro",
                   host = "localhost", port = 5432,
                   user = "postgres", password = "sl150793")
  
  alcatuire_disc = dbGetQuery(con, "select * from alcatuire_disc") 
  discografie = dbGetQuery(con, "select * from discografie")
  discuri_clasamente = dbGetQuery(con,  "select * from discuri_clasamente")
  formatii = dbGetQuery(con, "select * from formatii")
  muzicieni = dbGetQuery(con, "select * from muzicieni")
  realizare_disc = dbGetQuery(con, "select * from realizare_disc")
  tari = dbGetQuery(con, "select * from tari")
  vanzari_totale_tari = dbGetQuery(con, "select * from vanzari_totale_tari")
  
  # Close the connection
  dbDisconnect(con)
  
  # Return the requested dataset
  # Note that we use eventReactive() here, which depends on
  # input$update (the action button), so that the output is only
  # updated when the user clicks the button
  datasetInput <- eventReactive(input$update, {
    switch(input$dataset,
           "alcatuire_disc" = alcatuire_disc,
           "discografie" = discografie,
           "discuri_clasamente" = discuri_clasamente,
           "formatii" = formatii,
           "muzicieni" = muzicieni,
           "realizare_disc" = realizare_disc,
           "tari" = tari,
           "vanzari_totale_tari" = vanzari_totale_tari)
  }, ignoreNULL = FALSE)
  
  # Generate a summary of the dataset
  output$summary <- renderPrint({
    dataset <- datasetInput()
    summary(dataset[1:isolate(input$obs),])
  })
  
  # Show the first "n" observations
  # The use of isolate() is necessary because we don't want the table
  # to update whenever input$obs changes (only when the user clicks
  # the action button)
  output$view <- renderTable({
    head(datasetInput(), n = isolate(input$obs))
  })
  
}

shinyApp(ui, server)
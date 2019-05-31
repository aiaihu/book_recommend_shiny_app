library(shiny)

ui <- fluidPage(
  tags$head(
    tags$link(rel = "stylesheet", type = "text/css", href = "site.css")
  ),

  titlePanel("Book Recommendation"),

  sidebarLayout(
    sidebarPanel(
      h5("Please rate the books"),
      uiOutput("booksToRate")
    ),
    mainPanel(
      actionButton("showResults", "Get the recommendations", class = "btn-warning"),
      textOutput("message"),
      uiOutput("recommendations")
    )
  )
)

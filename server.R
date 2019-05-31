library(shiny)
source("recom_model.R")

get_user_ratings <- function(value_list) {
  # convert names of value_list to a vector which contains book IDs or NA
  book_id = sapply(
    strsplit(names(value_list), "_"), # rating has the name like 'rating_ 1234'
    function(x) ifelse(length(x) > 1, x[[2]], NA)
  )
  
  # convert values of value_list to a vector
  rating = unlist(as.character(value_list))
  dat <- data.table(book_id = book_id,
                    rating = rating)
  dat <- dat[!is.null(rating) & !is.na(book_id)]
  dat <- dat[, ':=' (book_id = as.numeric(book_id), rating = as.numeric(rating))]
  dat <- dat[rating > 0]
}

server <- function(input, output) {
  showResultsPressed <- reactive({
    value_list <- reactiveValuesToList(input)
    ratings <- get_user_ratings(value_list)
    if (length(ratings$rating) < 5) {
      output$message <- renderText({
        "You have to rate at least 5 books to get the recommendations!"
      })
    } else {
      output$recommendations <- renderUI({
        recommendations = getRecommendations(ratings)
        lapply(1:length(recommendations$book_id), function(i) {
          div(class = "book",
            div(class = "largeImage", img(src = books$image_url[i])),
            div(class = "info",
              div(class = "author", books$authors[i]),
              div(class = "title", strong(books$title[i]))
            )
          )
        })
      })
    }
  })

  getBooks <- reactive({
    num_books = 10
    books <- getSampleBooks(num_books)
    lapply(1:num_books, function(i) {
      div(class = "book",
          div(class = "image", img(src = books$small_image_url[i])),
          div(class = "info",
            div(class = "author", books$authors[i]),
            div(class = "title", strong(books$title[i]))
          ),
          div(class = "rating", 
              sliderInput(
                paste("rating_", books$book_id[i]), 
                "",
                min = 0, 
                max = 5, 
                value = 0
              )
          )
        )
    })
  })


  output$booksToRate <- renderUI({
    getBooks()
  })

  observeEvent(input$showResults, {
    showResultsPressed()
  })
}

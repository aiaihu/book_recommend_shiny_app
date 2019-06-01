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
    user_ratings <- get_user_ratings(value_list)
    if (length(user_ratings$rating) < 5) {
      output$message <- renderText({
        "You have to rate at least 5 books to get the recommendations!"
      })
    } else {
      recommendations = getRecommendations(user_ratings)
      if (length(recommendations$book_id) > 0) {
        output$message <- renderText({ "" })
        output$recommendations <- renderUI({
          lapply(1:length(recommendations$book_id), function(i) {
            div(class = "book",
              div(class = "largeImage", img(src = recommendations$image_url[i])),
              div(class = "info",
                div(class = "author", recommendations$authors[i]),
                div(class = "title", strong(recommendations$title[i]))
              )
            )
          })
        })
      } else {
        output$message <- renderText({ "No recommendation. Please try again." })
      }
    }
  })

  getBooks <- reactive({
    num_books = 20
    theBooks <- getSampleBooks(num_books)
    lapply(1:num_books, function(i) {
      div(class = "book",
          div(class = "image", img(src = theBooks$small_image_url[i])),
          div(class = "info",
            div(class = "author", theBooks$authors[i]),
            div(class = "title", strong(theBooks$title[i]))
          ),
          div(class = "rating", 
              sliderInput(
                paste("rating_", theBooks$book_id[i]), 
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

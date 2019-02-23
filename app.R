library(shiny)
library(nessy)
library(glue)
library(tidyverse)
library(mongolite)
library(rlang)
library(shinyhelper)
library(praise)
# library(shinyjs)
# library(V8)

## This will add the options for the MongoDB - for your own, do:
# options(mongodb = list(
#   "host" = "YOUR HOST",
#   "username" = "YOUR USERNAME",
#   "password" = "YOUR PASSWORD"
# ))

source("www/credentials.R")

## my dance ----
my_dance <- function(code) {
  code <- read_file(code)
  tibble(expr = parse_exprs(code))
}

## db setup ----
database_name <- "tidy_classification"
collection_name <- "responses"
save_data <- function(data, collection_name = "responses") {
  db <- mongo(collection = collection_name,
              url = sprintf(
                "mongodb://%s:%s@%s/%s?ssl=true&replicaSet=Cluster0-shard-0&authSource=admin&retryWrites=true",
                options()$mongodb$username,
                options()$mongodb$password,
                options()$mongodb$host,
                database_name))
  db$insert(data)
}

update_data <- function(collection_name, query, update) {
  db <- mongo(collection = collection_name,
              url = sprintf(
                "mongodb://%s:%s@%s/%s?ssl=true&replicaSet=Cluster0-shard-0&authSource=admin&retryWrites=true",
                options()$mongodb$username,
                options()$mongodb$password,
                options()$mongodb$host,
                database_name))
  db$update(query, update)
}

load_data <- function(collection_name = "responses") {
  db <- mongo(collection = collection_name,
              url = sprintf(
                "mongodb://%s:%s@%s/%s?ssl=true&replicaSet=Cluster0-shard-0&authSource=admin&retryWrites=true",
                options()$mongodb$username,
                options()$mongodb$password,
                options()$mongodb$host,
                database_name))
  data <- db$find()
  data
}

## test things ----
url <- read_lines("data/urls.txt")
## ui ----
ui <- cartridge(
  # useShinyjs(),
  # extendShinyjs(script = "www/coin.js"),
  title = "",
  "Help us classify functions from data analysis R scripts into categories! We have some .R files for you to classify, or you can upload your own.",
  ### login ----
  conditionalPanel("!input.login", 
                   container_with_title(
                     "login",
                     text_input("user", "username", inline = TRUE),
                     button_primary("login", "login"),
                     br(), br(),
                     "Input your own username, or login with the unique name given.")
  ),
  ### main panel ----
  conditionalPanel("input.login",
                   tags$style(HTML(
                     "body {max-width: 800px; margin: auto;}
    @media only screen and (max-width:800px) {
      body {
        font-size: 10px;
      }
      html {
        font-size: 10px;
      }
      h1 {
        font-size: 19px;
      }
    }
    ")),
                   container_with_title(
                     "classify that code!", 
                     textOutput("user", inline = TRUE), HTML("&nbsp;&nbsp;&nbsp;&nbsp;"),
                     tags$img(src = "lucy_coin.png"),
                     "X",
                     textOutput("coins", inline = TRUE),  HTML("&nbsp;&nbsp;&nbsp;&nbsp;"),
                     "rank:", textOutput("rank", inline = TRUE),
                     br(), br(),
                     ### code ----
                     conditionalPanel(
                       "!output.end",
                       container_simple(
                         is_dark = TRUE,
                         uiOutput("code")
                       )
                     ),
                     ### another ----
                     conditionalPanel(
                       "output.end && input.start && output.rank",
                       container_simple(
                         is_dark = TRUE,
                         "You finished the file! Want to try another?!"
                       ),
                       balloon("Thank you!", side = "right"),
                       tags$img(src = "jeff-thanks.png"),
                       checkbox("new", "I'd like to use my own .R file", width = "600px"),
                       conditionalPanel("input.new",
                                        fileInput("file2", "Choose .R File"),
                                        hr()
                       ),
                       button_success("another", "Do another!")
                     ),
                     br(),
                     ### start ----
                     conditionalPanel("!input.start",
                                      checkbox("upload", "I'd like to use my own .R file", width = "600px"),
                                      conditionalPanel("input.upload", 
                                                       fileInput("file", "Choose .R File")
                                      ),
                                      button_success("start", "Start")
                     )
                   ),
                   container_simple(
                     radio_buttons("class", "Select the Class",
                                   choices = c("setup", "import", "exploratory", "data cleaning",
                                               "modeling", "visualization", "evaluation", "communication", "export", "not sure")),
                     button_primary("click", "Next")
                   ),
                   HTML(glue("App powered by R using ", 
                             "{tags$a(href = 'http://shiny.rstudio.com', '{shiny}', target = '_blank', style = 'color: blue')} ",
                             " and ",
                             "{tags$a(href = 'https://github.com/ColinFay/nessy', '{nessy}', target = '_blank', style = 'color: blue')} ",
                             "Questions? Comments? Tweet ",
                             "{tags$a(href = 'https://twitter.com/LucyStats', '@LucyStats', target = '_blank', style = 'color: blue')}"
                   )
                   )
                   )
) 


## server ----
server <- function(input, output, session) {
  session_id <- runif(1)*1e23
  clicks <- reactiveVal(0)
  
  ### username ----
  user <- reactive({
    if (input$user == "") {
      glue("{sample(praise_parts$adjective, 1)}{sample(1:10000, 1)}")
    } else input$user
  })
  
  observe({
    updateTextInput(session, "user", placeholder = user())
  })
  
  output$user <- renderText(user())
  
  ### load data ----
  resp <- load_data()
  
  ### rank user ----
  user_data <- load_data("users")
  rank <- reactiveVal(nrow(user_data))
  n_coins <- reactiveVal(0)
  
  observeEvent(input$login, {
    if (user() %in% user_data$user) {
      n_coins({
        user_data$coins[user_data$user == user()] %>%
          as.numeric()
        })
      rank({
        user_data %>% 
          arrange(-as.numeric(coins)) %>%
          mutate(rank = 1:n()) %>%
          filter(user == user()) %>%
          pull(rank)
      })
    } else {
      user_data <- tibble(
        user = user(),
        coins = 0
      )
      save_data(user_data, "users")
      
    }
  })
  
  user_data_ <- eventReactive(input$start, {
    load_data("users")
  })
  
  observeEvent(input$click, {
    n_coins(n_coins() + 1)
    rank({
      user_data_() %>%
      mutate(coins = case_when(
        user == user() ~ as.character(n_coins()),
        TRUE ~ coins)
        ) %>%
      arrange(-as.numeric(coins)) %>%
      mutate(rank = 1:n()) %>%
      filter(user == user()) %>%
      pull(rank)
    })
    update_data("users", 
                glue('{"user": "[user()]"}',
                     .open = "[", .close = "]"),
                glue('{"$set":{"coins": "[n_coins()]"}}',
                     .open = "[", .close = "]"))
    # js$coin()
  })
  
  output$rank <- renderText(rank())
  
  ### read in .R file ----
  codes_ <- reactiveVal({
    
    done <- FALSE
    while(done == FALSE) {
      tryCatch({
        u <- sample(url, 1)
        m <- my_dance(url(u))
        done <- TRUE
      }, error = function(e){
      })
    }
    m$expr
  })
  
  my_file <- reactiveVal(FALSE)
  
  observeEvent(input$file2, {
    m <- my_dance(input$file2$datapath)
    codes_(m$expr)
    my_file(TRUE)
  })
  
  observeEvent(input$file, {
    m <- my_dance(input$file$datapath)
    codes_(m$expr)
  })
  
  observeEvent(input$another, {
    if (my_file() == TRUE) {
      return()
    } 
    done <- FALSE
    while(done == FALSE) {
      tryCatch({
        u <- sample(url, 1)
        m <- my_dance(url(u))
        done <- TRUE
      }, error = function(e){
      })
    }
    codes_(m$expr)
  })
  
  #### code display ----
  code <- reactiveVal({
    "Code will appear in this box. Click on the classifier below that best fits the code, then click the NEXT button. Press START to begin."
  })
  
  output$code <- renderUI({
    tagList(tags$span(style="color:white", HTML(code())))
  })
  
  ### end ----
  output$end <- reactive({clicks() == 0 & input$click})
  outputOptions(output, "end", suspendWhenHidden = FALSE)
  
  observeEvent({
    input$start | input$click | input$another 
  }, {
    if (input$start == 0){
      return()
    }
    clicks({
      if (clicks() == length(codes_())) {
        0
      } else clicks() + 1
    })
    
    if (clicks() > 0) {
      code(codes_()[[clicks()]] %>%
             deparse())
    }
  })
  
  observeEvent(input$click,  save_data(data()))
  
  ### update data with user input ----
  data <- reactive({
    tibble(code = paste(code(), collapse = " "),
           class = input$class,
           user = user(),
           session = session_id) 
  })
  
  ### coinz ----
  output$coins <- renderText(n_coins())
}

## RUN APP! ----
shiny::shinyApp(ui, server)

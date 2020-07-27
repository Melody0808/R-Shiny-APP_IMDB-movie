library(shiny)
library(shinydashboard)
library(DT)
library(shinyjs)
library(sodium)
library(DBI)
library(odbc)
library(shinythemes)
library(shinyWidgets)
library(readr)
library(ggplot2)
library(tm)
library(NLP)
library(wordcloud)
library(RColorBrewer)
library(dplyr)
library(tidyverse)

source("./credentials_gp5.R")
unique_genres <- read_csv("genres-unique.csv")
# Main login screen
loginpage <- div(id = "loginpage", style = "width: 500px; max-width: 100%; margin: 0 auto; padding: 20px;",
                 wellPanel(
                     tags$h2("LOG IN", class = "text-center", style = "padding-top: 0;color:#333; font-weight:600;"),
                     textInput("userName", placeholder="Username", label = tagList(icon("user"), "Username")),
                     passwordInput("passwd", placeholder="Password", label = tagList(icon("unlock-alt"), "Password")),
                     br(),
                     div(
                         style = "text-align: center;",
                         actionButton("login", "SIGN IN", style = "color: orange; background-color:#222d32;
                                 padding: 10px 15px; width: 150px; cursor: pointer;
                                 font-size: 18px; font-weight: 600;"),
                         shinyjs::hidden(
                             div(id = "nomatch",
                                 tags$p("Oops! Incorrect username or password!",
                                        style = "color: red; font-weight: 600; 
                                            padding-top: 5px;font-size:16px;", 
                                        class = "text-center"))),
                         br(),
                         br(),
                         
                     )),
                 img(src = "https://imagizer.imageshack.com/img922/3667/i6tu0x.png",
                     height = 180, width = 470)
                 
)
# add the user id and password, asign them into advanced or basic
credentials = data.frame(
    username_id = c("admin", "user"),
    passod   = sapply(c("admin", "user"),password_store),
    permission  = c("advanced", "basic"), 
    stringsAsFactors = F
)

header <- dashboardHeader( title = "FilmHub", uiOutput("logoutbtn"))
sidebar <- dashboardSidebar(uiOutput("sidebarpanel")) 
body <- dashboardBody(shinyjs::useShinyjs(), uiOutput("body"))
ui<-dashboardPage(header, sidebar, body, skin = "yellow")

server <- function(input, output, session) {
    
    login = FALSE
    USER <- reactiveValues(login = login)
    
    observe({ 
        if (USER$login == FALSE) {
            if (!is.null(input$login)) {
                if (input$login > 0) {
                    Username <- isolate(input$userName)
                    Password <- isolate(input$passwd)
                    if(length(which(credentials$username_id==Username))==1) { 
                        pasmatch  <- credentials["passod"][which(credentials$username_id==Username),]
                        pasverify <- password_verify(pasmatch, Password)
                        if(pasverify) {
                            USER$login <- TRUE
                        } else {
                            shinyjs::toggle(id = "nomatch", anim = TRUE, time = 1, animType = "fade")
                            shinyjs::delay(3000, shinyjs::toggle(id = "nomatch", anim = TRUE, time = 1, animType = "fade"))
                        }
                    } else {
                        shinyjs::toggle(id = "nomatch", anim = TRUE, time = 1, animType = "fade")
                        shinyjs::delay(3000, shinyjs::toggle(id = "nomatch", anim = TRUE, time = 1, animType = "fade"))
                    }
                } 
            }
        }    
    })
    
    output$logoutbtn <- renderUI({
        req(USER$login)
        tags$li(a(icon("fa fa-sign-out"), "Logout", 
                  href="javascript:window.location.reload(true)"),
                class = "dropdown", 
                style = "background-color: #FFA500 !important; border: 0;
                    font-weight: bold; margin:5px; padding: 10px;")
    })
    
    #do not edit the code before this line!! please!! 
    #add different menuItems to advanced and basic users
    output$sidebarpanel <- renderUI({
        if (USER$login == TRUE ){
            if (credentials[,"permission"][which(credentials$username_id==input$userName)]=="advanced") {
                sidebarMenu(
                    menuItem("New Movies", tabName = "tab1", icon = icon("dashboard")),
                    menuItem("Advanced Search", tabName = "tab2", icon = icon("dashboard")),
                    menuItem("Update", tabName = "tab3", icon = icon("dashboard")),
                    menuItem("Delete", tabName = "tab4", icon = icon("dashboard")),
                    menuItem("Insert", tabName = "tab5", icon = icon("dashboard")),
                    menuItem("Analytics", tabName = "tab6", icon = icon("dashboard"))
                    
                )
            } else {
                sidebarMenu(
                    menuItem("New Movies", tabName = "tab1", icon = icon("dashboard")),
                    menuItem("Advanced Search", tabName = "tab2", icon = icon("dashboard")),
                    menuItem("Reviews", tabName = "tab7", icon = icon("dashboard")),
                    menuItem("Analytics", tabName = "tab6", icon = icon("dashboard"))
                )
            }
            
        }
    })
    
    output$body <- renderUI({
        if (USER$login == TRUE ) {
            # advanced users tab Items
            if (credentials[,"permission"][which(credentials$username_id==input$userName)]=="advanced") {
                tabItems(
                    tabItem(tabName ="tab1",
                            box(width = 4, offset = 2,
                                background = "yellow",
                                tags$img(src = "https://imagizer.imageshack.com/img922/8506/yRk3av.jpg", 
                                         height = 315 , width = 350)),
                            box(width = 7, offset = 7,
                                background = "yellow",
                                HTML('<iframe width="670" height="315" src="https://www.youtube.com/embed/zAGVQLHvwOY" frameborder="0" allow="accelerometer; autoplay; encrypted-media; gyroscope; picture-in-picture" allowfullscreen></iframe>')),
                            box(width = 4, offset = 2,
                                background = "yellow",
                                tags$img(src = "https://imagizer.imageshack.com/img921/9322/tYMF3i.jpg", 
                                         height = 315 , width = 350)),
                            box(width = 7, offset = 7,
                                background = "yellow",
                                HTML('<iframe width="670" height="315" src="https://www.youtube.com/embed/TcMBFSGVi1c" frameborder="0" allow="accelerometer; autoplay; encrypted-media; gyroscope; picture-in-picture" allowfullscreen></iframe>')),
                            setBackgroundImage(src = "https://imagizer.imageshack.com/img922/9472/Ve4rME.jpg",
                                               shinydashboard = TRUE)
                            
                    ),
                    tabItem(tabName ="tab2", class = "active",
                            tabsetPanel(
                                tabPanel("Movie Search",
                                         fluidRow(
                                             selectInput("genre", "Choose a genre:",
                                                         c("All",
                                                           unique(as.character(unique_genres$Genres)))),
                                             #label = "Select Search Criteria",
                                             #label = shiny::HTML("<p> <span style='color: orange'>Select Search Criteria</span></p >")),
                                             
                                             textInput("title", label = h3("Title"), value = "Enter title:"),
                                             sliderInput("year", "Year Range:",
                                                         min = 1950, max = 2020,
                                                         value = c(1950,2020)),
                                             sliderInput("rating", "Rating Range:",
                                                         min = 0, max = 10,
                                                         value = c(0,10)),
                                             actionButton("Goa", "Get results"),
                                             box(offset = 5,
                                                 title = "This is your search result", 
                                                 width = 12,
                                                 tags$head(tags$style('h3 {color:orange;}')),
                                                 DT::dataTableOutput("searchTable")))
                                ),
                                tabPanel("Actor Search",
                                         textInput("movie", label = h3("MovieName:")),
                                         textInput("actor", label = h3("ActorName:")),
                                         actionButton("get", "Search Actors"),
                                         box(
                                             title = "This is your search result",
                                             tags$head(tags$style('h3 {color:orange;}')),
                                             DT::dataTableOutput("actorTable")
                                         )
                                         
                                )
                            )
                            
                            
                    ),
                    tabItem(
                        tabName = "tab3",
                        h2("Update People's Death Year", align = "center",
                           style = "font-family: 'times'; font-si16pt; color:Orange"),
                        textInput("People", label = h3("The person to update"), 
                                  value = "Enter people's name"),
                        textInput("deathyear", label = h3("Enter Death Year:"), 
                                  value = "Enter Death Year:"),
                        actionButton("update", "Update"),
                        box(width = 10,
                            tags$head(tags$style('h3 {color:orange;}')),
                            DT::dataTableOutput("updateTable")),
                        
                        
                    ),
                    tabItem(
                        tabName = "tab4",
                        h2("Delete Movies", align = "center",
                           style = "font-family: 'times'; font-si16pt; color:orange"),
                        actionButton("pre_delete", "Search Movie"),
                        textInput("movies_to_delete", label = h3("The movie to be deleted"), 
                                  value = "enter movie's title"),
                        
                        actionButton("delete", "Delete"),
                        box(width = 10,
                            tags$head(tags$style('h3 {color:orange;}')),
                            DT::dataTableOutput("movietable")
                        ),
                        
                    ),
                    tabItem(
                        tabName = "tab5",
                        h2("Insert New Movies", align = "center",
                           style = "font-family: 'times'; font-si16pt; color:orange"),
                        textInput("mid", label = h3("Movie ID:"), 
                                  value = "ttXXXXXXX"),
                        textInput("mtype", label = h3("Title Type:"), 
                                  value = ""),
                        textInput("mptitle", label = h3("Primary Title:"), 
                                  value = ""),
                        textInput("motitle", label = h3("Original Title:"), 
                                  value = ""),
                        textInput("madult", label = h3("isAdult:"), 
                                  value = "0 or 1"),
                        textInput("msyear", label = h3("Start Year:"), 
                                  value = "XXXX"),
                        textInput("meyear", label = h3("End Year:"), 
                                  value = "XXXX"),
                        textInput("mtime", label = h3("Run time in minutes:"), 
                                  value = ""),
                        actionButton("insert", "Insert"),
                        
                        DT::dataTableOutput("inserttable")
                    ),
                    tabItem(tabName = "tab6",
                            
                            tabsetPanel(
                                type = "tabs",
                                tabPanel("Ratings Scatter plot",
                                         box(width = 8, offset = 2,
                                             background = "orange",
                                             plotOutput("scatterplot")
                                         )    
                                ),
                                tabPanel("Genre Count in different Year",
                                         box(width = 8, offset = 2,
                                             background = "orange",
                                             plotOutput("linechart")
                                         )
                                )
                            )
                    )
                    
                )
                
              # basic users tab Items  
            } else {
                tabItems(
                    tabItem(tabName ="tab1",
                            box(width = 4, offset = 2,
                                background = "yellow",
                                tags$img(src = "https://imagizer.imageshack.com/img922/8506/yRk3av.jpg", 
                                         height = 315 , width = 350)),
                            box(width = 7, offset = 7,
                                background = "yellow",
                                HTML('<iframe width="670" height="315" src="https://www.youtube.com/embed/zAGVQLHvwOY" frameborder="0" allow="accelerometer; autoplay; encrypted-media; gyroscope; picture-in-picture" allowfullscreen></iframe>')),
                            box(width = 4, offset = 2,
                                background = "yellow",
                                tags$img(src = "https://imagizer.imageshack.com/img921/9322/tYMF3i.jpg", 
                                         height = 315 , width = 350)),
                            box(width = 7, offset = 7,
                                background = "yellow",
                                HTML('<iframe width="670" height="315" src="https://www.youtube.com/embed/TcMBFSGVi1c" frameborder="0" allow="accelerometer; autoplay; encrypted-media; gyroscope; picture-in-picture" allowfullscreen></iframe>')),
                            setBackgroundImage(src = "https://imagizer.imageshack.com/img922/9472/Ve4rME.jpg",
                                               shinydashboard = TRUE)
                            
                    ),
                    tabItem(tabName ="tab2", class = "active",
                            tabsetPanel(
                                tabPanel("Movie Search",
                                         fluidRow(
                                             column(width = 4,
                                                    selectInput("genre", "Choose a genre:",
                                                                c("All",
                                                                  unique(as.character(unique_genres$Genres))))),
                                             
                                             
                                             
                                             textInput("title", label = h3("Title"), value = "Enter text..."),
                                             box(width = 6,
                                                 sliderInput("year", "Year Range:",
                                                             min = 1950, max = 2020,
                                                             value = c(1950,2020)),
                                                 tags$head(tags$style('h3 {color:orange;}'))),
                                             box(width = 6, offset = 6,
                                                 sliderInput("rating", "Rating Range:",
                                                             min = 0, max = 10,
                                                             value = c(0,10)),
                                                 tags$head(tags$style('h3 {color:orange;}'))),
                                             actionButton("Goa", "Get results"),
                                             box(offset = 5,
                                                 title = "This is your search result", 
                                                 width = 12,
                                                 tags$head(tags$style('h3 {color:orange;}')),
                                                 DT::dataTableOutput("searchTable")))
                                ),
                                tabPanel("Actor Search",
                                         textInput("movie", label = h3("MovieName:")),
                                         textInput("actor", label = h3("ActorName:")),
                                         actionButton("get", "Search Actors"),
                                         box(
                                             title = "This is your search result",
                                             tags$head(tags$style('h3 {color:orange;}')),
                                             DT::dataTableOutput("actorTable")
                                         )
                                         
                                )
                            )
                    ),
                    tabItem(tabName = "tab7",
                            
                            box(width = 3,
                                textInput("titlename", label = h3("Title Name:"), value = "Enter Title"),
                                tags$head(tags$style('h3 {color:orange;}'))),
                            box(width = 3, offset = 3,
                                tags$head(tags$style('h3 {color:orange;}')),
                                textInput("username", label = h3("User Name:"), value = "Enter Your Username"),),
                            textInput("review", label = h3("Review:"), value = "Enter Review"),
                            actionButton("add", "Add"),
                            box(width = 8,
                                tags$head(tags$style('h3 {color:orange;}')),
                                DT::dataTableOutput("reviewsTable")
                            )
                            
                    ),
                    tabItem(tabName = "tab6",
                            
                            tabsetPanel(
                                type = "tabs",
                                tabPanel("Ratings Scatter plot",
                                         box(width = 10, offset = 2,
                                             background = "orange",
                                             plotOutput("scatterplot")
                                         )    
                                ),
                                tabPanel("Genre Count in different Year",
                                         box(width = 10, offset = 2,
                                             background = "orange",
                                             plotOutput("linechart")
                                         )
                                )
                            )
                    )
                )
                
            }
            
        }
        else {
            loginpage
        }
    })
    
    observeEvent(
        input$Goa, {
            #browser()
            db <- dbConnector(
                server   = getOption("database_server"),
                database = getOption("database_name"),
                uid      = getOption("database_userid"),
                pwd      = getOption("database_password"),
                port     = getOption("database_port")
            )
            on.exit(dbDisconnect(db), add = TRUE)
            #browser()
            query <- paste(
                "select t.tconst as TitleID, t.primaryTitle as Title,t.startYear,g.genres,r.averageRating as Rating
              from d5.Title t join d5.TitleToGenre tg on t.tconst = tg.tconst
              join d5.Genre g on g.genre_id = tg.genre_id 
              join d5.Ratings r on r.tconst = t.tconst
              where t.primaryTitle LIKE '%",input$title,"%' 
              AND (t.startYear between ",input$year[1]," AND ",input$year[2], ")
              AND (r.averageRating between ",input$rating[1]," and ",input$rating[2], ")
              AND (g.genres like '%",input$genre,"%')
              ;", sep = ""
            )
            print(query)
            # Submit the fetch query and disconnect
            data <- dbGetQuery(db, query)
            
            
            output$searchTable = DT::renderDataTable({
                data 
            })
            
        }
    )
    
    observeEvent(
        input$get, {
            #browser()
            db <- dbConnector(
                server   = getOption("database_server"),
                database = getOption("database_name"),
                uid      = getOption("database_userid"),
                pwd      = getOption("database_password"),
                port     = getOption("database_port")
            )
            on.exit(dbDisconnect(db), add = TRUE)
            #browser()
            query <- paste(
                "
          select t.primarytitle, p.primaryname as people, pro.profession
          from d5.people p join d5.knownfor k on p.nconst = k.nconst
          join d5.title t on k.tconst = t.tconst 
          join d5.PeopleToPro pt on pt.nconst = p.nconst
          join d5.Profession pro on pro.proId = pt.proId
          where t.primarytitle like '%",input$movie,"%'
          and pro.profession like 'act%'
          and p.primaryname like '%",input$actor,"%'
          ", sep = ""
            )
            print(query)
            # Submit the fetch query and disconnect
            data <- dbGetQuery(db, query)
            
            
            output$actorTable = DT::renderDataTable({
                data 
            })
            
        }
    )
    
    
    observeEvent(
        input$update, {
            #browser()
            db <- dbConnector(
                server   = getOption("database_server"),
                database = getOption("database_name"),
                uid      = getOption("database_userid"),
                pwd      = getOption("database_password"),
                port     = getOption("database_port")
            )
            on.exit(dbDisconnect(db), add = TRUE)
            #browser()
            query1 <- paste(
                "UPDATE d5.People
         SET deathYear = '",input$deathyear,"'
         WHERE primaryName = '",input$People,"';", sep = ""
            )
            print(query1)
            data <- dbGetQuery(db, query1)
            query2 <- paste("select * from d5.People where primaryName = '",input$People,"';", sep = "")
            print(query2)
            # Submit the fetch query and disconnect
            data <- dbGetQuery(db, query2)
            
            
            output$updateTable = DT::renderDataTable({
                data 
            })
            
        }
    )
    observeEvent(
        input$pre_delete, {
            db <- dbConnector(
                server   = getOption("database_server"),
                database = getOption("database_name"),
                uid      = getOption("database_userid"),
                pwd      = getOption("database_password"),
                port     = getOption("database_port")
            )
            on.exit(dbDisconnect(db), add = TRUE)
            query2 <- paste("select * from d5.title")
            print(query2)
            data <- dbGetQuery(db, query2)
            
            output$movietable = DT::renderDataTable({
                data 
            })
        })
    
    observeEvent(
        input$delete, {
            #browser()
            db <- dbConnector(
                server   = getOption("database_server"),
                database = getOption("database_name"),
                uid      = getOption("database_userid"),
                pwd      = getOption("database_password"),
                port     = getOption("database_port")
            )
            on.exit(dbDisconnect(db), add = TRUE)
            #browser()
            
            query1 <- paste("DELETE d5.title WHERE primaryTitle = '",input$movies_to_delete,"';", sep = "" )
            print(query1)
            # Submit the fetch query and disconnect
            data <- dbGetQuery(db, query1)
            
            output$updateTable = DT::renderDataTable({
                data 
            })
            
        }
    )
    
    
    observeEvent(
        input$insert, {
            #browser()
            db <- dbConnector(
                server   = getOption("database_server"),
                database = getOption("database_name"),
                uid      = getOption("database_userid"),
                pwd      = getOption("database_password"),
                port     = getOption("database_port")
            )
            on.exit(dbDisconnect(db), add = TRUE)
            #browser()
            query <- paste(";", sep = "" )
            print(query)
            # Submit the fetch query and disconnect
            data <- dbGetQuery(db, query)
            
            
            output$insertTable = DT::renderDataTable({
                data 
            })
            
        }
    )
    
    observeEvent(
        input$insert, {
            #browser()
            db <- dbConnector(
                server   = getOption("database_server"),
                database = getOption("database_name"),
                uid      = getOption("database_userid"),
                pwd      = getOption("database_password"),
                port     = getOption("database_port")
            )
            on.exit(dbDisconnect(db), add = TRUE)
            #browser()
            query <- paste("INSERT INTO d5.Title (tconst, titleType, primaryTitle, originalTitle, 
                        isAdult, startYear, endYear, runtimeMinutes)
                        VALUES ('",input$mid,"','",input$mtype,"','",input$mptitle,"','",input$motitle,"',",input$madult,",'",
                           input$msyear,"','",input$meyear,"','",input$mtime,"');", sep = "" )
            print(query)
            # Submit the fetch query and disconnect
            data <- dbGetQuery(db, query)
            
            
            output$insertTable = DT::renderDataTable({
                data 
            })
            
        }
    )
    #####
    db <- dbConnector(
        server   = getOption("database_server"),
        database = getOption("database_name"),
        uid      = getOption("database_userid"),
        pwd      = getOption("database_password"),
        port     = getOption("database_port")
    )
    on.exit(dbDisconnect(db), add = TRUE)
    
    
    query1 <- paste("SELECT t.primaryTitle, r.averageRating, r.numVotes
                    FROM d5.Ratings r JOIN d5.Title t ON r.tconst = t.tconst;")
    
    ratings <- dbGetQuery(db, query1)
    
    output$scatterplot <- renderPlot({
        ggplot(ratings, aes(x = averageRating, y = numVotes, col = primaryTitle)) +
            geom_point(size = 2) +
            theme_minimal() +
            xlab("Average Rating") +
            ylab("Number of Votes") +
            ggtitle("Average Ratings vs. Number of Votes")
    })
    
    
    #####
    db <- dbConnector(
        server   = getOption("database_server"),
        database = getOption("database_name"),
        uid      = getOption("database_userid"),
        pwd      = getOption("database_password"),
        port     = getOption("database_port")
    )
    on.exit(dbDisconnect(db), add = TRUE)
    query2 <- paste("SELECT t.startYear, g.genres
               FROM d5.Title t JOIN d5.TitleToGenre tg ON t.tconst = tg.tconst
               JOIN d5.Genre g ON tg.genre_id = g.genre_id;")
    
    genrebyYear <- dbGetQuery(db, query2)
    gby <- genrebyYear %>%
        group_by(startYear) %>%
        count(genres) %>%
        spread(genres, n)
    gby[is.na(gby)] <- 0
    
    gby <- gby %>%
        select(startYear, Action) %>%
        gather(key = "variable", value = "value", -startYear) +
        xlab("Year") +
        ylab("Genre Count") +
        ggtitle("Genre Trend over Years")
    
    print(gby)
    
    
    output$linechart <- renderPlot({
        ggplot(gby, aes(x = startYear, y = value, group = 1)) + 
            ylim(0, 3) +
            geom_line(aes(color = variable))
    })
    #####
    observeEvent(
        input$add, {
            #browser()
            db <- dbConnector(
                server   = getOption("database_server"),
                database = getOption("database_name"),
                uid      = getOption("database_userid"),
                pwd      = getOption("database_password"),
                port     = getOption("database_port")
            )
            on.exit(dbDisconnect(db), add = TRUE)
            #browser()
            query <- paste("INSERT INTO d5.Review
                            VALUES ((SELECT ISNULL(MAX(reviewId) + 1, 0) FROM d5.Review), 
                            (SELECT tconst FROM d5.Title WHERE primaryTitle = '",input$titlename,"'), 
                            '",input$titlename,"', '",input$username,"', '",input$review,"');", sep = "")
            print(query)
            
            query3 <- paste("select * from d5.review")
            # Submit the fetch query and disconnect
            data1 <- dbGetQuery(db, query)
            data2 <- dbGetQuery(db, query3)
            
            
            output$reviewsTable = DT::renderDataTable({
                data2 
            })
            
        }
    )
    
}

shinyApp(ui = ui, server = server)
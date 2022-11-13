#
# This is a Shiny web application. You can run the application by clicking
# the 'Run App' button above.
#
# Find out more about building applications with Shiny here:
#
#    http://shiny.rstudio.com/
#

library(shiny)
library(googlesheets)
library(dplyr)
library(tidyr)
library(readr)
library(purrr)

set.seed(1313)

# Define UI for application that draws a histogram
ui <- fluidPage(

    # Application title
    h1("Gruppenaufteilung"),
    p("Basierend auf folgendem Google Sheet (update mit F5): "),
    tags$a("Google Sheet", href = "https://docs.google.com/spreadsheets/d/1i09dPrdBiJAYglR_D7Qy1qVP7yibQhG9EvcmlHwctKg/edit?gid=0"),
    tags$hr(), 
    numericInput("n", "Anzahl der Gruppen:", value = 3, min = 1),
    tags$hr(), 
    htmlOutput("groups"), 
    tags$hr(), 
    p("Alles Gute zum Geburtstag, Bro!"),
    p(paste("Made with ",emo::ji("sparkling_heart") ," by Robi")), # a('Robi', href = 'https://viz.netlify.com/')) ,
    HTML('<a href="https://twitter.com/RKalcik?ref_src=twsrc%5Etfw" class="twitter-follow-button" data-lang="en" data-show-count="false">Follow @RKalcik</a><script async src="https://platform.twitter.com/widgets.js" charset="utf-8"></script>')
    # tags$hr(),
    # visNetworkOutput("v", height = "1000px")
)

# Define server logic required to draw a histogram
server <- function(input, output) {
    
    party <- read_rds("party.RDS")
  
    # token <- read_rds("googlesheets_token.rds")
    # gs_auth(token)
    # party <- gs_title("30er (Responses)") %>% 
    #     gs_read("Form responses 1") %>% 
    #     janitor::clean_names()
    # 
    # party <- party %>% 
    #     select(-timestamp, -x11, name = what_is_your_name)
    
    distance <- party %>% 
        gather("key", "value", -name) %>% 
        widyr::pairwise_dist("name", "key", "value")
    
    order <- party %>% 
        gather("key", "value", -name) %>% 
        group_by(name) %>% 
        summarise(avg = mean(value, na.rm = T)) %>% 
        ungroup() %>% 
        mutate(rank = rank(avg), z = avg - mean(avg, na.rm = T))
    
    guests <- nrow(party)
    
    group_list <- reactive({
        n <- input$n
        if (!is.na(n)){
            remainder <- guests %% n
            group_size <- c(rep(guests %/%n, n-remainder), rep(guests%/%n +1, remainder))
            dist <- distance
            o <- order
            
            l <- list()
            for (i in 1:n){
                center = o[abs(o$z) == max(abs(o$z)), "name"][1,] %>% as.character()
                l[i] <-  center 
                l[[i]] <- append(l[[i]],
                                 dist[dist$item1 == center, ] %>% 
                                     arrange(distance) %>% 
                                     head(group_size[i] - 1) %>% 
                                     pull(item2)
                )
                o <- o[!o$name %in% l[[i]],]
                dist <- dist[!dist$item1  %in% l[[i]] & !dist $item2  %in% l[[i]], ]
            }
            l
        } else {
            l = list("noone")
        }
    })
    
    observe({
        l <- group_list()
        n <- input$n
        if (!is.na(n)){
            g <- paste0("Gruppe ", seq(1:n), ": ",  map(l, ~paste(.x, collapse = ", ")), collapse = "<br> ")
            output$groups <- renderText(g)    
        } else {
            output$groups <- renderText("Bitte Anzahl der Gruppen auswählen")
        }
    })
}

# Run the application 
shinyApp(ui = ui, server = server)

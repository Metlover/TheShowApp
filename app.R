#
# This is a Shiny web application. You can run the application by clicking
# the 'Run App' button above.
#
# Find out more about building applications with Shiny here:
#
#    http://shiny.rstudio.com/
#

library(shiny)
library(jsonlite)
library(dplyr)
library(DT)
library(curl)

update_vals = function(){
    urls = c(paste0(rep("https://theshownation.com/mlb20/apis/listings.json?type=mlb_cards&page=",70),c(1:70)),
             paste0(rep("https://theshownation.com/mlb20/apis/listings.json?type=stadium&page=",5),c(1:5)),
             paste0(rep("https://theshownation.com/mlb20/apis/listings.json?type=equipment&page=",10),c(1:10)),
             paste0(rep("https://theshownation.com/mlb20/apis/listings.json?type=sponsorship&page=",5),c(1:5)),
             paste0(rep("https://theshownation.com/mlb20/apis/listings.json?type=unlockable&page=",10),c(1:10)))
    
    process = function(url){
        temp = fromJSON(txt=url)
        df = temp$listings
        print(url[[1]])
        df$type = case_when(grepl('mlb_cards',url)~'mlb_cards',
                            grepl('stadium',url)~'stadium',
                            grepl('equipment',url)~'equipment',
                            grepl('sponsorship',url)~'sponsorship',
                            grepl('unlockable',url)~'unlockable',
                            TRUE~'NA')
        return(df)
    }
    
    main_df = bind_rows(lapply(urls, process))
    
    main_df = main_df %>%
        mutate(best_sell_price.taxed = best_sell_price * 0.9,
               pot_profit = best_sell_price.taxed - best_buy_price) %>%
        arrange(-pot_profit)
    
    return(main_df)
}

#show_market_tbl = update_vals()

# Define UI for application that draws a histogram
ui <- fluidPage(

    h2("MLB The Show Community Market (Refresh to Update Market)"),
    DTOutput("show_market")
)

# Define server logic required to draw a histogram
server <- function(input, output) {

    # show_market_tbl <- eventReactive(input$go, {
    #     update_vals()
    # })
    
    output$show_market = DT::renderDT(
        update_vals(),
        filter = "top",
        options = list(
            pageLength = 30
        )
    )
}

# Run the application 
shinyApp(ui = ui, server = server)

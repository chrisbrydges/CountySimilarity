library(shinydashboard)
library(shiny)
library(DT)
library(factoextra)
library(dplyr)
load("county_data.RData")

ui <- dashboardPage(skin = "green",
    dashboardHeader(title = "County Demographic Similarity Calculator", titleWidth = 500),
    dashboardSidebar(div(style = "overflow-y: scroll"),
                     selectizeInput("county", "Select county of interest:", df$NAME),
                     checkboxGroupInput("variable", "Variables to match on:", 
                                        c("Population" = "total_population",
                                          "Median Age" = "age",
                                          "Median Income" = "income",
                                          "Education (% Bachelor's degree or more)" = "education_bachelors",
                                          "Health (% uninsured)" = "health_uninsured", 
                                          "% Hispanic" = "hispanic",
                                          "Race (% White)" = "race_white",
                                          "Race (% Black)" = "race_black",
                                          "Race (% Native American)" = "race_natam",
                                          "Race (% Asian)" = "race_asian",
                                          "Race (% Hawaiian/Pacific Island)" = "race_hawai",
                                          "Race (% Other)" = "race_other",
                                          "Race (% Multi)" = "race_multi",
                                          "2020 Election Result" = "votes_DEM")),
                     actionButton("go", "Find matches")),
    dashboardBody(
        DT::dataTableOutput("similar")
    )
)

server <- function(input, output) {
    
    output$similar <- DT::renderDataTable({
        input$go
        isolate({
            # What are the selected variables
            vars_of_interest <- input$variable
            # Scale selected variables
            x <- scale(df[, vars_of_interest])
            rownames(x) <- df$NAME
            # Calculate distances for every county
            res.dist <- as.matrix(dist(x, method = "euclidean"))
            res.dist <- as.data.frame(res.dist)
            # Only keep the county of interest
            df2 <- t(res.dist[input$county, ])
            df2 <- cbind(rownames(df2), data.frame(df2, row.names = NULL))
            colnames(df2) <- c("County", "Distance")
            # Put them in rank order
            df2$Rank <- rank(df2$Distance, ties.method = "average")
            df2$Rank <- df2$Rank - 1
            # Replace the vote_DEM variable with 2020 Election Voting
            vars_of_interest <- ifelse(vars_of_interest == "votes_DEM", "VOTE", vars_of_interest)
            # Put the demographic data into a table with the distance and rank
            df2 <- merge(df2, df, by.x = "County", by.y = "NAME")
            df2 <- df2[, (colnames(df2) %in% c("County", "Rank", "Distance", vars_of_interest))]
            df2 <- df2[order(df2$Rank), ]
            
            # Round all the numeric variables to two decimal places
            Round <- function(x, k) if (is.numeric(x)) round(x, k) else x
            replace(df2, TRUE, lapply(df2, Round, 2))
            
            # Rename columns to be more readable
            lookup <- c("Population" = "total_population",
                        "Median Age" = "age",
                        "Median Income" = "income",
                        "Education (% Bachelor's degree or more)" = "education_bachelors",
                        "Health (% uninsured)" = "health_uninsured", 
                        "% Hispanic" = "hispanic",
                        "Race (% White)" = "race_white",
                        "Race (% Black)" = "race_black",
                        "Race (% Native American)" = "race_natam",
                        "Race (% Asian)" = "race_asian",
                        "Race (% Hawaiian/Pacific Island)" = "race_hawai",
                        "Race (% Other)" = "race_other",
                        "Race (% Multi)" = "race_multi",
                        "2020 Election Result" = "VOTE")
            
            df2 <- df2 %>% rename(any_of(lookup))
            
            # Create the table
            df2})
    }, rownames = FALSE, options = list(scrollX = TRUE))
}

# Run the application 
shinyApp(ui = ui, server = server)
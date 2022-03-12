library(shinydashboard)
library(shiny)
library(DT)
library(factoextra)
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
                                          "Race (% Multi)" = "race_multi")),
                     actionButton("go", "Find matches")),
    dashboardBody(
        DT::dataTableOutput("similar")
    )
)

server <- function(input, output) {
    
    output$similar <- DT::renderDataTable({
        input$go
        isolate({
            # Scale selected variables
            x    <- scale(df[, input$variable])
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
            # Put the demographic data into a table with the distance and rank
            df2 <- merge(df2, df, by.x = "County", by.y = "NAME")
            df2 <- df2[, (colnames(df2) %in% c("County", "Rank", "Distance", "total_population", "age",
                                               "income", "education_bachelors", "health_uninsured", 
                                               "hispanic", "race_white", "race_black", "race_natam", 
                                               "race_asian", "race_hawai", "race_other", "race_multi"))]
            df2 <- df2[order(df2$Rank), ]
            
            # Round all the numberic variables to two decimal places
            df2$Distance <- round(df2$Distance, 2)
            df2$education_bachelors <- round(df2$education_bachelors, 2)
            df2$health_uninsured <- round(df2$health_uninsured, 2)
            df2$hispanic <- round(df2$hispanic, 2)
            df2$race_white <- round(df2$race_white, 2)
            df2$race_black <- round(df2$race_black, 2)
            df2$race_natam <- round(df2$race_natam, 2)
            df2$race_asian <- round(df2$race_asian, 2)
            df2$race_hawai <- round(df2$race_hawai, 2)
            df2$race_other <- round(df2$race_other, 2)
            df2$race_multi <- round(df2$race_multi, 2)
            colnames(df2) <- c("County", "Difference", "Rank", "Population", "Median Age",
                               "Median Income", "Education (% Bachelor's degree or more)",
                               "Health (% uninsured)", "% Hispanic", "Race (% White)",
                               "Race (% Black)", "Race (% Native American)", "Race (% Asian)", 
                               "Race (% Hawaiian/Pacific Island)", "Race (% Other)", "Race (% Multi)")
            
            # Create the table
            df2})
    }, rownames = FALSE, options = list(scrollX = TRUE))
}

# Run the application 
shinyApp(ui = ui, server = server)
# CountySimilarity
County Demographic Similarity Calculator is an R Shiny dashboard to calculate which US counties are most similar to each other. The dashboard can be found at https://chrisbrydges.shinyapps.io/CountySimilarity/

Data for this app has been extracted from the US Census data using the official APIs and the R censusapi package. 

To use the app, either click the link above or download the app.R file and the county_data.RData file and run the app. Next, select the county of interest (i.e., the county you want to find similarities to), then select the demographic variables that you want to match on. Then, click the "Find matches" button, and the table will rank the counties based on similarity. Similarity is measured using Euclidean distance between counties, after the variables have been autoscaled.

For any bugs/suggestions for improvements, please let me know in the Issues page on here. 

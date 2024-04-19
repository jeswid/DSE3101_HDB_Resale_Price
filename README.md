# Predicting Singapore HDB Resale Prices

<p align="center">
<img src = './frontend/www/hdb_5.png'>
</p>

### Check out our interactive web app for Singapore HDB resale price prediction [here](https://brandono7.shinyapps.io/HDB-All-You-Need-To-Know/)! 

### Introduction
Home buyers frequently face challenges such as planning their budget, selecting a suitable location, and understanding the dynamics of the property market.

Our website will provide users a map of Singapore to search for their desired HDB block and view curated information about the nearest amenities to their HDB block. Buyers can select the desired characteristics of their HDB resale flat and a predicted price of the HDB resale flat will be churned out. Finally, they can view trends in prices of that HDB resale flat from 2017 - 2024 via a graph. The prediction of prices and plotting of the graph is powered by our Extreme Gradient Boosting Machine Learning Model, which we have found to perform better than our benchmark ordinary least squares regression.

Traditionally, house prices have been thought to reflect their proximity to urban centers, transportation networks, and amenities. Yet, the real connection between a property's practical benefits and its market value can be obscured by casual discussions or the perspectives of sales agents.

Our aim is to resolve these issues by providing buyers with crucial knowledge, enabling them to make informed decisions when purchasing their ideal home. This, in turn, reduces their reliance on external intermediaries like property agents.

### Guide to the GitHub Repository
To ensure our codes would work, please either clone the repository or download this as a zip file. <br>


We split our repository into two folders: <br>
The backend houses the code for the pre-processing of data, exploratory data analysis and training of the machine learning models that we considered. <br>
The frontend houses the code for the visualization and the design of our app, separated into the UI and server files. 
Before diving into our two folders, ensure that you install these packages onto your local device:
```R
install.packages(c('parsnip', 'leaflet', 'xgboost', 'htmltools', 'DT', 'plyr', 'dplyr', 'tidyr','ggplot2', 'shiny', 'shinydashboard', 'RColorBrewer', 'sf', 'plotly', 'stringr','shinyjs', 'tidyverse', 'randomForest', 'rpart', 'VIM', 'gbm', 'hdm', 'recipes'))
```
### Guide to the Backend Folder
In the backend folder, we pre-processed data [here](https://github.com/brandono7/DSE3101_HDB_Resale_Price/blob/main/backend/Data%20Pre-Processing.R), generated geospatial data [here](https://github.com/brandono7/DSE3101_HDB_Resale_Price/blob/main/backend/geospatial%20data%20functions.R), conducted exploratory data analysis [here](https://github.com/brandono7/DSE3101_HDB_Resale_Price/blob/main/backend/Exploratory%20Data%20Analysis.Rmd) and ran our machine learning models code [here](https://github.com/brandono7/DSE3101_HDB_Resale_Price/blob/main/backend/ML.Rmd). 

You may find the data that we sourced for our datasets from the raw dataset. Additionally, the processed data folder contains datasets that we generated from our data_prepocessing.R and our geospatial_data_functions.R script.

To replicate how we generated the datasets and eventually build our final machine learning model. 

You may follow the steps that we took: <br>
1) To generate the geospatial data of HDB flats and amenities, you may run the codes in [geospatial_data_functions.R](https://github.com/brandono7/DSE3101_HDB_Resale_Price/blob/main/backend/geospatial%20data%20functions.R). <br>
2) To conduct further cleaning on our HDB transactions data as well as one hot encoding on categorical variables, you may run the codes in [data_prepocessing.R](https://github.com/brandono7/DSE3101_HDB_Resale_Price/blob/main/backend/Data%20Pre-Processing.R). <br>
3) To explore the data and visualise data distributions of our variables, you may run the codes in [Exploratory_Data_Analysis.Rmd](https://github.com/brandono7/DSE3101_HDB_Resale_Price/blob/main/backend/Exploratory%20Data%20Analysis.Rmd). <br>
4) To see our benchmark ordinary least squares regression model and the machine learning models that we considered, you may run the codes in [ML.Rmd](https://github.com/brandono7/DSE3101_HDB_Resale_Price/blob/main/backend/ML.Rmd).

### Guide to the Frontend Folder
To launch our website manually, you may either run the [ui.R](https://github.com/brandono7/DSE3101_HDB_Resale_Price/blob/main/frontend/ui.R) or [server.R](https://github.com/brandono7/DSE3101_HDB_Resale_Price/blob/main/frontend/server.R) and launch the app. The [global_settings.R](https://github.com/brandono7/DSE3101_HDB_Resale_Price/blob/main/frontend/global_settings.R) contains the global variables and functions that we need for the functionality of the app. This file will be called automatically by the ui.R and server.R. If you want to explore the global_settings.R, adjusting the file path would be needed.

Authors: Brandon, Jessica, Li Xuan, Wan Ting <br>
Special Thanks to Prof Huang and Prof Denis for their guidance. <br>
Last Updated: 19 Apr 2024

## 🐨 Koala Rescue: Australia Bushfire Visualization 🇦🇺🔥
This RShiny project aims to visualize the impact of bushfires on the Australian koala population. The app consists of three main pages, which can be navigated using the top bar control. All the required packages are included in the app.R file using if require() statements.

## Demo
[![Youtube Video](http://img.youtube.com/vi/FKLT9vTf7S4/0.jpg)](https://youtu.be/FKLT9vTf7S4)

## 📁 Files
The project contains only one main file:

app.R: This file contains all the code required to run the RShiny app, including the UI, server, and required packages.

## 📚 Packages
The project uses the following R packages:

shiny: For building the interactive web app.
leaflet: For creating interactive maps.
ggplot2: For creating static plots.
dplyr: For data manipulation and transformation.
DT: For displaying data tables.
readr: For reading data from external files.

## 🖥️ How to Run
To run this RShiny app, follow these steps:

Clone this repository to your local machine.
Open the app.R file in RStudio.
Install the required packages by running `install.packages(c("shiny", "leaflet", "ggplot2", "dplyr", "DT", "readr"))in the RStudio console. 4. Run the app by clicking on the "Run App" button in RStudio, or by typingshiny::runApp()` in the console.

## 📖 Project Structure
The RShiny app consists of three main pages, which can be navigated using the top bar control:

## 🌏 Overview
This page provides an overview of the Australian bushfire situation, including general statistics and a map showing the affected areas. It also includes a summary of the impact on the koala population.

## 🔥 Bushfire Data
This page displays detailed data related to the bushfires, such as the location, size, and duration of the fires. The data can be explored using interactive tables and charts.

## 🐨 Koala Data
This page focuses on the impact of the bushfires on the koala population, presenting information on koala habitats, population estimates, and the number of koalas affected by the fires. It includes interactive maps, charts, and tables to visualize the data.

This project is a data science project that predicts car prices in Qatar and recommends the best deals when presented with car that are put for sale.

The main 3 file for submission are:
Final_73.Rmd R Markup file
Final_073.pdf Final Report
car_073.R is the R file (R Code)

However you will also need the below to run the project:
input04Apr202410pm.csv is your full data source of car listing 17,500 records collected on 04th April 2024.
pred09Apr202411am.csv is a recent data source of car listing 360 records collected on 09th April 2024.
rf01.RData is a workspace saved that is used to load Random forest model. This model was trained for over 20 hours.

System output:
01. Linear Regression Model - Full Data Export: Contains System Recommendation for car deals based on the Method 01 Linear Regression for the full dataset (Trained/tested dataset) 13560 cars.
02. Linear Regression Model - Recent Data Export: Contains System Recommendation for car deals based on the Method 01 Linear Regression for the recent car listing (Unseen data) 360 cars.
03. Random Forest Model - Full Data Export: Contains System Recommendation for car deals based on the Method 02 Random Forest for the full dataset (Trained/tested dataset) 13560 cars. Method 01 prediction is still available here for comparison.
04. Random Forest Model - Recent Data Export: Contains System Recommendation for car deals based on the Method 02 Random Forest for the recent car listing (Unseen data) 360 cars.Method 01 prediction is still available here for comparison.
_Export_Formated_Filtered_rf_check sold is a file that will be discussed and referred to in the Final PDF Report Conclusion section. It highlights that more than 25 % of cars recommended as best deals have already boon sold indicating good results.

Other files:
_archive: file is used to archive files. You don't really need it.
.Rhistory contains R History source file
harvard_final_project.Rproj is your R Project file
Data_Load.RData is a workspace saved in case you want to load the 13560 data already clean and splitted.
Train Trials.xlsx contains the different parameters tried on KNN and Random forest.


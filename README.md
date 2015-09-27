# GettingAndCleaningDataProject

Run_analysis.R is the main file for the project. It has the R function for the assignment. The function name is called run_analysis()
To execute the function, get the R script on your machine, setwd() to the folder containing the data set and execute run_analysis()
When the script executes, it creates (at the end of step 5) a text file called step5_out.txt which has the final outcome.

For testing, I created a workbook.xlsx merging all the training / test data and Y / subject files. You could use that for testing as well.
Just select any subject and Activity combination and check the average of the different variables, and compare with 
the step5_out.txt file my R script produces. You could also load the step5_out.txt in Excel to do the comparison easily.

I have also included a codebook.Rmd / codebook.html file for the Codebook explaining each step in detail and the variable names.


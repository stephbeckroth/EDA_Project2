# GCD_Project
This repository contains the files required for the Getting and Cleaning Data Project.

The file tidytable.txt contains the tidy dataset requested.  This dataset contains the means of all mean and standard deviation variables from the original data provided by 
http://archive.ics.uci.edu/ml/datasets/Human+Activity+Recognition+Using+Smartphones 
along with a variable, set, indicating whether the data is test or training data, a participant id variable, part_id, which denotes a number between 1 and 30 corresponding to a participant and an activity label variable, activitylabel, indicating one of 6 movements on which the data was recorded and subsequently averaged by as well as participant.

The run_analysis.R is an R script which creates the tidytable.txt file.
The run_analysis.Rmd is an R script which generates the run_analysis.md markdown file which was copied to CodeBook.md to create the necessary Code Book for the tidytable.txt file as well as a run_analysis.html for an alternate view.  The html file was converted to run_analysis.pdf to provide another convenient method for viewing the summaries.

To reproduce this analysis, copy run_analysis.R to your working directory and submit it.  

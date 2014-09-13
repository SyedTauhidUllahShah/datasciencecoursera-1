# Description on Cleaning and Summarizing Accelerometer Data (run_analysis.R)
The purpose of this script is to create a tidy dataset from the supplied data and create a second dataset that summarizes the mean value for each variable in the tidy dataset based on subject ID, activity, and the variable of interest.  The variables of interest are the mean and standard deviation values provided in the raw data - all other data are filtered.

To use the script, place it into a directory and set the directory to the working directory using setwd().

## Step by step description of run_analysis.R script functionality
1.  The data is downloaded from the url https://d396qusza40orc.cloudfront.net/getdata%2Fprojectfiles%2FUCI%20HAR%20Dataset.zip as a .zip file into the ./data directory.  If the ./data firectory does not exist, it is created.  If the .zip file is already downloaded into the ./data directory, it will not be downloaded again.

2.  The .zip file is unzipped in the ./data directory, creating the "UCI HAR Dataset" subdirectory.  The data are split into two categories: test and train, extracted into the directories "DataRoot/test/" and "DataRoot/train/", respectively, where DataRoot = "./data/UCI HAR Dataset/".

3.  The data files for test and train datasets are read in using read.table().  Each set contains three files: subject_\<dataset\>.txt, X_\<dataset\>.txt, and y_\<dataset\>.txt, where \<dataset\> is test or train.  Note:  "X" is capitalized while "y" is not.

4.  In the DataRoot directory, the files "activity_labels.txt" and "features.txt" are read in using read.table() to generate the dataframes ActivityLabels and FeatureNames, respectively.  These dataframes will be used to replace the activity numbers with activity labels and column names with feature names in the tidy dataset.

5.  The column names in the xTestData and xTrainData dataframes, created in step 4, are replaced using the FeatureNames dataframe.

6.  The activity numbers in the yTestData and yTrainData dataframes, created in step 4, are merged with the ActivityLabels dataframe to create an index relating activity numbers with activity labels.

7.  Two dataframes, TrainData and TestData, are formed by column binding the Subject\<dataset\>Data, y\<dataset\>Data, and x\<dataset\>Data dataframes, in that order.  Then, the activity number column is removed, subsequently renaming the the ActivityLabel column to "Activity".

8.  The TrainData and TestData dataframes are merged together using all columns to create the CompleteData dataframe.  All columns are used since the column names are identical in the TrainData and TestData dataframes.

9.  The first column of the CompleteData dataframe, named SubjectID, is converted to a factor column.

10.  The columns that contain the mean and standard deviation are selected to remain, forming the FirstDataSet dataframe.  The names are cleaned to remove "()", "-", and rename "mean" to Mean" and "std" to "Std".  This forms our first tidy dataset.

11.  The FirstDataSet is written as a CSV file to "firstdataset.txt" in the current working directory.

12.  ID columns and variable columns are selected, stored in the IDCols and VarCols, vectors, respectively, from which the FirstDataSet is melted.  The ID columns are the SubjectID and Activity columns, and the variable columns are the rest.

13.  The ddply function is used to compute the mean value for each subjectID, Activity and variable.  These results are stored in the SummaryData dataframe.

14.  The SummaryData dataset is written as a CSV file to "MeanSummaryDataSet.txt" in the current working directory. 

## Required R libraries
The script requires the following packages: RCurl, plyr, reshape2.  Please refer to R help for package installation.


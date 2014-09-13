# Code Book

The project has two files containing the data sets: firstdataset.txt (10300 rows (including one header row) x 81 columns) and MeanSummaryDataSet.txt (3161 rows (including one header row) x 4 columns).

## Variables for MeanSummaryDataSet.txt

The column names in the MeanSummaryDataSet.txt file are "SubjectID", "Activity", "variable", and "mean", respectively, where 
  - "SubjectID" is a factor variable enumerating each participant subject, ranges from 1 to 30 inclusive.
  - "Activity" is a factor variable enumerating the six different activities in the study: WALKING, WALKING_UPSTAIRS, WALKING_DOWNSTAIRS, SITTING, STANDING, AND LAYING.
  - "variable" is a factor variable that contains the 79 different measurements of interest.  These entries form the majority of the column names of the firstdataset.txt dataset, and their descriptions will be explained in the next section.
  - "mean" is the mean computed for each variable, categorized by "SubjectID" and "Activity".
  
## Variables for firstdataset.txt

The first two columns in the firstdataset.txt file are the same "SubjectID" and "Activity" features as described above.  Columns 3:81 are the numeric features that have the column names enumerated in the "variable" feature of the MeanSummaryDataSet.txt dataset.

The feature names of columns 3:81 are of the following form:

\<Field1\>\<Field2\>\<Field3\>\<Field4\>\<Field5\>,
where:
  - \<Field1\> = one of \<t,f\>:
    * "t" indicates time domain, and "f" indicates frequency domain.
  - \<Field2\> = one of \<Body,Gravity\>:
    * "Body" indicates due body movement, and "Gravity" indicates due to gravity.
  - \<Field3\> = one of \<Acc,AccJerk,Gyro,GyroJerk,AccMag,AccJerkMag,GyroMag,GyroJerkMag,BodyAccJerkMag,BodyGyroMag,BodyGyroJerkMag\>:
    * "Acc" indicates linear acceleration, "AccJerk" indicates the linear acceleration jerk, "Gyro" indicates gyroscopic (angular) acceleration, "GyroJerk" indicates gyroscopic acceleration jerk, "AccMag" indicates linear acceleration magnitude, "AccJerkMag" indicates linear acceleration jerk magnitude, "GyroMag" indicates gyroscopic acceleration magnitude, "GyroJerkMag" indicates gyroscopic acceleration jerk magnitude, "BodyAccJerkMag" indicates linear body acceleration jerk magnitude, "BodyGyroMag" indicates body gyroscopic acceleration magnitude, and "BodyGyroJerkMag" indicates the body gyroscopic acceleration jerk magnitude.
  - \<Field4\> = one of \<Mean,Std,MeanFreq\>:
    * "Mean" indicates the mean, "Std" indicates the standard deviation, and "MeanFreq" indicates the mean value in the frequency domain.
  - \<Field5\> = one of \<X,Y,Z,No value\>:
    * "X" indicates measurement in the X-direction, "Y" indicates in the Y-direction, "Z" indicated measurement in the Z-direction, and No value indicates the vector magnitude (in concert with the *Mag values of Field 3. 

For example, tBodyAccJerkStdX is the standard deviation of the linear body acceleration jerk in the X-direction.

Finally, all features are normalized to range between -1 and 1.

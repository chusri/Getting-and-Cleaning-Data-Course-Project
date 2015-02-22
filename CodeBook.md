Code Book
========
Data source
-----------
This dataset is derived from the "Human Activity Recognition Using Smartphones Data Set" which was originally made avaiable here: http://archive.ics.uci.edu/ml/datasets/Human+Activity+Recognition+Using+Smartphones

Feature Selection 
-----------
I refer you to the README and features.txt files in the original dataset to learn more about the feature selection for this dataset. And there you will find the follow description:

The features selected for this database come from the accelerometer and gyroscope 3-axial raw signals tAcc-XYZ and tGyro-XYZ. These time domain signals (prefix 't' to denote time) were captured at a constant rate of 50 Hz. Then they were filtered using a median filter and a 3rd order low pass Butterworth filter with a corner frequency of 20 Hz to remove noise. Similarly, the acceleration signal was then separated into body and gravity acceleration signals (tBodyAcc-XYZ and tGravityAcc-XYZ) using another low pass Butterworth filter with a corner frequency of 0.3 Hz. 

Subsequently, the body linear acceleration and angular velocity were derived in time to obtain Jerk signals (tBodyAccJerk-XYZ and tBodyGyroJerk-XYZ). Also the magnitude of these three-dimensional signals were calculated using the Euclidean norm (tBodyAccMag, tGravityAccMag, tBodyAccJerkMag, tBodyGyroMag, tBodyGyroJerkMag). 

Finally a Fast Fourier Transform (FFT) was applied to some of these signals producing fBodyAcc-XYZ, fBodyAccJerk-XYZ, fBodyGyro-XYZ, fBodyAccJerkMag, fBodyGyroMag, fBodyGyroJerkMag. (Note the 'f' to indicate frequency domain signals). 

The reasoning behind my selection of features is that the assignment explicitly states "Extracts only the measurements on the mean and standard deviation for each measurement."
To be complete, I included all variables having to do with mean or standard deviation.

In short, for this derived dataset, these signals were used to estimate variables of the feature vector for each pattern:  
'-XYZ' is used to denote 3-axial signals in the X, Y and Z directions.

The set of variables that were estimated from these signals are: 

*  mean(): Mean value
*  std(): Standard deviation
*  mad(): Median absolute deviation 
*  max(): Largest value in array
*  min(): Smallest value in array
*  sma(): Signal magnitude area
*  energy(): Energy measure. Sum of the squares divided by the number of values. 
*  iqr(): Interquartile range 
*  entropy(): Signal entropy
*  arCoeff(): Autoregression coefficients with Burg order equal to 4
*  correlation(): Correlation coefficient between two signals
*  maxInds(): Index of the frequency component with largest magnitude
*  meanFreq(): Weighted average of the frequency components to obtain a mean frequency
*  skewness(): Skewness of the frequency domain signal 
*  kurtosis(): Kurtosis of the frequency domain signal 
*  bandsEnergy(): Energy of a frequency interval within the 64 bins of the FFT
   of each window.
*  angle(): Angle between some vectors.

No unit of measures is reported as all features were normalized and bounded
within [-1,1].

Data transformation
-------------------

The raw data sets are processed with run_analisys.R script to create a tidy data
set.

### Merge training and test sets

Test and training data (X_train.txt, X_test.txt), subject ids (subject_train.txt,
subject_test.txt) and activity ids (y_train.txt, y_test.txt) are merged to obtain
a single data set. Variables are labelled with the names assigned by original
collectors (features.txt).

### Extract mean and standard deviation variables

From the merged data set is extracted and intermediate data set with only the
values of estimated mean (variables with labels that contain "mean") and standard
deviation (variables with labels that contain "std").

### Use descriptive activity names

A new column is added to intermediate data set with the activity description.
Activity id column is used to look up descriptions in activity_labels.txt.

### Label variables appropriately

Labels given from the original collectors were changed:
* to obtain valid R names without parentheses, dashes and commas
* to obtain more descriptive labels

### Create a tidy data set

From the intermediate data set is created a final tidy data set where numeric
variables are averaged for each activity and each subject.

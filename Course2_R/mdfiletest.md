---
title: "Code book"
author: "milindaj"
date: "October 23, 2014"
output: html_document
---

This code book contains details on the variables in R script(run_analysis.R), input data, steps in data transofomation

**Variables used in run_analysis.R**

*Temp variables for storing directory locations*
```
datadir
testdir
traindir
```


*Extract of activity labels from "activity_labels.txt" and features (time and frequency domain variables*
```
actLabels
features
```

*Intermediate data frames to hold extract of traning and test data*
```
trainSubject
trainLabel
trainSet
trainAll
testSubject
testLabel
testSet
testAll
dataAll
```

*Intermediate variable containing features having mean() or std()*
```
requiredVars
```

*Vector with activity label description*
```
actDescLabel
```

*This holds one of the tidy dataset requested as part of this assignment. The columns include Activity Label Description, Subject identifier and 66 feature variables containing meand and sd*
```
tidyData
```

*Intermideate melt variable*
```
tMelt
```

*one of the tidy dataset requested as part of this assignment*
```
tidyAvgData
```

*Name of the output file*
```
tidyAvgData.txt
```

**Data**

The raw data provided for this assignment contain below data under training and test sets

- Triaxial acceleration Angular velocity data. This is present in the files under "Inertial Signals" folder and not used for this assignment
- A 561-feature vector with time and frequency domain variables. This is present under X_[train/test].txt files 
- Its activity label present under y_[train/test].txt files
- An identifier of the subject who carried out the experiment. This data is present under subject_[train/test].txt files

**Data transformations** 

The data transformations needed to produce the tidy data involve below steps
1. Read activity lables from activity_labels.txt file and features list (time and frequency domain variables) from "features.txt" file
2. Read activity labels, subject data and feature data for training and test sets from the respective files
3. Column bind training data sets to create combined training dataset containing Activity label, subject identifier and 561 feature variables. Similarly create combined test data set
4. Row bind training and test data sets created in above step to create a single data set
5. Assign descriptive column names to the combined dataset using the features list read in step 1
6. Identify the feature variables which measure mean and standard daviation. This is achived by using grep() function
7. subset the combined dataset to include only mean and std variables along with Activity and Subject columns and create the tidy data set
8. To generate the tidy data set with average of variables across Activity Label and Subject, first melt the tidy data set created in above step using melt() function in reshape2 package followed by decasting it with mean() function using dcast function
9. Write out the dataset created in step above to a text file

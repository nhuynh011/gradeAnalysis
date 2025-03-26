# Grade Analysis
This project analyzes survey data on university students' mathematics grades, applying statistical methods (ANOVA, linear regression, and t-tests) to identify key factors influencing academic performance. Based off of  https://github.com/prashantva/stem_ed_fall2018 and “Factors Influencing Success in Advanced Engineering Mathematics Courses: A Case Study” by Athavale et al. (2021).

# Contents
Data:<br/>
**raws**: has all the raw survey data collected from SurveyMonkey.<br/>
**MA23(...).xlsx**: cleaned survey data to be processed using prelim.R before analysis.<br/>
**datadictionary.txt**: A description of all columns in MA23(...).xlsx files.<br/>
**datafor_r.xlsx**: 2018 student survey data from “Factors Influencing Success in Advanced Engineering Mathematics Courses: A Case Study” by Athavale et al. (2021).<br/>
**poll2023.csv**: teenager's average screen time from https://news.gallup.com/poll/512576/teens-spend-average-hours-social-media-per-day.aspx?utm_source=chartr&utm_medium=email&utm_campaign=chartr_20240617.<br/>
<br/>
Data Preparation:<br/>
**cleaning.R**: shows how files were cleaned from raws to MA23(...).xlsx files.<br/>
**prelim.R**: joins all cleaned files by mathematics courses and returns calc3.xlsx, diffeq.xlsx, and all.xlsx, which are needed for all analysis files.<br/>
<br/>
Analysis:<br/>
**anova.R**: Comparing screentime, Calculus II grade before and after COVID-19 using ANOVA. Includes violin plots, QQ plots to verify normality.<br/>
**linearModels.R**: Correlation plots and stepwise regression of grade in mathematics class and other survey results. Includes correlation plots.<br/>
**tTest.R**: t-tests of Calculus II grade, screentime, Elementary Differential Equations grade, and self-reported study hours. Includes box and whiskers plot of the 4 variables of interest.<br/>
<br/>
# Building the R file for visualizations
It is easier and faster to obtain visualizations using the graphs branch since it has all the files needed for execution. If you want to use the master branch, you will need 3 extra xlsx obtained from prelim.R<br/>
<br/>
For graphs branch:<br/>
For any analysis file (anova.R, linearModels.R, or tTest.R), make sure the path to calc3.xlsx, diffeq.xlsx, and all.xlsx is correct according to your organization. Then, run the file from top to bottom for visualizations.<br/>
<br/>
For master branch:<br/>
First open prelim.R and check the file path for reading in MA23(...).xlsx files, make sure it is the right path. Run prelim.R to get calc3.xlsx, diffeq.xlsx, and all.xlsx. These xlsx will be needed for tTest.R, linearModels.R, and anova.R. Once you have calc3.xlsx, diffeq.xlsx, and all.xlsx, then you can run any analysis file for visualizations.<br/>
<br/>
# Support
Contact mihuynh@clarkson.edu or pathaval@clarkson.edu

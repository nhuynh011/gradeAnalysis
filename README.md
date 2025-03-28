# Grade Analysis
This project analyzes survey data on university students' mathematics grades, applying statistical methods (ANOVA, linear regression, and t-tests) to identify key factors influencing academic performance. Based off of  https://github.com/prashantva/stem_ed_fall2018 and “Factors Influencing Success in Advanced Engineering Mathematics Courses: A Case Study” by Athavale et al. (2021).

# Contents
Data:<br/>
-> **raw**: has all the raw survey data collected from SurveyMonkey.<br/>
-> **MA23(...) files**: cleaned survey data to be processed using prelim.R before analysis.<br/>
-> **data**: files from survey ready for analysis. Also includes the following files:
**datafor_r.xlsx**: 2018 student survey data from “Factors Influencing Success in Advanced Engineering Mathematics Courses: A Case Study” by Athavale et al. (2021).<br/>
**poll2023.csv**: teenager's average screen time from https://news.gallup.com/poll/512576/teens-spend-average-hours-social-media-per-day.aspx?utm_source=chartr&utm_medium=email&utm_campaign=chartr_20240617.<br/>
<br/>
**datadictionary.txt**: A description of all columns in MA23(...).xlsx files.<br/>
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
For master branch:<br/>
No building needed, all analysis files have required pathing and data.<br/>
<br/>
From original survey data:<br/>
1. Start with cleaning.R to process all the files in gradeAnalysis/raw, you will need to change the file names in the path.<br/>
2. After obtaining all gradeAnalysis/MA23(...) files, run prelim.R to get files in gradeAnalysis/data required for analysis.<br/>
3. Run desired analysis file (anova.R, linearModels.R, tTest.R).
# Support
Contact mihuynh@clarkson.edu or pathaval@clarkson.edu

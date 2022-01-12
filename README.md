# College Prediction Engine - Find Out Your Chances


## Introduction
The goal of this project is to build a recommendation engine for high school students to "chance" themselves at various colleges. I scraped over 88K profiles on admitsee.com with each profile having these main features: 

Ethnicity \n
English is first language
First Generation Admit 
Legacy
Gender
Hometown State
SAT Score
GPA Weighted 
GPA Unweighted
Number of Extracurricular Activities 
Number of Sports Played 
Number of APs and SAT2s Taken

Once these inputs are passed, I filter the main dataframe by college of choice provided by the user. Then, a logistic regression is trained on the said dataframe to generate a probability of acceptance. In addition, a user can find where his SAT score and GPA rank amongst students who got admitted to the college. 

You can find the application here: https://applytocollege.shinyapps.io/collegeapp-/

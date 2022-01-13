# College Prediction Engine - Find Out Your Chances

The goal of this project is to build a recommendation engine for high school students to chance themselves at various colleges. I scraped over 88K profiles on admitsee.com with each profile having these main features: 

Ethnicity <br />
English is first language  <br />
First Generation Admit  <br />
Legacy  <br />
Gender  <br />
Hometown State  <br />
SAT Score  <br />
GPA Weighted  <br /> 
GPA Unweighted  <br />
Number of Extracurricular Activities  <br /> 
Number of Sports Played  <br /> 
Number of APs and SAT2s Taken  <br />

Once these inputs are passed, I filter the main dataframe by college of choice provided by the user. Then, a logistic regression is trained on the said dataframe to generate a probability of acceptance. In addition, a user can find where his SAT score and GPA rank amongst students who got admitted to the college. 

You can find the application here: https://applytocollege.shinyapps.io/collegeapp-/

Case Study ppt: https://docs.google.com/presentation/d/16OsPlD8V2Y5xlIQvm-_CrdrvSwamRV1BZSQ3rxBMXcM/edit?usp=sharing

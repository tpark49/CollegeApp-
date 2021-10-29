library(stringr)
library(dplyr)
library(tidyr)
library(ggplot2)

#read files
df1 <- read.csv("example_datafile.csv")
df2 <- read.csv("example_datafile1.csv")
df3 <- read.csv("example_datafile4.csv")

#concatenate total df 
df = rbind(rbind(df1, df2), df3)
df = df[!duplicated(df), ]

#extract number for GPA
df$GPA = str_replace(df$GPA, "\nGPA", "")
mode(df$GPA) = "numeric"

#make new column for GPA (weigted vs unweigthed)
df$GPA_measure = ifelse(df$GPA>4, "weighted", "unweighted")

#number of essays 
df$num_of_Essays = str_replace(df$num_of_Essays, "Essays", "")
mode(df$num_of_Essays) = "numeric"

#number of scores 
df$num_of_Scores = str_replace(df$num_of_Scores, "Scores", "")
mode(df$num_of_Scores) = "numeric"

#number of Advice 
df$num_of_Advice = str_replace(df$num_of_Advice, "Advice", "")
mode(df$num_of_Advice) = "numeric"

#number of ECs 
df$num_of_ECs = str_replace(df$num_of_ECs, "ECs", "")
mode(df$num_of_ECs) = "numeric"

#number of Sports 
df$num_of_Sports = str_replace(df$num_of_Sports, "Sports", "")
mode(df$num_of_Sports) = "numeric"

#number of Schools 
df$num_of_schools = str_replace(df$num_of_schools, "Schools", "")
mode(df$num_of_schools) = "numeric"

#SAT Score - change type to numeric & convert 2/3 of old SAT score
df$SAT = str_replace(df$SAT, "SAT", "")
mode(df$SAT) = "numeric"

df$SAT[df$SAT <40] = NA
df$SAT[df$SAT >2400] = NA
df$SAT = ifelse(df$SAT>1600, df$SAT*2/3, df$SAT)
#df$SAT[df$SAT == 222] = 2222

#ACT Score
df$ACT = str_replace(df$ACT, "ACT", "")
mode(df$ACT) = "numeric"
df$ACT[df$ACT == 0] = NA 

#pivot long by colleges for accepted
df %>%
  mutate(Accepted = str_extract_all(df$accepted_schools, "([A-Za-z\\s&'/.-]+,)|([A-Za-z\\s&'/.-]+)")) %>%
  tidyr::unnest(Accepted) -> Accepted_df

Accepted_df$Accepted = str_replace(Accepted_df$Accepted, ",", "")
Accepted_df$Accepted = trimws(Accepted_df$Accepted)



#pivot long by colleges for rejected
df %>%
  mutate(Rejected = str_extract_all(df$rejected_schools, "([A-Za-z\\s&'/.-]+,)|([A-Za-z\\s&'/.-]+)")) %>%
  tidyr::unnest(Rejected)-> Rejected_df

Rejected_df$Rejected = str_replace(Rejected_df$Rejected, ",", "")
Rejected_df$Rejected = trimws(Rejected_df$Rejected)

View(Rejected_df)




  
  

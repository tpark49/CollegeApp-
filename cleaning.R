library(stringr)
library(dplyr)
library(tidyr)
library(ggplot2)


df1 <- read.csv("example_datafile.csv")
df2 <- read.csv("example_datafile1.csv")
df3 <- read.csv("example_datafile4.csv")

df = rbind(rbind(df1, df2), df3)
df = df[!duplicated(df), ]

###CLEANING###

#extract number for GPA
df$GPA = str_replace(df$GPA, "\nGPA", "")
mode(df$GPA) = "numeric"

#make new column for GPA (weigted vs unweigthed)
df$GPA_measure = ifelse(df$GPA>4, "weighted", "unweighted")

#pivot long by colleges
df %>%
  mutate(Accepted = str_extract_all(df$accepted_schools, "([A-Za-z\\s&'/.-]+,)|([A-Za-z\\s&'/.-]+)")) %>%
  tidyr::unnest(Accepted) -> df

df$Accepted = str_replace(df$Accepted, ",", "")
df$Accepted = trimws(df$Accepted)

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

df = filter(df, SAT >40)
df$SAT = ifelse(df$SAT>1600, df$SAT*2/3, df$SAT)
#df$SAT[df$SAT == 222] = 2222

#ACT Score
df$ACT = str_replace(df$ACT, "ACT", "")
mode(df$ACT) = "numeric"
df$ACT[df$ACT == 0] = NA






















#conduct Logistic Regression here 

#Given input is Harvard 

#filter df based on input
input_df_accepted = Accepted_df[Accepted_df$Accepted=="Harvard",]
input_df_rejected = Rejected_df[Rejected_df$Rejected=="Harvard",]

#Standardize column names to result to concat 
colnames(input_df_accepted)[which(names(input_df_accepted)=="Accepted")]  = "Result"
colnames(input_df_rejected)[which(names(input_df_rejected)=="Rejected")]  = "Result"

#turn Result column into binary 
input_df_accepted$Result = 1 
input_df_rejected$Result = 0 

#concat two dataframes
input_df = rbind(input_df_accepted, input_df_rejected)

#convert GPA measure into binary 
input_df$GPA_measure = ifelse(input_df$GPA_measure =="weighted", 1, 0)

#first gen into binary 
input_df$first_gen = ifelse(input_df$first_gen == "Yes", 1, 0)

#Admission binary - early action vs not 
input_df$Admission = ifelse(input_df$Admission %in% c("Early Action", "Early Decision"), 1, 0)

#English_FL - 0 no 1 else
input_df$English_FL = ifelse(input_df$English_FL == "No", 0, 1)

#Gender 
input_df$gender = ifelse(input_df$gender == "Male", 1,0)

#Dummify ethnicity on 4-5 categories 
input_df$ethnicity = ifelse(input_df$ethnicity %in% c("Native American"), 1, ifelse(
  input_df$ethnicity %in% c("Black / African American"), 2, ifelse(
    input_df$ethnicity %in% c("Hispanic"), 3, ifelse(
      input_df$ethnicity %in% c("Asian"), 4, ifelse(
        input_df$ethnicity %in% c("White Non-Hispanic"), 5, 0)
      )
    )
  )
)

#hometown - extract state 
input_df$hometown = str_extract(input_df$hometown, paste(state.name, collapse='|'))


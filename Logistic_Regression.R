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

View(input_df)

#Legacy 
input_df$legacy = ifelse(input_df$legacy =="No", 0, 1)

#hometown - extract state then convert to binary -  
input_df$hometown = str_extract(input_df$hometown, paste(state.name, collapse='|'))
states = c("alabama", "alaska", "arizona", "arkansas", 
           "california", "colorado", "connecticut", "delaware", "district of columbia", 
           "florida", "georgia", "hawaii", "idaho", "illinois", "indiana", 
           "iowa", "kansas", "kentucky", "louisiana", "maine", "maryland", 
           "massachusetts", "michigan", "minnesota", "mississippi", "missouri", 
           "montana", "nebraska", "nevada", "new hampshire", "new jersey", 
           "new mexico", "new york", "north carolina", "north dakota", "ohio", 
           "oklahoma", "oregon", "pennsylvania", "rhode island", "south carolina", 
           "south dakota", "tennessee", "texas", "utah", "vermont", "virginia", 
           "washington", "west virginia", "wisconsin", "wyoming")
values = seq(1:51)

input_df$hometown = tolower(input_df$hometown)

input_df$hometown = mapvalues(input_df$hometown, from =states,to = values)

mode(input_df$hometown) = "numeric"

#drop a few irrelevant columns
input_df = input_df[, !(names(input_df) %in% c("user_url", "username", "current_school", "user_major",
                                    "accepted_schools", "waitlisted_schools",
                                    "rejected_schools", "rank", "num_of_Essays",
                                    "num_of_Advice", "num_of_schools", "ACT", "class_of"))]

#train the data on logistic classifier
#split data for train/test 
sample <- sample.int(n = nrow(input_df), size = floor(.75*nrow(input_df)), replace = F)


train = input_df[sample,]
test = input_df[-sample,]

#sum(is.na(input_df$hometown))

#fit the model 
model = glm(Result ~ ., family = binomial(link='logit'), data=train)
summary(model)

#predict given test data set 
# value = c(1,2,3,4,5,6,7,8,9,10,11,12,13,14)
# input = data.frame(
#   "hometown" =1,
#   "gender" = 0 
# )
# input
# 
# 
# model %>%
#   predict(input, type="response")
# model %>%
#   predict(test, type="response") ->prediction 
# 
# prediction = ifelse(prediction>0.5, 1, 0)
# result = prediction==test$Result
# sum((!is.na(result) & result == TRUE))/sum((!is.na(result)))

                                           
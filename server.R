
server <- function(input, output, session){

  observeEvent(input$ActionButton, {
    
    updateTabItems(session=session,inputId = "sidebar", selected="Result" )
  })


  
  #Normal distribution for SAT
  output$SAT_ND <- renderPlot({
    
    SAT_mean = mean(Accepted_df[Accepted_df$Accepted==input$School,]$SAT, na.rm = T)
    Accepted_df %>%
      filter(Accepted == input$School) %>%
      ggplot(aes(SAT)) + 
      geom_histogram(aes(y=..density..),
                     outliers= FALSE,
                     breaks = seq(500, 2000, by = 20),
                     color="grey",
                     fill="white") +
      geom_density(alpha=0.3,
                   fill ="#FF6666") +
      scale_x_continuous(breaks=seq(500, 1800, 100), limits=c(1000, 1700))+
      geom_vline(aes(xintercept=SAT_mean),
                 col="black", size=1, linetype="dashed")+ 
      annotate(x=SAT_mean, y=0, label="mean", geom="label") + 
      geom_vline(aes(xintercept=input$SAT), 
                 col="blue", size=1, linetype="dashed") + 
      annotate(x=input$SAT, y=0.007, label="Your Score", geom="label", 
               color="blue")
  }) 
  
  
  #boxplot for SAT
  output$SAT_BOX <- renderPlot({
    
    Accepted_df %>%
      filter(Accepted == input$School) %>%
      ggplot(aes(y=SAT)) +
      geom_boxplot(outliers=FALSE) +
      xlim(-1,1) +
      #ylim(1200, 1650) + 
      geom_hline(yintercept = input$SAT, linetype="dashed",
                 size=1, color="blue") +
      annotate(x=-0.7, y=input$SAT, label="Your Score",
               color="blue", geom="label")

    })
  
  
  #Normal Distribution for GPA
  output$GPA_ND <- renderPlot({

    
    GPA_mean = mean(Accepted_df[Accepted_df$Accepted==input$School&Accepted_df$GPA_measure==tolower(input$GPA_Scale),]$GPA, na.rm = T)
    
    Accepted_df %>%
      filter(Accepted == input$School & GPA_measure==tolower(input$GPA_Scale)) %>%
      ggplot(aes(GPA)) +
      geom_histogram(aes(y=..density..),
                     outliers = FALSE, 
                     bins = 20,
                     #breaks = seq(500, 2000, by = 20),
                     color="grey",
                     fill="white") + 
      geom_density(alpha=0.3,
                   fill ="#FF6666") +
      #scale_x_continuous(breaks=seq(3, 5, 0.5), limits=c(3.5, 4.5))+
      geom_vline(aes(xintercept=GPA_mean),
                 col="black", size=1, linetype="dashed")+
      annotate(x=GPA_mean, y=0, label="mean", geom="label") +
      geom_vline(aes(xintercept=input$GPA),
                 col="blue" ,size=1, linetype="dashed") +
      annotate(x=input$GPA, y=5, label="Your GPA",
               color="blue", geom="label")

  })
  
  #boxplot for GPA
  output$GPA_BOX <- renderPlot({
    Accepted_df %>%
      filter(Accepted == input$School & GPA_measure==tolower(input$GPA_Scale)) %>%
      ggplot(aes(y=GPA)) +
      geom_boxplot(outliers=FALSE) +
      xlim(-1,1) +
      geom_hline(yintercept = input$GPA, linetype="dashed",
                 size=1, color="blue") +
      annotate(x=-0.7, y=input$GPA, label="Your GPA",
               color="blue", geom="label")
    })
  
  
  #Logisitc Regression to predict person's acceptance
  output$Model <- renderText({
    
    #filter df based on input
    input_df_accepted = Accepted_df[Accepted_df$Accepted==input$School,]
    input_df_rejected = Rejected_df[Rejected_df$Rejected==input$School,]
    
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
    
    #make dictionary for mapping user inputs 
    h = hash()
    h[states] = values
  
    input_df$hometown = tolower(input_df$hometown)
    
    input_df$hometown = mapvalues(input_df$hometown, from =states,to = values)
    
    mode(input_df$hometown) = "numeric"
    
    
    #drop a few irrelevant columns
    input_df = input_df[, !(names(input_df) %in% c("user_url", "username", "current_school", "user_major",
                                                   "accepted_schools", "waitlisted_schools",
                                                   "rejected_schools", "rank", "num_of_Essays",
                                                   "num_of_Advice", "num_of_schools", "ACT", "class_of"))]
    
    #fill na values with mean 
    input_df = na.aggregate(input_df)
    
    
    #remove outliers for SAT & GPA
    outliers_SAT <- boxplot(input_df$SAT, plot = FALSE)$out
    input_df = input_df[!(input_df$SAT %in% outliers_SAT), ]
    
    # outliers_GPA <- boxplot(input_df$GPA, plot = FALSE)$out
    # input_df = input_df[!(input_df %in% outliers_GPA), ]
    # 
    
    # #predict using model
    user_input = data.frame(
      "hometown" = h[[tolower(input$Hometown)]],
      "gender" = ifelse(input$Gender == "Male", 1, 0),
      "ethnicity" = ifelse(input$Ethnicity=="Native American", 1,
                          ifelse(input$Ethnicity=="Black / African American", 2,
                                 ifelse(input$Ethnicity=="Hispanic",3,
                                        ifelse(input$Ethnicity=="Asian", 4,
                                               ifelse(input$Ethnicity=="White Non-Hispanic",5,0))))),
      "English_FL" = ifelse(input$English_FL == "Yes", 1, 0),
      "Admission" = ifelse(input$Admission_Type == "Early", 1, 0),
      "first_gen" = ifelse(input$First_Gen == "Yes", 1, 0),
      "legacy" = ifelse(input$Legacy == "Yes", 1, 0),
      "GPA_measure" = ifelse(input$GPA_Scale == "Weighted", 1, 0),
      "GPA" = input$GPA + input$slider_1,
      "num_of_Scores" = input$Num_of_Scores,
      "num_of_ECs" = input$Num_of_ECs,
      "num_of_Sports" = input$Num_of_Sports,
      "SAT"=input$SAT + input$slider)
    
    #fit the model
    model = glm(Result ~ ., family = binomial(link='logit'), data=input_df)
    
    #predict on the model 
    model %>%
      predict(user_input, type="response") ->prediction
    
    
    
    #compute model performance based on same input of data
    sample <- sample.int(n = nrow(input_df), size = floor(.75*nrow(input_df)), replace = F)
    
    #split test and train
    train = input_df[sample,]
    test = input_df[-sample,]
    
  
    #only compute test if factor is bigger than 1
    if (length(unique(test$Result))>1) {
      #compute model performance based on same input of data
      model_test = glm(Result ~ ., family = binomial(link='logit'), data=train)
      # # 
      model_test %>%
        predict(test, type="response") ->prediction_test
      # # 
      prediction_test = ifelse(prediction_test>0.5, 1, 0)
      
      prediction_test = as.factor(prediction_test)
      test$Result = as.factor(test$Result)
      #
      model_perf = confusionMatrix(test$Result, prediction_test)
      
    }else{
      model_perf = "Not Enough Data"
    }
    
    
    HTML(
      
          paste0(
                "<div>",
                "<h1 style=
                'color:grey; 
                font-size:45px;'>","Probability of getting into ", input$School, ": ",
                "<b style='font-size:60px; color:black'>",
                round((prediction)*100), "%","</b>", "</h1>", 
                "<h1 style= color:grey;font-size:45px;'>",
                "Your SAT Score: ", "<b style='font-size:60px; color:black'>",
                input$SAT + input$slider, "</b>",
                "</h1>",
                "<h1 style= color:grey;font-size:45px;'>",
                "Your GPA: ", "<b style='font-size:60px; color:black'>",
                round(input$GPA + input$slider_1, digits =1), "</b>",
                "</h1>",
                "<br>",
                "<h1>Model Performance</h1>",
                "</div>"
              
                )
    )
    
  })
  
  #model perforamnce
  output$Model_Plot <- renderPlot({
    
    #filter df based on input
    input_df_accepted = Accepted_df[Accepted_df$Accepted==input$School,]
    input_df_rejected = Rejected_df[Rejected_df$Rejected==input$School,]
    
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
    
    #make dictionary for mapping user inputs 
    h = hash()
    h[states] = values
    
    input_df$hometown = tolower(input_df$hometown)
    
    input_df$hometown = mapvalues(input_df$hometown, from =states,to = values)
    
    mode(input_df$hometown) = "numeric"
    
    
    #drop a few irrelevant columns
    input_df = input_df[, !(names(input_df) %in% c("user_url", "username", "current_school", "user_major",
                                                   "accepted_schools", "waitlisted_schools",
                                                   "rejected_schools", "rank", "num_of_Essays",
                                                   "num_of_Advice", "num_of_schools", "ACT", "class_of"))]
    
    #fill na values with mean 
    input_df = na.aggregate(input_df)
    
    
    #remove outliers for SAT & GPA
    outliers_SAT <- boxplot(input_df$SAT, plot = FALSE)$out
    input_df = input_df[!(input_df$SAT %in% outliers_SAT), ]
    
    # outliers_GPA <- boxplot(input_df$GPA, plot = FALSE)$out
    # input_df = input_df[!(input_df %in% outliers_GPA), ]
    # 
    
    # #predict using model
    user_input = data.frame(
      "hometown" = h[[tolower(input$Hometown)]],
      "gender" = ifelse(input$Gender == "Male", 1, 0),
      "ethnicity" = ifelse(input$Ethnicity=="Native American", 1,
                           ifelse(input$Ethnicity=="Black / African American", 2,
                                  ifelse(input$Ethnicity=="Hispanic",3,
                                         ifelse(input$Ethnicity=="Asian", 4,
                                                ifelse(input$Ethnicity=="White Non-Hispanic",5,0))))),
      "English_FL" = ifelse(input$English_FL == "Yes", 1, 0),
      "Admission" = ifelse(input$Admission_Type == "Early", 1, 0),
      "first_gen" = ifelse(input$First_Gen == "Yes", 1, 0),
      "legacy" = ifelse(input$Legacy == "Yes", 1, 0),
      "GPA_measure" = ifelse(input$GPA_Scale == "Weighted", 1, 0),
      "GPA" = input$GPA + input$slider_1,
      "num_of_Scores" = input$Num_of_Scores,
      "num_of_ECs" = input$Num_of_ECs,
      "num_of_Sports" = input$Num_of_Sports,
      "SAT"=input$SAT + input$slider)
    
    #fit the model
    model = glm(Result ~ ., family = binomial(link='logit'), data=input_df)
    
    #predict on the model 
    model %>%
      predict(user_input, type="response") ->prediction
    
    
    
    #compute model performance based on same input of data
    sample <- sample.int(n = nrow(input_df), size = floor(.75*nrow(input_df)), replace = F)
    
    #split test and train
    train = input_df[sample,]
    test = input_df[-sample,]
    
    
    #only compute test if factor is bigger than 1
    if (length(unique(test$Result))>1) {
      #compute model performance based on same input of data
      model_test = glm(Result ~ ., family = binomial(link='logit'), data=train)
      # # 
      model_test %>%
        predict(test, type="response") ->prediction_test
      # # 
      prediction_test = ifelse(prediction_test>0.5, 1, 0)
      
      prediction_test = as.factor(prediction_test)
      test$Result = as.factor(test$Result)
      #
      model_perf = confusionMatrix(test$Result, prediction_test)
      
      conf = confusion_matrix(targets = test$Result, 
                              predictions = prediction_test)
      
      plot_confusion_matrix(conf$`Confusion Matrix`[[1]])
      
      
    }else{
      model_perf = "Not Enough Data"
    }
    

    # print(model_perf)
  })


}

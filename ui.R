ui <- dashboardPage(
  dashboardHeader(), 
  dashboardSidebar(
    sidebarMenu(id="sidebar", 
    menuItem("Chance Me", tabName = "Chance_Me", icon = icon("address-card")),
    menuItem("SAT Score", tabName = "SAT_Score", icon = icon("pencil-ruler")), 
    menuItem("GPA", tabName = "GPA", icon=icon("graduation-cap")), 
    menuItem("Result", tabName = "Result", icon=icon("check-square"))
  ))
  ,
  dashboardBody(
    
    tabItems(
      tabItem("Chance_Me",
              fluidRow(
                column(6,
                       selectInput("Ethnicity", "Ethnicity", c("Native American", "Black / African American",
                                                               "Hispanic","Asian","White Non-Hispanic"), width="70%"),
                       radioButtons(inputId="English_FL", label="English First Language?", choices=c("Yes","No")),
                       radioButtons(inputId="Admission_Type", label="Admission Type", choices=c("Regular","Early")), 
                       radioButtons(inputId="First_Gen", label="First Generation?", choices=c("Yes","No")),
                       radioButtons(inputId="Legacy", label="Legacy?", choices=c("Yes","No")),
                       radioButtons(inputId="Gender", label="Gender", choices=c("Male","Female")),
                       selectInput("Hometown", "Hometown", state.name, width="60%")
          ),
                
                column(6,
                       numericInput(inputId = "SAT", label = "SAT Score (1600)", value = 1450, width = "60%", min = 0, max = 1600, step=50),
                       
                        numericInput(inputId = "GPA", label = "GPA", value = 4.0, width = "60%", min = 0, max = 5.0, step=0.1),
                        radioButtons(inputId="GPA_Scale", label="GPA Scale", choices=c("Weighted","Unweighted")),
                       numericInput(inputId = "Num_of_ECs", label = "Number of Extracurricular", value = 0, width = "60%", min = 0, max = 10, step=0.5),
                       numericInput(inputId = "Num_of_Sports", label = "Number of Sports Played", value = 0, width = "60%", min = 0, max = 10, step=0.5),
                       numericInput(inputId = "Num_of_Scores", label = "Number of AP & SATII Taken", value = 0, width = "60%", min = 0, max = 10, step=0.5),
                       
                        selectInput("School", "Pick a College", unique(Accepted_df$Accepted), width="60%")
                )
                
              ), 
              fluidRow(
                column(10, 
                       actionButton(inputId = "ActionButton", label = "Chance Me", width = "100%",
                                    style= "color: #fff; background-color: #337ab7; border-color: #2e6da4"))
                
              )
              
              
              ),
      
      tabItem("SAT_Score", 
              box(plotOutput("SAT_ND"), width ="100%"), 
              box(plotOutput("SAT_BOX"), width = "100%")
              ),
      
      tabItem("GPA",
              box(plotOutput("GPA_ND"), width ="100%"),
              box(plotOutput("GPA_BOX"), width ="100%")
              
              ),
      
      tabItem("Result",

              sidebarPanel(
              sliderInput("slider",
                          label= "SAT Score Improvement:",
                          min=0, max=400, value=0),
              sliderInput("slider_1",
                          label= "GPA Improvement:", 
                          min=0, max = 1, value = 0),
              ),
              htmlOutput("Model")
      )
      
  
    )
    
  )

)









  


server <- function(input, output){
  
  #Normal distribution for SAT
  output$SAT_ND <- renderPlot({
    
    SAT_mean = mean(Accepted_df[Accepted_df$Accepted==input$School,]$SAT, na.rm = T)
    Accepted_df %>%
      filter(Accepted == input$School) %>%
      ggplot(aes(SAT)) + 
      geom_histogram(aes(y=..density..),
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
      geom_boxplot() +
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
      geom_boxplot() +
      xlim(-1,1) + 
      geom_hline(yintercept = input$GPA, linetype="dashed", 
                 size=1, color="blue") + 
      annotate(x=-0.7, y=input$GPA, label="Your GPA", 
               color="blue", geom="label")
   
    
  })
  
  
}

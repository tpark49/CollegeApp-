#SAT (user can choose 2400 scale, but will be converted to 1600)
SAT_mean = mean(Accepted_df[Accepted_df$Accepted=="UPenn",]$SAT, na.rm = T)
Accepted_df %>%
  filter(Accepted == "UPenn") %>%
  ggplot(aes(SAT)) + 
  geom_histogram(aes(y=..density..),
                 breaks = seq(500, 2000, by = 20),
                 color="grey",
                 fill="white") +
  geom_density(alpha=0.3,
               fill ="#FF6666") +
  xlim(1000, 1800) +  
  geom_vline(aes(xintercept=SAT_mean),
             col="black", size=1, linetype="dashed")+ 
  annotate(x=SAT_mean, y=0, label="mean", geom="label") + 
  geom_vline(aes(xintercept=1550), 
             col="blue", size=1, linetype="dashed") + 
  annotate(x=1550, y=0.007, label="Your Score", geom="label", 
           color="blue")

#boxplot - SAT
Accepted_df %>%
  filter(Accepted == "UPenn") %>%
  ggplot(aes(y=SAT)) +
  geom_boxplot() +
  xlim(-1,1) +
  geom_hline(yintercept = 1550, linetype="dashed",
             size=1, color="blue") +
  annotate(x=-0.7, y=1550, label="Your GPA",
           color="blue", geom="label")


#GPA - Weighted (or unweighted - user can specify)
GPA_mean = mean(Accepted_df[Accepted_df$Accepted=="UPenn"&Accepted_df$GPA_measure=="weighted",]$GPA, na.rm = T)

Accepted_df %>%
  filter(Accepted == "UPenn" & GPA_measure=="weighted") %>%
  ggplot(aes(GPA)) +
  geom_histogram(aes(y=..density..),
                 bins = 20,
                 # breaks = seq(500, 2000, by = 20),
                 color="grey",
                 fill="white") +
  geom_density(alpha=0.3,
               fill ="#FF6666") +
  geom_vline(aes(xintercept=GPA_mean),
             col="black", size=1, linetype="dashed")+
  annotate(x=GPA_mean, y=0, label="mean", geom="label") + 
  geom_vline(aes(xintercept=4.5), 
             col="blue" ,size=1, linetype="dashed") + 
  annotate(x=4.5, y=5, label="Your GPA", 
           color="blue", geom="label")

#boxplot - GPA
Accepted_df %>%
  filter(Accepted == "UPenn" & GPA_measure=="weighted") %>%
  ggplot(aes(y=GPA)) +
  geom_boxplot() +
  xlim(-1,1) + 
  geom_hline(yintercept = 4.5, linetype="dashed", 
             size=1, color="blue") + 
  annotate(x=-0.7, y=4.5, label="Your GPA", 
           color="blue", geom="label")








  

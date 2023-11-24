#Weekly Epidemiological Record
library(tidyverse)
library(ggplot2)

library(readxl)
Df <- read_excel("")
View(Df)

names <- c('WEEKLY' ,'year')
Df[,names] <- lapply(Df[,names] , factor) 
str(Df)

attach(DENV)
names(DENV)

#Option 1
Test <- ggplot(Df, aes(WEEKLY,fill=year))
Test+geom_bar()+
  labs(y="Cases", x="Weekly Epidemiological Record")+
  theme(axis.title=element_text(size=24,face="bold"))+
  theme(axis.text.x = element_text(size = 25, angle = 90, vjust = 1.5, hjust = 1)) +
  theme(axis.text.y = element_text(size = 25, vjust = 1, hjust = 1)) +
  theme(text = element_text(size = 24))


#Option 2 Assuming DENV has columns named "Weekly", "Year", and "Cases"
df <- df %>%
  group_by(year, WEEKLY) %>%
  summarise(Cases = n())

attach(Df)
names(Df) 

Df$WEEKLY <- as.numeric(as.character(Df$WEEKLY))

ggplot(Df, aes(x = WEEKLY, y = Cases, color = as.factor(year))) +
  geom_line() +
  labs(title = "Cases Over Weeks", x = "Weekly", y = "Cases") +
  theme(axis.title=element_text(size=24,face="bold"))+
  theme(axis.text.x = element_text(size = 25, angle = 90, vjust = 1.5, hjust = 1)) +
  theme(axis.text.y = element_text(size = 25, vjust = 1, hjust = 1)) +
  theme(text = element_text(size = 24))
  
  


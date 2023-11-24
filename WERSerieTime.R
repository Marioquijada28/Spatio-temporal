library(tidyverse)
library(ggplot2)

library(readxl)
DENV <- read_excel("Desktop/Arbovirus/Data/Arbovirus/DENV/In process data/DENV.xlsx")
View(DENV)

names <- c('WEEKLY' ,'year')
DENV[,names] <- lapply(DENV[,names] , factor) 
str(DENV)

attach(DENV)
names(DENV)

#Option 1
Test <- ggplot(DENV, aes(WEEKLY,fill=year))
Test+geom_bar()+
  labs(y="Cases", x="Weekly Epidemiological Record")+
  theme(axis.title=element_text(size=24,face="bold"))+
  theme(axis.text.x = element_text(size = 25, angle = 90, vjust = 1.5, hjust = 1)) +
  theme(axis.text.y = element_text(size = 25, vjust = 1, hjust = 1)) +
  theme(text = element_text(size = 24))


#Option 2 Assuming DENV has columns named "Weekly", "Year", and "Cases"
DENV1 <- DENV %>%
  group_by(year, WEEKLY) %>%
  summarise(Cases = n())

attach(DENV1)
names(DENV1) 

DENV1$WEEKLY <- as.numeric(as.character(DENV1$WEEKLY))

ggplot(DENV1, aes(x = WEEKLY, y = Cases, color = as.factor(year))) +
  geom_line() +
  labs(title = "Dengue Cases Over Weeks", x = "Weekly", y = "Cases") +
  theme(axis.title=element_text(size=24,face="bold"))+
  theme(axis.text.x = element_text(size = 25, angle = 90, vjust = 1.5, hjust = 1)) +
  theme(axis.text.y = element_text(size = 25, vjust = 1, hjust = 1)) +
  theme(text = element_text(size = 24))
  
  


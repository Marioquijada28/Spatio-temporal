library(ggplot2)
library(rlang)
library(readxl)
library(ggplot2)
library(tidyr)

DENV <- read_excel("~/Desktop/Arbovirus/Data/Arbovirus/DENV/In process data/DENV.xlsx")

group_by(DENV, year, age)
summarise(DENV1, year, age, Cases = n())

DENV1 <- DENV %>%
  group_by(year, age) %>%
  summarise(Cases = n(), .groups = "drop")

DENV1$age.group <- findInterval(DENV1$age, c(0,10,20,30,40,50,60,70,80,90,100))

#0-9 10-19 20-29 30-39 40-49 50-59 60-69 70-79 80-89 90-99

DENV1$Agestratified <- as.character(cut(DENV1$age,
                                                     breaks = c(-1,9,19,29,39,49,59,69,79,89,99),
                                                     labels = c("0-9", "10-19", "20-29", "30-39", "40-49", "50-59", "60-69", "70-79", "80-89", "90-99"),
                                                     right = TRUE))

DENV1$Agestratified <- cut(DENV1$age,
                                        breaks = c(-1,9,19,29,39,49,59,69,79,89,99),
                                        labels = c("0-9", "10-19", "20-29", "30-39", "40-49", "50-59", "60-69", "70-79", "80-89", "90-99"),
                                        right = TRUE)
#firstOption
Agestratified <- ggplot(DENV, aes(Agestratified ,fill=as.factor(Year)))
Agestratified+geom_bar(width = .20)+
  labs(y="Cases", x="Age") +
  theme(axis.title=element_text(size=20,face="bold"))+
  theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1))

#SecondtOption
Agestratified <- ggplot(DENV, aes(Agestratified ,fill=as.factor(Year)))
Agestratified+geom_bar(width = .20)+
  labs(y="Cases", x="Age") +
  theme(axis.title=element_text(size=20,face="bold"))+
  theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1))



library(ggplot2)

Agestratified <- ggplot(DENV, aes(x = Agestratified, fill = as.factor(Year), group = Year)) +
  geom_bar(width = 0.20) +
  labs(y = "Cases", x = "Age") +
  theme(axis.title = element_text(size = 20, face = "bold")) +
  theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust = 1))


# Another option to plot this data of AS
DENVAS <- ggplot(DENV, aes(x = Agestratified, y= Cases ,color = as.factor(Year))) +
geom_line() +
  labs(title = "Dengue Cases Over Weeks", x = "Weekly", y = "Cases")

library(ggplot2)

ggplot(DENVAS, aes(x = Agestratified, y = Cases, color = as.factor(Year))) +
  geom_line() +
  labs(title = "Dengue Cases Over Weeks", x = "Weekly", y = "Cases")



DENVAS+geom_bar(width = .20)+
  labs(y="Cases", x="Age") +
  theme(axis.title=element_text(size=20,face="bold"))+
  theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1))

ggplot() +
  geom_line(data = DENGUE, aes(x = age.group, y = Cases, color = as.factor(Year))) +
  labs(title = "Dengue Cases Over Weeks", x = "Weekly", y = "Cases")


ggplot() +
  geom_line(data = DENVAS, aes(x = age.group, y = Cases, color = as.factor(Year))) +
   labs(title = "Dengue Cases Over Weeks", x = "Weekly", y = "Cases")



geom_point(data = DENVAS, aes(x = age.group, y = Cases), color = "red")

#lastGraphpositiondodge

ggplot(DENV, aes(x = Agestratified, fill = Year)) +
  geom_bar(width = 0.8, position = position_dodge(width = 0.9)) +  # Adjust width if needed
  labs(y = "Cases", x = "Age") +
  theme(axis.title = element_text(size = 20, face = "bold"),
        axis.text.x = element_text(angle = 90, vjust = 0.5, hjust = 1))

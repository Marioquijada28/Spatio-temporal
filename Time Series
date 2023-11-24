#Time Series
library(zoo)
library(dplyr)
library(ggplot2)
library(ggfortify)
library(nlme)
library("mFilter")

attach(DENV)
names(DENV)


group_by(DENV, year)

DENV <- group_by(DENV, year)
summarise(DENV, year = n())

DENVTS <- group_by(DENV, year)
summarise(DENVTS, CASES = n())

DENVTS <- summarise(DENVTS, CASES = n())

tseries=ts(DENVTS$CASES, start = c(2000), frequency = 1)
start(tseries); end(tseries)
class(tseries)
plot(tseries, ylab = "Cases", main = "Cases 2000-2022", col='red', lwd=3)

#Another option to create plot with point.
ggplot() +
  geom_line(data = DENVTS, mapping = aes(x=YEAR, y=CASES),, color='black')+
  geom_point(data = DENVTS, mapping = aes(x=YEAR, y=CASES), color="black")
labs(x="Year",y="Cases", main = "Time series 2000-2022")

#Another option to create plot with point and all year.
ggplot() +
  geom_line(data = DENVTS, mapping = aes(x = year, y = CASES), color = 'black') +
  geom_point(data = DENVTS, mapping = aes(x = year, y = CASES), color = "black") +
  scale_x_continuous(breaks = seq(2000, 2022, by = 1)) +
  labs(title = "2000-2022", x = "Year", y = "Cases") +
  theme(axis.title = element_text(size = 25, face = "bold")) +
  theme(axis.text.x = element_text(size = 25, angle = 90, vjust = 1.5, hjust = 1)) +
  theme(axis.text.y = element_text(size = 25, vjust = 1, hjust = 1)) +
  theme(text = element_text(size = 24))


ggplot(data = DENVTS, aes(x = year, y = CASES)) +
  geom_line(color = 'black') +
  geom_point(color = "black") +
  geom_text(aes(label = CASES), vjust = -0.5, hjust = 0.5, size = 10) +  # Add this line
  scale_x_continuous(breaks = seq(2000, 2022, by = 1)) +
  labs(title = "2000-2022", x = "Year", y = "Cases") +
  theme(axis.title = element_text(size = 25, face = "bold")) +
  theme(axis.text.x = element_text(size = 25, angle = 90, vjust = 1.5, hjust = 1)) +
  theme(axis.text.y = element_text(size = 25, vjust = 1, hjust = 1)) +
  theme(text = element_text(size = 24))




library(tidyr)
library(dplyr)
library(datasets)
set.seed(10)
messy <- data.frame(id = 1:4,
trt = sample(rep(c('control', 'treatment'), each = 2)),
work.T1 = runif(4),
home.T1 = runif(4),
work.T2 = runif(4),
home.T2 = runif(4))
messy
# We want to gather all the columns, except for the id and trt ones,
# in two columns key and value:
gathered.messy <- gather(messy, key, value, -id, -trt)
head(gathered.messy)

########
# spread
#####

set.seed(14)
stocks <- data.frame(time = as.Date('2009-01-01') + 0:9,
X = rnorm(10, 0, 1),
Y = rnorm(10, 0, 2),
Z = rnorm(10, 0, 4))
stocksm
stocksm <- gather(stocks, stock, price, -time)
stocksm
spread.stock <- spread(stocksm, stock, price)
head(spread.stock)
spread.stock

########
# unite
#####

set.seed(1)
date <- as.Date('2016-01-01') + 0:14
hour <- sample(1:24, 15)
min <- sample(1:60, 15)
second <- sample(1:60, 15)
event <- sample(letters, 15)
data <- data.frame(date, hour, min, second, event)
data

# Now, let us combine the date, hour, min, and
# second columns into a new column called datetime.
dataNew <- data %>%
unite(datehour, date, hour, sep = ' ') %>%
unite(datetime, datehour, min, second, sep = ':')
dataNew
data

########
# dplyr
#####
airquality
filter(airquality, Temp > 70)
filter(airquality, Temp > 80 & Month > 5)
mutate(airquality, TempInC = (Temp - 32) * 5 / 9)
summarise(airquality, mean(Temp, na.rm = TRUE))
summarise(group_by(airquality, Month), mean(Temp, na.rm = TRUE))
arrange(airquality, desc(Month), Day)
airquality %>%
filter(Month != 5) %>%
group_by(Month) %>%
summarise(mean(Temp, na.rm = TRUE))

#*******************************************************************************************************
# Exploratory Data Analysis: Bike Dataset
#*******************************************************************************************************


#*******************************************************************************************************
# 1 - Damos hipótesis sobre el dataset:
#    Quizá la estacion o clima influya en el número de bicis a tener disponibles... 
#    Si llueve seguramente se necesiten menos...
#*******************************************************************************************************

#Leemos los datos

train<-read.csv("./datasets/train.csv", header=TRUE)
test<-read.csv("./datasets/test.csv", header=TRUE)

#*******************************************************************************************************
# 2 - Buscamos variables dependientes e indenpendientes y vemos por encima una primera aproximacion 
# al problema.
#
# Para poder comprobar los datos completos uniremos train y test, pero antes hay que añadir
# a test los datos de las variables dependientes que le faltan. 
#*******************************************************************************************************

# Las variables dependientes son aquellas que no están en el test
# Las independientes serán las siguientes

test$casual<-0
test$count<-0
test$registered<-0

allData<-rbind(train, test)

#*******************************************************************************************************
# 3 - Buscamos valores perdidos 
#*******************************************************************************************************

is.na(allData)


#*******************************************************************************************************
# 4 - Pintamos histogramas para ver la distribucion de las variables
#*******************************************************************************************************


multiplot <- function(..., plotlist=NULL, file, cols=1, layout=NULL) {
  library(grid)
  
  # Make a list from the ... arguments and plotlist
  plots <- c(list(...), plotlist)
  
  numPlots = length(plots)
  
  # If layout is NULL, then use 'cols' to determine layout
  if (is.null(layout)) {
    # Make the panel
    # ncol: Number of columns of plots
    # nrow: Number of rows needed, calculated from # of cols
    layout <- matrix(seq(1, cols * ceiling(numPlots/cols)),
                     ncol = cols, nrow = ceiling(numPlots/cols))
  }
  
  if (numPlots==1) {
    print(plots[[1]])
    
  } else {
    # Set up the page
    grid.newpage()
    pushViewport(viewport(layout = grid.layout(nrow(layout), ncol(layout))))
    
    # Make each plot, in the correct location
    for (i in 1:numPlots) {
      # Get the i,j matrix positions of the regions that contain this subplot
      matchidx <- as.data.frame(which(layout == i, arr.ind = TRUE))
      
      print(plots[[i]], vp = viewport(layout.pos.row = matchidx$row,
                                      layout.pos.col = matchidx$col))
    }
  }
}

library(ggplot2)

p1<-ggplot(allData, aes(season)) + geom_histogram(binwidth = 0.5) 
p2<-ggplot(allData, aes(holiday)) + geom_histogram(binwidth = 0.5) 
p3<-ggplot(allData, aes(workingday)) + geom_histogram(binwidth = 0.5) 
p4<-ggplot(allData, aes(weather)) + geom_histogram(binwidth = 0.5) 
p5<-ggplot(allData, aes(temp)) + geom_histogram(binwidth = 0.5) 
p6<-ggplot(allData, aes(atemp)) + geom_histogram(binwidth = 0.5) 
p7<-ggplot(allData, aes(humidity)) + geom_histogram(binwidth = 0.5) 
p8<-ggplot(allData, aes(windspeed)) + geom_histogram(binwidth = 0.5)


multiplot(p1, p2, p3, p4,p5,p6,p7,p8, cols=2)


#*******************************************************************************************************
# 5 - Ingenieria de carácteristicas
#*******************************************************************************************************


#Generamos una variable para la hora

allData$hour<-substring(allData$datetime, 12, 13)
allData$hour<-as.factor(allData$hour)

train = allData[as.integer(substr(allData$datetime,9,10))<20,]
test = allData[as.integer(substr(allData$datetime,9,10))>19,]

#Los usuarios registrados tienen dos picos, tarde y primera hora
boxplot(train$registered~train$hour)

#Los usuarios casuales tienen una distribucion a lo largo de la mañana, posible turismo
boxplot(train$casual~train$hour)


# En los gráficos de cajas hemos visto muchos outliers. Probablemente en los ocasionales tenemos muchos
# outliers debido a fiestas. Si comprobamos los outliers en los datos de los registrados, son muchos menos. 
# Si realizamos una transformacion logaritmica sobre los datos tendremos menos outliers. 


#Generamos una variable categorica para el dia

allData$day<-substring(allData$datetime, 1, 10)

allData$days<-weekdays(as.Date(allData$day))

train = allData[as.integer(substr(allData$datetime,9,10))<20,]
test = allData[as.integer(substr(allData$datetime,9,10))>19,]


#Vemos la distribucion

#Los usuarios registrados tienen dos picos, tarde y primera hora
boxplot(train$registered~train$days)

#Los usuarios casuales tienen una distribucion a lo largo de la mañana, posible turismo
boxplot(train$casual~train$days)




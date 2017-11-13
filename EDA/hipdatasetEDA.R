#*******************************************************************************************************
# Exploratory Data Analysis
#*******************************************************************************************************

#*******************************************************************************************************
#Descargate el  dataset hip con el siguiente commando 
#*******************************************************************************************************


hip  <-read.table("http://astrostatistics.psu.edu/datasets/HIP_star.dat", header=T,fill=T)

#*******************************************************************************************************
# Una vez descargado comprueba la dimensión y los nombres de las columnas del dataset. 
# ¿Qué dimensión tiene? ¿qué datos alberga?
#*******************************************************************************************************

dimnames(hip)

# La dimension de hip es 2719 observaciones con 9 variables

apply(hip,2,typeof)

# Todas las caracteristicas son de tipo double

#*******************************************************************************************************
# Muestra por pantalla la columna de la variable RA
#*******************************************************************************************************

hip[,3]

#*******************************************************************************************************
#Calcula las tendencias centrales de todos los datos del dataset (mean, media) utilizando la function apply
#*******************************************************************************************************

apply(hip,2,mean, na.rm=T)
apply(hip,2,median, na.rm=T)

#*******************************************************************************************************
# Haz lo mismo para las medidas de dispersión mínimo y máximo. ¿Seria posible hacerlo con un único comando?¿Que hace la función range()
#*******************************************************************************************************

apply(hip,2,min, na.rm=T)
apply(hip,2,max, na.rm=T)


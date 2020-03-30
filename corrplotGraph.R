data()
data(Orange)

View(Orange)

mean(Orange$age)

plot(Orange$age,Orange$circumference)

cor(Orange$age,Orange$circumference)

install.packages("corrplot")
library(corrplot)

data("mtcars")

m<-cor(mtcars) #Cria uma matriz de correla??o 'm'. Essa matriz ser? usada nas fun??es a seguir

corrplot(m) #padr?o com c?rculos: o tamanho do c?rculo mostra o tamanho da correla??o
corrplot(m, method = "color") #cores
corrplot(m, method = "ellipse") # elipses
corrplot(m, method = "shade") #elipses
corrplot(m, method = "number") #n?meros

#posi??o
corrplot(m, type = "upper")
corrplot(m, method = "color", type = "upper")

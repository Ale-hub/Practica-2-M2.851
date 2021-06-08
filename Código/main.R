##### 0. Librerías #####
if(!require("stringr")){
  install.packages("stringr")
  library("stringr")
}

if(!require("lubridate")){
  install.packages("lubridate")
  library("lubridate")
}

if(!require("randomForest")){
  install.packages("randomForest")
  library("randomForest")
}

if(!require("Hmisc")){
  install.packages("Hmisc")
  library("Hmisc")
}


if(!require("nortest")){
  install.packages("nortest")
  library("nortest")
}

if(!require("kableExtra")){
  install.packages("kableExtra")
  library("kableExtra")
}

##### 1. Lectura de datos #####
# Lectura del dataset.
data_fifa <- read.csv("data.csv", header=TRUE, encoding="UTF-8")
# Selección de columnas.
columnas <- c('ID', 'Age', 'Nationality', 'Overall', 'Club', 'Value', 'Wage',
              'Preferred.Foot','Weak.Foot','Skill.Moves', 'Work.Rate',
              'Position', 'Height', 'Weight')
fifa_DF <- data_fifa[,columnas]
# Estructura del dateset resultante
str(fifa_DF)

##### 2. Limpieza #####
# Resumen de los datos
summary(fifa_DF)

###### 2.1 Normalizar cuantitativos######
# Value
head(fifa_DF$Value)

fifa_DF$Value <- gsub("€", '',fifa_DF$Value)

for (i in 1:nrow(fifa_DF)) {
  # Si contiene M:
  if (str_detect(fifa_DF$Value[i], "M") && !is.na(fifa_DF$Value[i])) {
    # Elimino la M del string y convierto a double.
    valor <-  as.double(gsub("M", '',fifa_DF$Value[i]))
    # Como M representa 1 millón, multiplico el valor por 1 millón.
    fifa_DF$Value[i] <- valor * 1000000
  } 
  # Si contiene K:
  if (str_detect(fifa_DF$Value[i], "K") && !is.na(fifa_DF$Value[i])){
    # Elimino la K del string y convierto a double.
    valor <-  as.double(gsub("K", '',fifa_DF$Value[i]))
    # Como K representa mil, multiplico el valor por 1000.
    fifa_DF$Value[i] <- valor * 1000
  }
}
fifa_DF$Value <- as.numeric(fifa_DF$Value)
summary(fifa_DF$Value)

## Wage
fifa_DF$Wage <- gsub("€", '',fifa_DF$Wage)

for (i in 1:nrow(fifa_DF)) {
  # Si contiene M:
  if (str_detect(fifa_DF$Wage[i], "M") && !is.na(fifa_DF$Wage[i])) {
    # Elimino la M del string y convierto a double.
    valor <-  as.double(gsub("M", '',fifa_DF$Wage[i]))
    ## Como M representa 1 millón, multiplico el valor por 1 millón.
    fifa_DF$Wage[i] <- valor * 1000000
  } 
  # Si contiene K:
  if (str_detect(fifa_DF$Wage[i], "K") && !is.na(fifa_DF$Wage[i])){
    # Elimino la K del string y convierto a double.
    valor <-  as.double(gsub("K", '',fifa_DF$Wage[i]))
    # Como K representa mil, multiplico el valor por 1000.
    fifa_DF$Wage[i] <- valor * 1000
  }
}
fifa_DF$Wage <- as.numeric(fifa_DF$Wage)
summary(fifa_DF$Wage)

## Height
# Sustituyo la (') por el punto  (.) 
fifa_DF$Height <- gsub("'", '.',fifa_DF$Height)
# Convierto el tipo de la variable numeric
fifa_DF$Height <- as.numeric(fifa_DF$Height)
# Camio la unidedad de pies a metros.
fifa_DF$Height <- fifa_DF$Height*0.3048

summary(fifa_DF$Height)

## Weight
# Elimino los caracteres lbs del string
fifa_DF$Weight <- gsub("lbs", '',fifa_DF$Weight)
# Convierto el tipo de la variable numeric
fifa_DF$Weight <- as.numeric(fifa_DF$Weight)
# Camio la unidedad de libras a kilogramos
fifa_DF$Weight <- fifa_DF$Weight/2.2046

summary(fifa_DF$Weight)

###### 2.2 Normalizar categóricos######
## Nationality
unique(fifa_DF$Nationality)
# Hacer que los strings vacíos sean nulos
fifa_DF$Nationality[fifa_DF$Nationality==''] <- NA
# Convertir a factor
fifa_DF$Nationality <- factor(fifa_DF$Nationality)


## Club
unique(fifa_DF$Club)
# Hacer que los strings vacíos sean nulos
fifa_DF$Club[fifa_DF$Club==''] <- NA
# Convertir a factor
fifa_DF$Club <- factor(fifa_DF$Club)
summary(fifa_DF$Club)

## Preferred.Foot
unique(fifa_DF$Preferred.Foot)
# Hacer que los strings vacíos sean nulos
fifa_DF$Preferred.Foot[fifa_DF$Preferred.Foot==''] <- NA
# Convertir a factor
fifa_DF$Preferred.Foot <- factor(fifa_DF$Preferred.Foot)

summary(fifa_DF$Preferred.Foot)

## Work.Rate
unique(fifa_DF$Work.Rate)
# Hacer que los strings vacíos sean nulos
fifa_DF$Work.Rate[fifa_DF$Work.Rate==''] <- NA
# Convertir a factor
fifa_DF$Work.Rate <- factor(fifa_DF$Work.Rate)

summary(fifa_DF$Work.Rate)

## Position
unique(fifa_DF$Position)
# Hacer que los strings vacíos sean nulos
fifa_DF$Position[fifa_DF$Position==''] <- NA
# Convertir a factor
fifa_DF$Position <- factor(fifa_DF$Position)

summary(fifa_DF$Position)

###### 2.3 Nulos ######
colSums(is.na(fifa_DF))
# idx <- which(is.na(fifa_DF$Height))
# # Imputación con random forest
# set.seed(123)
# fifa_imputed <- rfImpute(Value ~ Age + Overall + Wage + Preferred.Foot + 
#                            Weak.Foot + Skill.Moves + Work.Rate + Position +
#                            Height + Weight,
#                          ntree=300,
#                          iter=3,
#                          data=fifa_DF[,!names(fifa_DF) %in% c('Nationality', 'Club')])
# 
# colSums(is.na(fifa_imputed))
# fifa_imputed[idx,]
# summary(fifa_imputed[idx,])
# 
# 
# fifa_imputed_2 <- aregImpute(~Age + Overall + Wage + Preferred.Foot + 
#                                Weak.Foot + Skill.Moves + Work.Rate + Position +
#                                Height + Weight + Value,
#                              data = fifa_DF[,!names(fifa_DF) %in% c('ID', 'Nationality', 'Club')],
#                              n.impute = 5)

fifa_clean <- fifa_DF[complete.cases(fifa_DF[, c('Overall', 'Value', 'Wage',
                                                 'Preferred.Foot', 'Weak.Foot',
                                                 'Skill.Moves', 'Work.Rate',
                                                 'Position', 'Height', 'Weight')
                                             ]),]

###### 2.4 Outliers ######
# Age
boxplot(fifa_clean$Age, col="lightcyan", horizontal=TRUE, main="Age")
points(mean(fifa_clean$Age), 1, col ="red", pch = 19)

# Overall
boxplot(fifa_clean$Overall, col="lightcyan", horizontal=TRUE, main="Overall")
points(mean(fifa_clean$Overall), 1, col ="red", pch = 19)

# Wage
boxplot(fifa_clean$Wage, col="lightcyan", horizontal=TRUE, main="Wage")
points(mean(fifa_clean$Wage), 1, col ="red", pch = 19)

# Value
boxplot(fifa_clean$Value, col="lightcyan", horizontal=TRUE, main="Value")
points(mean(fifa_clean$Value), 1, col ="red", pch = 19)

# Height
boxplot(fifa_clean$Height, col="lightcyan", horizontal=TRUE, main="Height")
points(mean(fifa_clean$Height), 1, col ="red", pch = 19)

# Weight
boxplot(fifa_clean$Weight, col="lightcyan", horizontal=TRUE, main="Weight")
points(mean(fifa_clean$Weight), 1, col ="red", pch = 19)

##### 3. Análisis #####
###### 3.1 Selección de grupos ######
# jugadores de campo
jCampo <- fifa_clean[fifa_clean$Position!='GK',]
# Zurdos
zurdos <- jCampo[jCampo$Preferred.Foot=='Left',]
# Diestro
diestros <- jCampo[jCampo$Preferred.Foot=='Right',]

###### 3.2 Normalidad y homogeneidad de la varianza ######

# Test de normalidad de Lilliefors. Kolmogorov-Smirnov
for (i in 2:ncol(fifa_clean)) {
  if(is.numeric(fifa_clean[,i])){
    if(lillie.test(fifa_clean[,i])$p.value<0.05){
      print(paste(colnames(fifa_clean)[i], ' No normal. p-value=',
                  lillie.test(fifa_clean[,i])$p.value, sep=' '))
    }else{
      print(paste(colnames(fifa_clean)[i], ' Distribución normal. p-value=',
                  lillie.test(fifa_clean[,i])$p.value, sep=' '))
    }
    
    hist(fifa_clean[,i], col="lightcyan", main=paste("Histograma de ",
                                                     colnames(fifa_clean)[i]),
         xlab="Rating", ylab="Frecuencia", breaks=20)
  }
}

# Test de homogeneidad de varianzas

for (i in 2:ncol(fifa_clean)) {
  if(is.numeric(fifa_clean[,i])){
    if(var.test(zurdos[,i], diestros[,i])$p.value<0.001){
      print(paste(colnames(fifa_clean)[i],
                  'Varianzas disintas p-value=',
                  var.test(zurdos[,i], diestros[,i])$p.value,
                  sep=' '))
    }else{
      print(paste(colnames(fifa_clean)[i],
                  'Homogeneas. p-value=',
                  var.test(zurdos[,i], diestros[,i])$p.value,
                  sep=' '))
    }
  }
}

###### 3.3 Pruebas estadísticas ######
# ¿Los zurdos tienen mejor Overall que los diestros?
# Asumimos normalidad por el teorema del límite central
t.test(zurdos$Overall, diestros$Overall,
       var.equal = FALSE,
       conf.level=0.95,
       alternative="greater")

# ¿ Qué atributos están más relacionadas con el Overall ?
columnas <- c('Age', 'Weak.Foot', 'Skill.Moves', 'Height', 'Weight')
corr_coef <- columnas
p_val <- columnas
for (i in 1:length(columnas)) {
  result = cor.test(jCampo[,columnas[i]],
                    jCampo[,'Overall'],
                    method = "spearman",
                    exact=FALSE)
    
  corr_coef[i] <- result$estimate
  p_val[i] <- result$p.value

}

out1 <- data.frame( var=columnas,
                   Correlation=corr_coef,
                   pvalue=p_val)
out1 %>% kable() %>% kable_styling()


# Predecir el valor de los jugadores
lm1 <- lm(Value ~ Age + Overall + Club + Position, data = fifa_clean)
lm2 <- lm(Value ~ Age + Preferred.Foot + Weak.Foot +
            Skill.Moves + Height + Weight,
          data = fifa_clean)
lm3 <- lm(Value ~ Overall + Position + Work.Rate, data = fifa_clean)

coeficientes <- matrix(c(1, summary(lm1)$r.squared,
                               2, summary(lm2)$r.squared,
                               3, summary(lm3)$r.squared),
                             ncol = 2, byrow = TRUE)

out2 <- data.frame( var=coeficientes[,1],
                    R_squared=coeficientes[,2]
                    )
out2 %>% kable() %>% kable_styling()

nuevosJugadores <- data.frame(
  Age = c(22,28,18),
  Overall = c(75,85,68),
  Club = c('Borussia Dortmund', 'Borussia Dortmund', 'Chelsea'),
  Position = c('CB', 'GK', 'RF')
)

predict(lm1, nuevosJugadores)

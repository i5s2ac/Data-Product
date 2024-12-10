library(dplyr)
library(plumber)
library(rpart)

fit <- readRDS("final_model.rds")

#' @param Pclass Clase en la que viaja el pasajero
#' @param Sex Genero
#' @param Age Edad
#' @param SibSp Numero de hermanos
#' @param Parch Numero de parientes
#' @param Fare Costo del ticket
#' @param Embarked Puerto de embarque
#' @post /survival
function(Pclass, Sex, Age, SibSp, Parch, Fare, Embarked) {
  features <- data.frame(Pclass = as.integer(Pclass),
                         Sex = Sex,
                         Age = as.numeric(Age),
                         SibSp = as.integer(SibSp),
                         Parch = as.integer(Parch),
                         Fare = as.numeric(Fare),
                         Embarked = Embarked)
  out <- predict(fit, features, type = "class")
  as.character(out)
}
#GRUPO 6 
#SCRAPPING DATOS GEIH DE LA PAGINA DE GITHUB DEL PROFESOR
## Librería necesaria
require(pacman)
library(pacman)
library(stargazer)
p_load(tidyverse,rvest,skimr) 

#Scrapping tabla 1
set_datos_1<-"https://ignaciomsarmiento.github.io/GEIH2018_sample/pages/geih_page_1.html"
set_datos_1_html<-read_html(set_datos_1)

tabla_datos_1<-set_datos_1_html%>%
  html_table()%>%
  as.data.frame()

#Scrapping tabla 2
set_datos_2<-"https://ignaciomsarmiento.github.io/GEIH2018_sample/pages/geih_page_2.html"
set_datos_2_html<-read_html(set_datos_2)

tabla_datos_2<-set_datos_2_html%>%
  html_table()%>%
  as.data.frame()

#Scrapping tabla 3
set_datos_3<-"https://ignaciomsarmiento.github.io/GEIH2018_sample/pages/geih_page_3.html"
set_datos_3_html<-read_html(set_datos_3)

tabla_datos_3<-set_datos_3_html%>%
  html_table()%>%
  as.data.frame()

#Scrapping tabla 4
set_datos_4<-"https://ignaciomsarmiento.github.io/GEIH2018_sample/pages/geih_page_4.html"
set_datos_4_html<-read_html(set_datos_4)

tabla_datos_4<-set_datos_4_html%>%
  html_table()%>%
  as.data.frame()

#Scrapping tabla 5
set_datos_5<-"https://ignaciomsarmiento.github.io/GEIH2018_sample/pages/geih_page_5.html"
set_datos_5_html<-read_html(set_datos_5)

tabla_datos_5<-set_datos_5_html%>%
  html_table()%>%
  as.data.frame()

#Scrapping tabla 6
set_datos_6<-"https://ignaciomsarmiento.github.io/GEIH2018_sample/pages/geih_page_5.html"
set_datos_6_html<-read_html(set_datos_6)

tabla_datos_6<-set_datos_6_html%>%
  html_table()%>%
  as.data.frame()

#Scrapping tabla 7
set_datos_7<-"https://ignaciomsarmiento.github.io/GEIH2018_sample/pages/geih_page_7.html"
set_datos_7_html<-read_html(set_datos_6)

tabla_datos_7<-set_datos_7_html%>%
  html_table()%>%
  as.data.frame()

#Scrapping tabla 8
set_datos_8<-"https://ignaciomsarmiento.github.io/GEIH2018_sample/pages/geih_page_8.html"
set_datos_8_html<-read_html(set_datos_8)

tabla_datos_8<-set_datos_8_html%>%
  html_table()%>%
  as.data.frame()

#Scrapping tabla 9
set_datos_9<-"https://ignaciomsarmiento.github.io/GEIH2018_sample/pages/geih_page_9.html"
set_datos_9_html<-read_html(set_datos_9)

tabla_datos_9<-set_datos_9_html%>%
  html_table()%>%
  as.data.frame()

#Scrapping tabla 10
set_datos_10<-"https://ignaciomsarmiento.github.io/GEIH2018_sample/pages/geih_page_10.html"
set_datos_10_html<-read_html(set_datos_10)

tabla_datos_10<-set_datos_10_html%>%
  html_table()%>%
  as.data.frame()

#Uniendo tablas de datos

base_todo <- rbind.data.frame(tabla_datos_1, tabla_datos_2, tabla_datos_3, tabla_datos_4, tabla_datos_5, tabla_datos_6, tabla_datos_7, tabla_datos_8, tabla_datos_9, tabla_datos_10)
as.data.frame(base_todo)
base_nueva<-select(base_todo, sex, age, relab, maxEducLevel, p6870, y_salary_m_hu, p6500, p6510s1, hoursWorkUsual)

base_nueva<-na.omit(base_nueva)

##base_todo[is.na(base_todo) | base_todo=="-Inf"] = NA


#Cálculo del salario:
base_nueva<-base_nueva %>%
  mutate(salario_mensual=p6500+p6510s1)%>%
  mutate(salario_mensual_hora=salario_mensual/(hoursWorkUsual*4))


#Logaritmo del salario
base_nueva<-base_nueva %>%
  mutate(log_salario_mensual_hora=log(salario_mensual_hora))

#Variable mujer. Cambio de Sex a female con female=1
base_nueva<-base_nueva %>%
  mutate(female = if_else(sex == 0, 1, 0) )

#En edad sólo mayores de 18 años
base_nueva<-base_nueva%>%
  filter(age>=18)

#Cambio de nombre para variable p6870: total personas de empresa donde labora:
base_nueva<-base_nueva %>% 
  rename(tam_empresa = p6870)%>%

#Variable edad al cuadrado
base_nueva<-base_nueva %>% 
  mutate(edad_2 = edad*edad)

##################
#PROBLEMA 4
##################

##################
##---------a. Estimación incondicional logaritmo salario vs género--------

reg_1<-lm(log_salario_mensual_hora ~ female, base_nueva)
pred_reg_1<-predict(reg_1)

summary(base_nueva)

stargazer(reg_1, type = "text",
          title = " Regresión Log Salario - Female",
          aling = TRUE,
          dep.var.labels = "Logaritmo del Salario",
          covariate.labels = c("female"),
          digits=3
)

summary(reg_1)

confint(reg_1)

####BORRAR##
summ = base_nueva %>%  
  group_by(female) %>%  
  summarize(
    mean_y = mean(log_salario_mensual_hora),
    pred_reg_1 = mean(pred_reg_1), .groups="drop"
  ) 



confint(reg_1)


#---------------b.i. EQUAL PAY FOR EQUAL JOB - TEOREMA FWL--------------
#Variables que se van a utilizar en el modelo: log_salario, mujer, relación laboral, 
#máximo nivel educativo alcanzado, tamaño de la empresa donde labora:

#Modelo lineal ln Salario con variables dependientes female y categoría de trabajo
reg_2<-lm(log_salario_mensual_hora ~ female+relab+maxEducLevel+edad+edad_2+tam_empresa, base_nueva)
stargazer(reg_2, type="text", digits=5)

stargazer(reg_2, type = "text",
          title = " Regresión Log Salario - todas las variables",
          aling = TRUE,
          dep.var.labels = "Logaritmo del Salario",
          digits=5
)

base_nueva<-base_nueva%>%
  mutate(pred_reg_2=predict(reg_2))


summary(reg_2, digits=3)
confint(reg_2)

#--------------------UTILIZANDO FWL---------------------
#PASO 1: Cálculo de los residuales de la regresión en los X2

reg_3<-lm(log_salario_mensual_hora~relab+maxEducLevel+edad+edad_2+tam_empresa, data=base_nueva)
stargazer(reg_3, type="text", digit=5)

base_nueva<-base_nueva %>%
  mutate(pred_reg_3=predict(reg_3))


resid_reg_3<-resid(reg_3)

#PASO 2: Cálculo de los residuales de la regresión de X1 y X2:
reg_4<-lm(female~relab+maxEducLevel+edad+edad_2+tam_empresa, base_nueva)
stargazer(reg_4, type="text", digit=5)
resid_reg_4=resid(reg_4)


#PASO 3: Regresión de ambos residuales
reg_5<-lm(resid_reg_3 ~ resid_reg_4 , base_nueva)

stargazer(reg_5, type = "text",
          title = " Regresión Resid reg_3 Resid reg_4",
          aling = TRUE,
          dep.var.labels = "Logaritmo del Salario",
          digits=5
)

#COMPARACION CON LA REGRESION INICIAL
stargazer(reg_5, reg_2, type="text", digit=5)

#Comparación de residuales
sum(resid(reg_5)^2)
sum(resid(reg_2)^2)



##----------------------FWL CON BOOTSTRAP-----
p_load("boot")

##Bootstrap con la función completa
sal_fn<-function(data,index) {
  coefs<-coef(lm(log_salario_mensual_hora ~ female+relab+maxEducLevel+edad+edad_2+tam_empresa, data=base_nueva, subset=index))
}

boot(base_nueva, sal_fn, R = 1000)


#FWL con bootstrap
#Se crea la función de FWL
fwl_bootstrap_log_salario<-function(data,index) {
  #FWL is the regression of residuals on residuals
  base_nueva$y_resid<-resid(lm(log_salario_mensual_hora~relab+maxEducLevel+edad+edad_2+tam_empresa, data=base_nueva, subset=index))
  base_nueva$x_resid<-resid(lm(female~relab+maxEducLevel+edad+edad_2+tam_empresa, data=base_nueva, subset=index))
  coef_interest<-coef(lm(y_resid~x_resid, data=base_nueva, subset=index))
  coef_interest
}

#verifico que funciona
lm(log_salario_mensual_hora~female+relab+maxEducLevel+edad+edad_sqr+tam_empresa,base_nueva)
fwl_bootstrap_log_salario(base_nueva,1:nrow(base_nueva))
#implemento Bootstrap
boot(base_nueva, fwl_bootstrap_log_salario, R = 1000)


###------------------Age-Wage profile-----------------
base_nueva<-base_nueva%>%
  mutate(mean_log_salario=mean(log_salario_mensual_hora))

base_nueva<-base_nueva%>%
  mutate(mean_pred_log_salario=mean(pred_reg_2))

## Gráfica Salario - Edad
base_nueva<-base_nueva%>%
  filter(edad<=55)

summ1 = base_nueva %>%
  group_by(
    female, edad) %>% 
  summarize(
    mean_y = mean(log_salario_mensual_hora),
    yhat_reg = mean(pred_reg_2), .groups="drop"
  ) 

ggplot(summ1) + 
  geom_point(
    aes(x = edad, y = mean_y, color=female)) + 
  geom_line(
    aes(x = edad, y = yhat_reg), 
    color = "green", size = 1.5
  ) + 
  labs(
    title = "Log salario con variables de control por edad",
    x = "Edad",
    y = "ln Salario"
  ) +
  theme_bw()
  
##-----------------------Peak Age---------------------

reg_edad<-lm(log_salario_mensual_hora ~ female+relab+maxEducLevel+edad+edad_2+tam_empresa, base_nueva)
stargazer(reg_edad, type="text", digits=5)

coefs_reg_edad <- reg_edad$coef
print(coefs_reg_2)


# Coeficientes como escalar:

b5 <- coefs_reg_edad[5]
b6 <- coefs_reg_edad[6]


# Cálculo del "peak age"

peak_age_reg2 <- (-b5/(2*b6))
peak_age_reg2

eta_mod2_fn<-function(data,index,
                      edad= 49) {
  
  #coeficientes
  coefs<-lm(log_salario_mensual_hora ~ female+relab+maxEducLevel+edad+edad_sqr+tam_empresa, data=base_nueva, subset = index)$coefficients
  
  #coeficientes en escalares  
  b5<-coefs[5]
  b6<-coefs[6] 
  
  #calcular peak age
  peak_age<-(-b5/(2*b6))
  
  #valor peak age
  return(peak_age)
}

eta_mod2_fn(base_nueva,1:nrow(base_nueva)) 

boot(base_nueva, eta_mod2_fn,R=1000)

resultados<-boot(base_nueva, eta_mod2_fn,R=1000)
resultados

boot.ci(resultados, type = c("norm", "basic"))

-----------------------PROBLEMA 5----------------------

####a. Dividimos la muestra en dos. 30% y 70%
set.seed(10101)
muestra <- sample(c(TRUE, FALSE), nrow(base_nueva), replace=TRUE, prob=c(0.7,0.3))
head(muestra)
sum(muestra)/nrow(base_nueva)

#70% es la muestra de entrenamiento y 30% la de testeo.
entrenamiento  <- base_nueva[muestra, ] 
test   <- base_nueva[!muestra, ]
dim(entrenamiento)

####b.
##########
##MODELO 1

modelo_1<-lm(log_salario_mensual_hora ~ female+relab+maxEducLevel, entrenamiento)
summary(modelo_1)
coef(modelo_1)
paste("Coef:", mean(entrenamiento$log_salario_mensual_hora))

#Predicción:
test$modelo_1<-predict(modelo_1,newdata = test)

#MSE Modelo_1
with(test,mean((log_salario_mensual_hora-modelo_1)^2))

###########
## MODELO 2
modelo_2<-lm(log_salario_mensual_hora ~ female+relab+maxEducLevel+edad+edad_sqr, base_nueva)
summary(modelo_2)
coef(modelo_2)
paste("Coef:", mean(entrenamiento$log_salario_mensual_hora))

#Predicción:
test$modelo_2<-predict(modelo_2,newdata = test)

#MSE Modelo_2
with(test,mean((log_salario_mensual_hora-modelo_2)^2))

###########
## MODELO 3
modelo_3<-lm(log_salario_mensual_hora ~ female+relab+maxEducLevel+edad+edad_sqr+tam_empresa, base_nueva)
summary(modelo_3)
coef(modelo_3)
paste("Coef:", mean(entrenamiento$log_salario_mensual_hora))

#Predicción:
test$modelo_3<-predict(modelo_3,newdata = test)

#MSE Modelo_3
with(test,mean((log_salario_mensual_hora-modelo_3)^2))

#####
#MSE Modelo_4

#Generamos variables al cuadrado para incluir en el modelo:
base_nueva<-base_nueva%>%
  mutate(relab_sqr=relab^2,
         tam_empresa_sqr=tam_empresa^2)


modelo_4<-lm(log_salario_mensual_hora ~ female+relab+relab_sqr+maxEducLevel+edad+edad_sqr+tam_empresa+tam_empresa_sqr, base_nueva)
summary(modelo_4)
coef(modelo_4)
paste("Coef:", mean(entrenamiento$log_salario_mensual_hora))

#Predicción:
test$modelo_4<-predict(modelo_4,newdata = test)

#MSE Modelo_1
with(test,mean((log_salario_mensual_hora-modelo_4)^2))

#####
#MSE Modelo_5

#Generamos variables al cuadrado para incluir en el modelo:
base_nueva<-base_nueva%>%
  mutate(relab_3=relab^3,
         tam_empresa_3=tam_empresa^3)


modelo_5<-lm(log_salario_mensual_hora ~ female+relab+relab_sqr+relab_3+maxEducLevel+edad+edad_sqr+tam_empresa+tam_empresa_sqr+tam_empresa_3, base_nueva)
summary(modelo_5)
coef(modelo_5)
paste("Coef:", mean(entrenamiento$log_salario_mensual_hora))

#Predicción:
test$modelo_5<-predict(modelo_5,newdata = test)

#MSE Modelo_5
with(test,mean((log_salario_mensual_hora-modelo_5)^2))

#TODOS LOS ERRORES
with(test,mean((log_salario_mensual_hora-modelo_1)^2))
with(test,mean((log_salario_mensual_hora-modelo_2)^2))
with(test,mean((log_salario_mensual_hora-modelo_3)^2))
with(test,mean((log_salario_mensual_hora-modelo_4)^2))
with(test,mean((log_salario_mensual_hora-modelo_5)^2))


#PLOT ERRORES

modelo_1<-lm(log_salario_mensual_hora ~ female+relab+maxEducLevel, base_nueva)

res_modelo_1<- resid(modelo_1,newdata = test)
plot(hist(res_modelo_1))
pred_modelo_1<-predict(modelo_1,newdata = test)

length(pred_modelo_1)
length(res_modelo_1)

##########
##LOOCV
library(caret)
set.seed(1010)
ctrl<-trainControl(method ="LOOCV")
model_lv<-train(log_salario_mensual_hora ~ female+relab+maxEducLevel, data=base_nueva, method="lm", trControl=ctrl)

print(model_lv)


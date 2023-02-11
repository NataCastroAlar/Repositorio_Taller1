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

# PREGUNTA 5 -------------- Predicting earning --------------------

# Para hacer reproducible el ejemplo
set.seed(10101)

#create ID column with a row number
base_nueva$id <- 1:nrow(base_nueva)

#usamos el 70% de la base de datos como entrenamiento y 30% como test 
entrenamiento <- base_nueva %>% dplyr::sample_frac(0.70)
test  <- dplyr::anti_join(base_nueva, entrenamiento, by = 'id')

#especificaciones previas ////

### Modelo 1
model1<-lm(log_salario_mensual_hora ~ edad + edad_2, data=entrenamiento)
test$model1<-predict(model1,newdata = test)                    # Predicción
with(test,mean((log_salario_mensual_hora-model1)^2))           #MSE del modelo 

### Modelo 2
model2<-lm(log_salario_mensual_hora ~ female, data=entrenamiento)
test$model2<-predict(model2,newdata = test)                    # Predicción
with(test,mean((log_salario_mensual_hora-model2)^2))           #MSE del modelo 

### Modelo 3
model3<-lm(log_salario_mensual_hora ~ female + edad + edad_2 + relab + maxEducLevel + tam_empresa, data=entrenamiento)
test$model3<-predict(model3,newdata = test)                    # Predicción
with(test,mean((log_salario_mensual_hora-model3)^2))           #MSE del modelo 

### nuevas especificaciones ///

### Modelo 4
model4<-lm(log_salario_mensual_hora ~ female + poly(edad, 2):female + relab:female + maxEducLevel:female + tam_empresa:female, data=entrenamiento)
test$model4<-predict(model4,newdata = test)                    # Predicción
with(test,mean((log_salario_mensual_hora-model4)^2))           #MSE del modelo 

### Modelo 5
model5<-lm(log_salario_mensual_hora ~ female + poly(edad, 2):female + poly(edad, 2):maxEducLevel +
             relab:female + maxEducLevel:female + tam_empresa:female +maxEducLevel:tam_empresa +
             relab:maxEducLevel + relab:tam_empresa, data=entrenamiento)

test$model5<-predict(model5,newdata = test)                    # Predicción
with(test,mean((log_salario_mensual_hora-model5)^2))           #MSE del modelo 

### Modelo 6
model6<-lm(log_salario_mensual_hora ~ female + poly(edad, 2):female + poly(edad, 2):maxEducLevel +
             poly(edad, 2):relab + poly(edad, 2):tam_empresa + relab:female +
             maxEducLevel:female + tam_empresa:female +maxEducLevel:tam_empresa +
             relab:maxEducLevel + relab:tam_empresa, data=entrenamiento)

test$model6<-predict(model6,newdata = test)                    # Predicción
with(test,mean((log_salario_mensual_hora-model6)^2))           #MSE del modelo 

### Modelo 7

model7<-lm(log_salario_mensual_hora ~ female + poly(edad, 3):female + poly(edad, 3):maxEducLevel +
             poly(edad, 3):relab + poly(edad, 3):tam_empresa + relab:female +
             maxEducLevel:female + tam_empresa:female +maxEducLevel:tam_empresa +
             relab:maxEducLevel + relab:tam_empresa, data=entrenamiento)

test$model7<-predict(model7,newdata = test)                    # Predicción
with(test,mean((log_salario_mensual_hora-model7)^2))           #MSE del modelo 

### Modelo 8

model8<-lm(log_salario_mensual_hora ~ edad + sqrt(edad) + female + edad:female + 
             sqrt(edad):female + edad:maxEducLevel + sqrt(edad):maxEducLevel +
             edad:relab + sqrt(edad):relab + edad:tam_empresa + sqrt(edad):tam_empresa +
             relab:female + maxEducLevel:female + tam_empresa:female +maxEducLevel:tam_empresa +
             relab:maxEducLevel + relab:tam_empresa, data=entrenamiento)

test$model8<-predict(model8,newdata = test)                    # Predicción
with(test,mean((log_salario_mensual_hora-model8)^2))           #MSE del modelo 


### Modelo 9

model9<-lm(log_salario_mensual_hora ~ female + poly(edad, 5):female + poly(edad, 5):maxEducLevel +
             poly(edad, 5):relab + poly(edad, 5):tam_empresa + relab:female +
             maxEducLevel:female + tam_empresa:female +maxEducLevel:tam_empresa +
             relab:maxEducLevel + relab:tam_empresa, data=entrenamiento)

test$model9<-predict(model9,newdata = test)                    # Predicción
with(test,mean((log_salario_mensual_hora-model9)^2))           #MSE del modelo 

### Modelo 10

model10<-lm(log_salario_mensual_hora ~ female + poly(edad, 8):female + poly(edad, 8):maxEducLevel +
              poly(edad, 8):relab + poly(edad, 8):tam_empresa + relab:female +
              maxEducLevel:female + tam_empresa:female +maxEducLevel:tam_empresa +
              relab:maxEducLevel + relab:tam_empresa, data=entrenamiento)

test$model10<-predict(model10,newdata = test)                    # Predicción
with(test,mean((log_salario_mensual_hora-model10)^2))            #MSE del modelo 


mse1<-with(test,round(mean((log_salario_mensual_hora-model1)^2),4))
mse2<-with(test,round(mean((log_salario_mensual_hora-model2)^2),4))
mse3<-with(test,round(mean((log_salario_mensual_hora-model3)^2),4))
mse4<-with(test,round(mean((log_salario_mensual_hora-model4)^2),4))
mse5<-with(test,round(mean((log_salario_mensual_hora-model5)^2),4))
mse6<-with(test,round(mean((log_salario_mensual_hora-model6)^2),4))
mse7<-with(test,round(mean((log_salario_mensual_hora-model7)^2),4))
mse8<-with(test,round(mean((log_salario_mensual_hora-model8)^2),4))
mse9<-with(test,round(mean((log_salario_mensual_hora-model9)^2),4))
mse10<-with(test,round(mean((log_salario_mensual_hora-model10)^2),4))

mse<-c(mse1,mse2,mse3,mse4,mse5,mse6,mse7,mse8,mse9,mse10)

db<-data.frame(model=factor(c("model1","model2","model3","model4","model5","model6","model7",
                              "model8","model9","model10"),ordered=TRUE),MSE=mse)
db

# 

stargazer(model8, type = "text",
          title = " Regresion con mejor error pedictivo (model8)",
          aling = TRUE,
          dep.var.labels = "Logaritmo del Salario"
)


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


############################################################
# Scrip Taller 1  ->        GRUPO 6                        #
# authors: Victor Chique    BIG DATA AND MACHINE LEARNING  #
#          Natalia Castro                                  #    
#          Victor Sanchez                                  #
############################################################

##################
##  EJERCICIO 2.##
##################

# ---------------SCRAPPING DATOS GEIH----------------------#

## Librería necesaria
require(pacman)
library(pacman)
library(stargazer)
library(rio)
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

# ---------------UNIR BASES DE DATOS---------------#

base_todo <- rbind.data.frame(tabla_datos_1, tabla_datos_2, tabla_datos_3, tabla_datos_4, tabla_datos_5, tabla_datos_6, tabla_datos_7, tabla_datos_8, tabla_datos_9, tabla_datos_10)
as.data.frame(base_todo)
base_nueva<-select(base_todo, sex, age, relab, maxEducLevel, p6870, y_salary_m_hu, p6500, p6510s1, hoursWorkUsual)

base_nueva<-na.omit(base_nueva)

# ---------------CONSTRUCCIÓN VARIABLE SALARIO-------------------#

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
  rename(tam_empresa = p6870)

#Cambio de nombre para variable edad
base_nueva<-base_nueva %>% 
  rename(edad = age)
  
#Variable edad al cuadrado
base_nueva<-base_nueva %>% 
  mutate(edad_2 = edad*edad)

# ---------------ESTADISTICAS DESCRIPTIVAS Y GRAFICOS---------------#

head(base_nueva)
glimpse(base_nueva)
skim(base_nueva)
skim(base_nueva$age)

#Graficos

#Scatter

ggplot(data = base_nueva, aes(x = edad, y = salario_mensual)) + 
  geom_point(color = "red", size = 0.5) +
  labs(x = "Edad", y = "Logaritmo Ingresos mensuales") +
  scale_y_continuous(labels = scales::dollar, trans = "log1p",
                     breaks = c(1e5, 1e6, 1e7, 1e8))

ggplot(data = base_nueva, aes(x = maxEducLevel, y = salario_mensual)) + 
  geom_point(color = "red", size = 0.5) +
  labs(x = "Educación", y = "Logaritmo de Ingresos mensuales") +
  scale_y_continuous(labels = scales::dollar, trans = "log1p",
                     breaks = c(1e5, 1e6, 1e7, 1e8))

#Distribución horas de trabajo con "Histogram"

ggplot(data = base_nueva, aes(x = hoursWorkUsual)) +
  geom_histogram() +
  labs(x = "Total Horas Trabajadas por semana",
       y = "No Individuos",
       title = "Distribución de horas trabajadas") +
  theme_minimal()

#Distribución horas de trabajo cpn "boxplot"

ggplot(data = base_nueva, aes(x = hoursWorkUsual, y = as.factor(sex), 
                              color = as.factor(sex))) +
  geom_boxplot() +
  theme_minimal() +
  scale_color_manual(values = c("0" ="#219ebc","1" = "#ffafcc"), 
                     label = c("0" = "Hombre", "1" = "Mujer"),
                     name = "Sexo") +
  labs(x = "Total Horas Trabajadas por semana",
       y = "Sexo",
       title = "Distribución de horas trabajadas por sexo ")


#Distribución de las variables

ggplot(base_nueva, aes(x = edad)) +
  geom_histogram ()

#Estadisticas descriptivas

stargazer(base_nueva, header = FALSE, type="text", title="Variables incluidas en la base de Datos")

stargazer(base_nueva, header = FALSE, type = "text", 
          title = "Estadisticas Descriptivas", digits = 1,
          covariate.labels = c("Salario", "Edad"),
          summary.stat = c("n", "min", "p25", "median", "mean","p75", "sd", "max"))

as.data.frame(base_nueva)
bd<-select(base_todo, sex, age, relab, maxEducLevel, p6870, y_salary_m_hu, p6500, p6510s1, hoursWorkUsual)

bd<-na.omit(bd)


#Cálculo del salario:
bd<-bd %>%
  mutate(salario_mensual=p6500+p6510s1)%>%
  mutate(salario_mensual_hora=salario_mensual/(hoursWorkUsual*4))

bd<-select(bd, sex, edad, relab, maxEducLevel, hoursWorkUsual, salario_mensual, salario_mensual_hora, log_salario_mensual_hora)

bd<-na.omit(bd)


#Logaritmo del salario
bd<-bd %>%
  mutate(log_salario_mensual_hora=log(salario_mensual_hora))

#Variable mujer. Cambio de Sex a female con female=1
bd<-bd %>%
  mutate(female = if_else(sex == 0, 1, 0) )

#En edad sólo mayores de 18 años
bd<-bd%>%
  filter(age>=18)

#Cambio de nombre para variable p6870: total personas de empresa donde labora:
bd<-bd %>% 
  rename(tam_empresa = p6870)

#Cambio de nombre para variable edad
bd<-bd %>% 
  rename(edad = age)

#Variable edad al cuadrado
bd<-bd %>% 
  mutate(edad_2 = edad*edad)

stargazer(bd, header = FALSE, type="text", title="Variables incluidas en la base de Datos")

##################
##  EJERCICIO 3.##
##################

#Analisis descriptivo y gráfico

summary(base_nueva$salario_mensual_hora)

stargazer(base_nueva, type = "text", digits = 1)

stargazer(base_nueva[c("salario_mensual_hora", "edad")], header = FALSE, type = "text", 
          title = "Estadisticas Descriptivas", digits = 1,
          covariate.labels = c("Salario", "Edad"),
          summary.stat = c("n", "min", "p25", "median", "mean","p75", "sd", "max")
)

g1 <- ggplot(data = base_nueva, mapping = aes(x = edad, y= log_salario_mensual_hora))+
  geom_point(col = "#A80000" , size = 0.9)+
  labs(x = "edad",
       y = "logaritmo del salario por hora",
       title = "Edad y Salario")+
  theme_bw() +
  theme(plot.title = element_text(hjust = 0.5))

g1

g2<- ggplot(data = base_nueva)+
  geom_histogram(mapping = aes(x=log_salario_mensual_hora),col= "#A80000", fill = "#A80000")+
  labs(x="logaritmo del salario",
       y="frecuencia",
       title = "Distribucion del salario")+
  theme_bw() +
  theme(plot.title = element_text(hjust = 0.5))

g2

g3 <- ggplot(data = base_nueva)+
  geom_boxplot(mapping = aes(x=salario_mensual_hora), fill = "#A80000", alpha =0.5)+
  labs(x="salario",
       title = "Box Plot")+
  theme_bw() +
  theme(plot.title = element_text(hjust = 0.5))

g3

g4 <- ggplot(data = base_nueva)+
  geom_boxplot(mapping = aes(x=log_salario_mensual_hora), fill = "#A80000", alpha =0.5)+
  labs(x="logaritmo del salario",
       title = "Box Plot")+
  theme_bw() +
  theme(plot.title = element_text(hjust = 0.5))

g4

grid.arrange(g3,g4, nrow = 1, widths = c(1,1.5))

quantile(x=base_nueva$salario_mensual_hora)

IQR(x=base_nueva$salario_mensual_hora)

# i) Regresion 

reglsalario_mes_hora <- lm(log_salario_mensual_hora ~ edad + edad_2, data = base_nueva)
reglsalario_mes_hora

stargazer(reglsalario_mes_hora, type = "text",
          title = "Resultados de la Regresion",
          aling = TRUE, digits=7,
          dep.var.labels = "Logaritmo del Salario",
          covariate.labels = c("edad", "edad al cuadrado")
)


base_nueva$res_log_salario_mensual_hora <- reglsalario_mes_hora$residuals

base_nueva$log_salario_mensual_hora_hat = predict(reglsalario_mes_hora)

# iii) Model's in sample fit

mean(base_nueva$res_log_salario_mensual_hora)

ggplot(data=base_nueva, mapping = aes(x = log_salario_mensual_hora_hat, y = res_log_salario_mensual_hora))+
  geom_point(col = "#A80000" , size = 0.9)+
  geom_hline(yintercept=0, linetype="dashed")+
  geom_smooth()+
  labs(x = "Fitted values",
       y = "Residuals",
       title = "Residual Plot for Quadratic Fit")+
  theme_bw() +
  theme(plot.title = element_text(hjust = 0.5))

qqnorm(base_nueva$res_log_salario_mensual_hora)

# iv) plot of the estimated edad-earning profile

summ = base_nueva %>%
  group_by(
    edad, edad_2
  ) %>%
  summarize(
    mean_log_salario_mensual_hora = mean(log_salario_mensual_hora),
    log_salario_mensual_hora_hat_reg = mean(log_salario_mensual_hora_hat), .groups = "drop"
  )

ggplot(summ) +
  geom_point(
    aes(x = edad, y = mean_log_salario_mensual_hora),
    color = "#A80000", size = 2
  ) +
  geom_line(
    aes(x = edad, y = log_salario_mensual_hora_hat_reg),
    color = "#003399", size = 1
  ) +
  labs(
    title = "Logaritmo del Salario por Edad",
    x = "edad",
    y = "logaritmo del salario"
  ) +
  theme_bw() +
  theme(plot.title = element_text(hjust = 0.5))

### Bootstrap de "peak edad"

reglsalario_mes_hora <- lm(log_salario_mensual_hora ~ edad + edad_2, data = base_nueva)

# i) Obtenemos los coeficientes de la regresion

coefs <- reglsalario_mes_hora$coefficients
coefs

# ii) Extrayendo los coeficientes como escalar

b0 <- coefs[1]
b1 <- coefs[2]
b2 <- coefs[3]

# Calculo del "peak edad"

peak_edad <- (-b1/(2*b2))
peak_edad

# Calculo de errores estandar

eta_reglsalario_mes_hora_fn <- function(data, index){
  coefs <- lm(log_salario_mensual_hora ~ edad + edad_2, data, subset = index)$coefficients
  b1 <- coefs[2]
  b2 <- coefs[3]
  peak_edad <- (-b1/(2*b2))
  return(peak_edad)
}

eta_reglsalario_mes_hora_fn(base_nueva,1:nrow(base_nueva))

# Implementamos boot

set.seed(123)

resultados <- boot(base_nueva, eta_reglsalario_mes_hora_fn, R=10000)
resultados

# Intervalo de confianza

boot.ci(resultados, type = c("norm", "basic"))


hist(resultados$t)
qqnorm(resultados$t, datax = T)

##################
##  EJERCICIO 4.##
##################

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
lm(log_salario_mensual_hora~female+relab+maxEducLevel+edad+edad_2+tam_empresa,base_nueva)
fwl_bootstrap_log_salario(base_nueva,1:nrow(base_nueva))
#implemento Bootstrap
boot(base_nueva, fwl_bootstrap_log_salario, R = 1000)


###------------------Age-Wage profile-----------------
base_nueva<-base_nueva%>%
  mutate(mean_log_salario=mean(log_salario_mensual_hora))

base_nueva<-base_nueva%>%
  mutate(mean_pred_log_salario=mean(pred_reg_2))


#Gráfico Salario estimado Mujeres Hombres

base_mujer<-base_nueva%>%
  filter(female==1)

base_hombre<-base_nueva%>%
  filter(female==0)

reg_mujer<-lm(log_salario_mensual_hora ~ female+relab+maxEducLevel+edad+edad_2+tam_empresa, base_mujer)
base_mujer<- base_mujer%>%
  mutate(pred_mujer=predict(reg_mujer))


reg_hombre<-lm(log_salario_mensual_hora ~ female+relab+maxEducLevel+edad+edad_2+tam_empresa, base_hombre)
base_hombre<- base_hombre%>%
  mutate(pred_hombre=predict(reg_hombre))%>%
  rename(pred_mujer=pred_hombre) #para unirlos bajo la misma variable más adelante

data_mh1<-base_mujer%>%
  select(pred_mujer, female, relab, maxEducLevel, edad, edad_2, tam_empresa, log_salario_mensual_hora)

data_mh2<-base_hombre%>%
  select(pred_mujer, female, relab, maxEducLevel, edad, edad_2, tam_empresa, log_salario_mensual_hora)

base_hombre_mujer<-rbind(data_mh1, data_mh2)

summ_mujer_hombre = base_hombre_mujer %>%
  group_by(
    female, edad) %>% 
  summarize(
    mean_pred_mujer = mean(pred_mujer), .groups="drop"
  ) 


ggplot(summ_mujer_hombre, aes(x = edad, y = mean_pred_mujer, group=female)) +
  geom_line(aes(color=female))+
  geom_point()+
  labs(
    title = "Salario Predicho Hombres y Mujeres con variables de control",
    x = "Edad",
    y = "Salario"
  ) +
  theme_bw()


## Gráfica Salario - todo el rango de Edad

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

## Gráfica Salario - sólo personas entre 18 y 55 años
base_edad_55<-base_nueva%>%
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

##Peak age para hombres y mujeres

reg_edad<-lm(log_salario_mensual_hora ~ female+relab+maxEducLevel+edad+edad_2+tam_empresa, base_nueva)
stargazer(reg_edad, type="text", digits=5)


reg_normal<-lm(log_salario_mensual_hora ~ female+relab+maxEducLevel+edad+edad_2+tam_empresa, base_nueva)


eta_mod2_fn<-function(data,index
) {
  
  #coeficientes
  coefs<-lm(log_salario_mensual_hora ~ +relab+maxEducLevel+edad+edad_2+tam_empresa, data=base_nueva, subset = index)$coefficients
  
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


######Peak ages by gender

##----Peak age para mujeres y hombres

###Para Mujeres

eta_mujer_fn<-function(data,index
) {
  
  #coeficientes
  coefs<-lm(log_salario_mensual_hora ~ female+relab+maxEducLevel+edad+edad_2+tam_empresa, data=base_mujer, subset = index)$coefficients
  
  #coeficientes en escalares  
  b5<-coefs[5]
  b6<-coefs[6] 
  
  #calcular peak age
  peak_age<-(-b5/(2*b6))
  
  #valor peak age
  return(peak_age)
}

eta_mod2_fn(base_mujer,1:nrow(base_mujer_65)) 

boot(base_mujer, eta_mujer_fn,R=1000)

resultados<-boot(base_mujer, eta_mod2_fn,R=1000)
resultados

###Para Hombres


eta_hombre_fn<-function(data,index
) {
  
  #coeficientes
  coefs<-lm(log_salario_mensual_hora ~ female+relab+maxEducLevel+edad+edad_2+tam_empresa, data=base_hombre, subset = index)$coefficients
  
  #coeficientes en escalares  
  b5<-coefs[5]
  b6<-coefs[6] 
  
  #calcular peak age
  peak_age<-(-b5/(2*b6))
  
  #valor peak age
  return(peak_age)
}

eta_mod2_fn(base_hombre,1:nrow(base_hombre)) 

boot(base_hombre, eta_hombre_fn,R=1000)

resultados<-boot(base_hombre, eta_mod2_fn,R=1000)
resultados

##################
##  EJERCICIO 5.##
##################

#-------------- Predicting earning --------------------


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
             edad:relab + sqrt(edad):relab + edad:tam_empresa + sqrt(edad):tam_empresa + relab +
             tam_empresa + maxEducLevel + tam_empresa + relab:female + maxEducLevel:female +
             tam_empresa:female +maxEducLevel:tam_empresa + relab:maxEducLevel + 
             relab:tam_empresa, data=entrenamiento)

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

db_mse<-data.frame(model=factor(c("model1","model2","model3","model4","model5","model6","model7",
                                  "model8","model9","model10"),ordered=TRUE),MSE=mse)
db_mse

# Mejor modelo

stargazer(model8, type = "text",
          title = " Regresion con mejor error predictivo (model8)",
          aling = TRUE,
          dep.var.labels = "Logaritmo del Salario"
)


#PLOT DE LOS ERRORES DE PREDICCION 

res_pred_modelo_8<- resid(model8,newdata = test)

hist(res_pred_modelo_8, main = "Distribucion de los errores de prediccion (model8)",
     xlab = "Error de prediccion")

hist(res_pred_modelo_8, main = "Distribucion de los errores de prediccion (model8)",
     xlab = "Error de prediccion", freq = FALSE)

pred_modelo_8<-predict(model8,newdata = test)

##########
##LOOCV
## Modelo 8
set.seed(1010)
folds=2000

index<-split(1:2000, 1:folds)
splt<-lapply(1:folds, function(ind) base_nueva[index[[ind]], ])
View(head(splt[[1]]))

p_load(data.table)

m8<-lapply(1:folds, function(ii) lm(log_salario_mensual_hora ~ edad + sqrt(edad) + female + edad:female + 
                                      sqrt(edad):female + edad:maxEducLevel + sqrt(edad):maxEducLevel +
                                      edad:relab + sqrt(edad):relab + edad:tam_empresa + sqrt(edad):tam_empresa + relab +
                                      tam_empresa + maxEducLevel + tam_empresa + relab:female + maxEducLevel:female +
                                      tam_empresa:female +maxEducLevel:tam_empresa + relab:maxEducLevel + 
                                      relab:tam_empresa, data = rbindlist(splt[-ii])))

##Predicción en elfold que dejé por fuera

p8<-lapply(1:folds, function(ii) data.frame(predict(m8[[ii]], newdata = rbindlist(splt[ii]))))

for (i in 1:folds) {
  colnames(p8[[i]])<-"yhat" 
  splt[[i]] <- cbind(splt[[i]], p8[[i]])
  
}

head(splt[[1]])

#Cálculo MSE de cada fold

MSE_k <- lapply(1:folds, function(ii) mean((splt[[ii]]$log_salario_mensual_hora - splt[[ii]]$yhat)^2))
MSE_k

#Calculo la media de los MSE

mean(unlist(MSE_k))

## Modelo 7

set.seed(1010)
folds=2000

index<-split(1:2000, 1:folds)
splt<-lapply(1:folds, function(ind) base_nueva[index[[ind]], ])
View(head(splt[[1]]))

p_load(data.table)

m7<-lapply(1:folds, function(ii) lm(log_salario_mensual_hora ~ female + poly(edad, 3):female + poly(edad, 3):maxEducLevel +
                                      poly(edad, 3):relab + poly(edad, 3):tam_empresa + relab:female +
                                      maxEducLevel:female + tam_empresa:female +maxEducLevel:tam_empresa +
                                      relab:maxEducLevel + relab:tam_empresa, data = rbindlist(splt[-ii])))

##Predicción en elfold que dejé por fuera

p7<-lapply(1:folds, function(ii) data.frame(predict(m7[[ii]], newdata = rbindlist(splt[ii]))))

for (i in 1:folds) {
  colnames(p7[[i]])<-"yhat" 
  splt[[i]] <- cbind(splt[[i]], p7[[i]])
  
}

head(splt[[1]])

#Cálculo MSE de cada fold

MSE_k <- lapply(1:folds, function(ii) mean((splt[[ii]]$log_salario_mensual_hora - splt[[ii]]$yhat)^2))
MSE_k

#Calculo la media de los MSE

mean(unlist(MSE_k))



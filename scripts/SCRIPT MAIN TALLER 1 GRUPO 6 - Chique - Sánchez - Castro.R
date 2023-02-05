#GRUPO 6 
#SCRAPPING DATOS GEIH DE LA PAGINA DE GITHUB DEL PROFESOR
## Librería necesaria
require(pacman)
library(pacman)

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
base_todo[is.na(base_todo) | base_todo=="-Inf"] = NA

#Cálculo del salario:
base_todo<-base_todo %>%
  mutate(salario=impa+isa+ie)

#Logaritmo del salario
base_todo<-base_todo %>%
  mutate(log_salario=log(salario))

#Variable mujer. Cambio de Sex a female con female=1
base_todo<-base_todo %>%
  mutate(female = if_else(sex == 0, 1, 0) )


#PROBLEMA 4
##a. Estimación incondicional logaritmo salario vs género
reg_1<-lm(log_salario ~ sex, data=base_todo)

stargazer(reg_1, type="text")

#b.i. EQUAL PAY FOR EQUAL JOB - TEOREMA FWL  
#Modelo lineal ln Salario con variables dependientes sexo y categoría de trabajo
reg_2<-lm(log_salario~mujer+relab+MaxEducLevel, base_todo)
stargazer(reg_2, type="text")

#PASO 1: Cálculo de los residuales de la regresión en X2=relab y MaxEducLevel
reg_3<-lm(log_salario~relab+MaxEducLevel, base_todo)
stargazer(reg_3, type="text")

#PASO 2
base_todo<-base_todo%>%
  mutate(reg_2_resid=reg_2$residuals)

#PASO 2: Cálculo de los residuales sólo con relab y MaxEducLevel
reg_3<-lm(log_salario~relab+MaxEducLevel, base_todo)
stargazer(reg_3, type="text")

base_todo_res<-base_todo%>%
  mutate(reg_3_resid=reg_3$residuals)

#PASO 3: Regresión de residuales del paso 2 sobre los del paso1
reg_4<-lm(reg_3_resid~reg_2_resid, base_todo)

#COMPARACION CON LA REGRESION INICIAL
stargazer(reg_4, reg_2, type="text")

#b.ii. EQUAL PAY FOR EQUAL JOB - TEOREMA FWL - CON BOOTSTRAP


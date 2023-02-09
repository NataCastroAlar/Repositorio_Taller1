#GRUPO 6 
#SCRAPPING DATOS GEIH DE LA PAGINA DE GITHUB DEL PROFESOR
## Librería necesaria
require(pacman)
library(pacman)

p_load(tidyverse,rvest,fixest,stargazer,knitr,skimr,boot,grid,gridExtra) 

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

# Cálculo del salario por hora

base_todo<-base_todo %>% 
  mutate(salario = (p6500+p6510s1)/(hoursWorkUsual*4)) 

summary(base_todo$salario)

#Variable mujer. Cambio de Sex a female con female=1
base_todo<-base_todo %>%
  mutate(female = if_else(sex == 0, 1, 0) )


# PREGUNTA 3 -------------- Age-wage profile --------------------

# Reestructurando la base de datos para la pregunta 3

df3<-base_todo %>%
  select(salario,age)

# Limpiando la base de datos

df3 <- df3 %>%
  drop_na()

# Base de datos de personas mayores de 18 annos y sin salarios cercanos a cero

df3 <- df3 %>%
  filter(age >= 18, salario >= 1)

# Transformando variables

df3 <- df3 %>%
  mutate(age2 = age^2, lnw =log(salario))

head(df3)

# Analisis descriptivo y grafico

summary(df3$salario)

stargazer(df3, type = "text", digits = 1)

stargazer(df3[c("salario", "age")], header = FALSE, type = "text", 
          title = "Estadisticas Descriptivas", digits = 1,
          covariate.labels = c("Salario", "Edad"),
          summary.stat = c("n", "min", "p25", "median", "mean","p75", "sd", "max")
)

g1 <- ggplot(data = df3, mapping = aes(x = age, y= lnw))+
  geom_point(col = "#A80000" , size = 0.9)+
  labs(x = "edad",
       y = "logaritmo del salario por hora",
       title = "Edad y Salario")+
  theme_bw() +
  theme(plot.title = element_text(hjust = 0.5))

g1

g2<- ggplot(data = df3)+
  geom_histogram(mapping = aes(x=lnw),col= "#A80000", fill = "#A80000")+
  labs(x="logaritmo del salario",
       y="frecuencia",
       title = "Distribucion del salario")+
  theme_bw() +
  theme(plot.title = element_text(hjust = 0.5))

g2

g3 <- ggplot(data = df3)+
  geom_boxplot(mapping = aes(x=salario), fill = "#A80000", alpha =0.5)+
  labs(x="salario",
       title = "Box Plot")+
  theme_bw() +
  theme(plot.title = element_text(hjust = 0.5))

g3

g4 <- ggplot(data = df3)+
  geom_boxplot(mapping = aes(x=lnw), fill = "#A80000", alpha =0.5)+
  labs(x="logaritmo del salario",
       title = "Box Plot")+
  theme_bw() +
  theme(plot.title = element_text(hjust = 0.5))

g4

grid.arrange(g3,g4, nrow = 1, widths = c(1,1.5))

quantile(x=df3$salario)

IQR(x=df3$salario)

# i) Regresion 

reglnw <- lm(lnw ~ age + age2, data = df3)
reglnw

stargazer(reglnw, type = "text",
          title = "Resultados de la Regresion",
          aling = TRUE, digits=7,
          dep.var.labels = "Logaritmo del Salario",
          covariate.labels = c("edad", "edad al cuadrado")
)


df3$res_lnw <- reglnw$residuals

df3$lnw_hat = predict(reglnw)

# iii) Model's in sample fit

mean(df3$res_lnw)

ggplot(data=df3, mapping = aes(x = lnw_hat, y = res_lnw))+
  geom_point(col = "#A80000" , size = 0.9)+
  geom_hline(yintercept=0, linetype="dashed")+
  geom_smooth()+
  labs(x = "Fitted values",
       y = "Residuals",
       title = "Residual Plot for Quadratic Fit")+
  theme_bw() +
  theme(plot.title = element_text(hjust = 0.5))

qqnorm(df3$res_lnw)

# iv) plot of the estimated age-earning profile

summ = df3 %>%
  group_by(
    age, age2
  ) %>%
  summarize(
    mean_lnw = mean(lnw),
    lnw_hat_reg = mean(lnw_hat), .groups = "drop"
  )

ggplot(summ) +
  geom_point(
    aes(x = age, y = mean_lnw),
    color = "#A80000", size = 2
  ) +
  geom_line(
    aes(x = age, y = lnw_hat_reg),
    color = "#003399", size = 1
  ) +
  labs(
    title = "Logaritmo del Salario por Edad",
    x = "edad",
    y = "logaritmo del salario"
  ) +
  theme_bw() +
  theme(plot.title = element_text(hjust = 0.5))

### Bootstrap de "peak age"

reglnw <- lm(lnw ~ age + age2, data = df3)

# i) Obtenemos los coeficientes de la regresion

coefs <- reglnw$coefficients
coefs

# ii) Extrayendo los coeficientes como escalar

b0 <- coefs[1]
b1 <- coefs[2]
b2 <- coefs[3]

# Calculo del "peak age"

peak_age <- (-b1/(2*b2))
peak_age

# Calculo de errores estandar

eta_reglnw_fn <- function(data, index){
  coefs <- lm(lnw ~ age + age2, data, subset = index)$coefficients
  b1 <- coefs[2]
  b2 <- coefs[3]
  peak_age <- (-b1/(2*b2))
  return(peak_age)
}

eta_reglnw_fn(df3,1:nrow(df3))

# Implementamos boot

set.seed(123)

resultados <- boot(df3, eta_reglnw_fn, R=10000)
resultados

# Intervalo de confianza

boot.ci(resultados, type = c("norm", "basic"))

names(resultados)
hist(resultados$t)
qqnorm(resultados$t, datax = T)

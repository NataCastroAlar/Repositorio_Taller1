
# Taller 1, 
# Calculando la variable salarios

# Limpiar todo

rm(list = ls())

# Librerias

library(pacman)

p_load(tidyverse,rvest,fixest,stargazer,knitr,skimr,boot,grid,gridExtra)

# Importar datos

base_todo <- read.csv("C:/Users/User/Desktop/Dulio/Big Data y Machine Learning/taller 1/base_todo.csv")

#head(base_todo)
#skim(base_todo)

# Generar variable salario por hora

summary(base_todo$impa)

summary(base_todo$hoursWorkUsual)
summary(base_todo$hoursWorkActualSecondJob)

base_todo<-base_todo %>% mutate(salario1 = (p6500+p6510s1)/(hoursWorkUsual*4))
summary(base_todo$salario1)


base_todo<-base_todo %>% mutate(salario2 = impa/(hoursWorkUsual*4))
summary(base_todo$salario2)

summary(base_todo$y_salary_m_hu)

# Scatter plot de toda la base de las tres variables de salario

g1 <- ggplot(data = base_todo, mapping = aes(x = age, y = log(salario1)))+
  geom_point(col = "#A80000" , size = 0.9)+
  labs(x = "edad",
       y = "ln salario1 por hora",
       title = "Edad y Salario")+
  theme_bw() +
  theme(plot.title = element_text(hjust = 0.5))

g3 <- ggplot(data = base_todo, mapping = aes(x = age, y = log(salario2)))+
  geom_point(col = "#A80000" , size = 0.9)+
  labs(x = "edad",
       y = "ln salario2 por hora",
       title = "Edad y Salario")+
  theme_bw() +
  theme(plot.title = element_text(hjust = 0.5))

g5 <- ggplot(data = base_todo, mapping = aes(x = age, y = log(y_salary_m_hu)))+
  geom_point(col = "#A80000" , size = 0.9)+
  labs(x = "edad",
       y = "ln salario3 por hora",
       title = "Edad y Salario")+
  theme_bw() +
  theme(plot.title = element_text(hjust = 0.5))

# Reestructurando la base de datos 

df3<-base_todo %>%
  select(y_salary_m_hu,salario1,salario2,age)

summary(base_todo$salario1)
summary(base_todo$salario2)
summary(base_todo$y_salary_m_hu)

# Limpiando la base de datos

df3 <- df3 %>%
  drop_na() %>%
  filter(age >= 18, salario1 >= 1) %>%
  mutate(age2 = age^2, lsalary =log(y_salary_m_hu), lsala1 = log(salario1), lsala2 = log(salario2))

summary(df3$salario1)
summary(df3$salario2)
summary(df3$y_salary_m_hu)

### Graficos

g2 <- ggplot(data = df3, mapping = aes(x = age, y = lsala1))+
  geom_point(col = "#A80000" , size = 0.9)+
  labs(x = "edad",
       y = "ln salario1 por hora",
       title = "Edad y Salario")+
  theme_bw() +
  theme(plot.title = element_text(hjust = 0.5))

g4 <- ggplot(data = df3, mapping = aes(x = age, y = lsala2))+
  geom_point(col = "#A80000" , size = 0.9)+
  labs(x = "edad",
       y = "ln salario2 por hora",
       title = "Edad y Salario")+
  theme_bw() +
  theme(plot.title = element_text(hjust = 0.5))

g6 <- ggplot(data = df3, mapping = aes(x = age, y = lsalary))+
  geom_point(col = "#A80000" , size = 0.9)+
  labs(x = "edad",
       y = "ln salario3 por hora",
       title = "Edad y Salario")+
  theme_bw() +
  theme(plot.title = element_text(hjust = 0.5))

# Multiples graficos

grid.arrange(g1,g2,g3,g4,g5,g6, nrow = 3, widths = c(1,1.5))

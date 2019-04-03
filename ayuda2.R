# (1) Define the data building blocks required for plotting purposes [uses
# a subset of the OAC results plotted above]

var.names <- c("All Flats", "No central heating", "Rooms per\nhousehold", "People per room", 
               "HE Qualification", "Routine/Semi-Routine\nOccupation", "2+ Car household", 
               "Public Transport\nto work", "Work from home")
var.order = seq(1:9)
values.a <- c(-0.1145725, -0.1824095, -0.01153078, -0.0202474, 0.05138737, -0.1557234, 
              0.1099018, -0.05310315, 0.0182626)
values.b <- c(0.2808439, -0.2936949, -0.1925846, 0.08910815, -0.03468011, 0.07385727, 
              -0.07228813, 0.1501105, -0.06800127)
values.c <- rep(0, 9)
group.names <- c("Blue Collar Communities", "Prospering Suburbs", "National Average")


# (2) Create df1: a plotting data frame in the format required for ggplot2

df1.a <- data.frame(matrix(c(rep(group.names[1], 9), var.names), nrow = 9, ncol = 2), 
                    var.order = var.order, value = values.a)
df1.b <- data.frame(matrix(c(rep(group.names[2], 9), var.names), nrow = 9, ncol = 2), 
                    var.order = var.order, value = values.b)
df1.c <- data.frame(matrix(c(rep(group.names[3], 9), var.names), nrow = 9, ncol = 2), 
                    var.order = var.order, value = values.c)
df1 <- rbind(df1.a, df1.b, df1.c)
colnames(df1) <- c("group", "variable.name", "variable.order", "variable.value")
df1


# (3) Create a radial plot using ggplot2
library(ggplot2)
library(reshape2)
ggplot(df1, aes(y = variable.value, x = reorder(variable.name, variable.order), 
                group = group, colour = group)) + coord_polar() + geom_point() + geom_path() + 
  labs(x = NULL)


# (4) Create df2: a plotting data frame in the format required for
# funcRadialPlot

m2 <- matrix(c(values.a, values.b), nrow = 2, ncol = 9, byrow = TRUE)
group.names <- c(group.names[1:2])
df2 <- data.frame(group = group.names, m2)
colnames(df2)[2:10] <- var.names
print(df2)
View(df2)


df2 = df2 %>% filter(group == "Prospering Suburbs")

source("multiplot_ranking_others.R")


CreateRadialPlot(df2, plot.extent.x = 1.5)


dd = dcast(df1, group~variable.name, value.va= variable.value)
dd2 = dd %>% filter(group == "Prospering Suburbs")
CreateRadialPlot(dd, plot.extent.x = 1.5)
CreateRadialPlot(dd2, plot.extent.x = 1.5)


####
tablita <- ldply(2:ncol(dat_ranking), calcular_ranking, dd=dat_ranking, IF="948 PROAKTIVA")
colnames(tablita) <- c("Variable", "Posicion", "Posicion_rel", "Etiqueta")
tablita$IF <- "948 PROAKTIVA"
tablita_2 = dcast(tablita, IF~Variable, value.var = "Posicion_rel")
tablita_2$IF = as.factor(tablita_2$IF)
CreateRadialPlot(tablita_2, plot.extent.x = 100)

View(dd)












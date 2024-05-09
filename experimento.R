### Librerías

library(readr)
library(fitdistrplus)
library(vcd)
library(ggplot2)

### Importación de datos

datos <- read_delim("https://raw.githubusercontent.com/auramolina/Confiabilidad/main/datos%20experimento.csv",delim = ";", escape_double = FALSE, trim_ws = TRUE)

cuerda3 <- datos[, 2]
cuerda4 <- datos[, 3]

boxplot(cuerda3, main = "Cuerda 3")
boxplot(cuerda4, main = "Cuerda 4")

    #hist(cuerda3, main = "Cuerda 3")
    #hist(cuerda4, main = "Cuerda 4")

ggplot(datos, aes(x = datos$`Cuerda 3`)) + 
  geom_histogram(aes(y = ..density..),colour = 1, fill = "lightblue2")+
  labs(title = "Cuerda 3", x = "", y = "Densidad")+
  geom_density()

ggplot(datos, aes(x = datos$`Cuerda 4`)) + 
  geom_histogram(aes(y = ..density..),colour = 1, fill = "lightblue2")+
  labs(title = "Cuerda 4", x = "", y = "Densidad")+
  geom_density()


### Ajuste de distribución 

# Poisson ~ NO

fitdistr(datos$`Cuerda 3`, "poisson")
ks.test(datos$`Cuerda 3`, "ppois", 22.725)
summary(goodfit(datos$`Cuerda 3`, "poisson"))

fitdistr(datos$`Cuerda 4`, "poisson")
ks.test(datos$`Cuerda 4`, "ppois", 27.2875)
summary(goodfit(datos$`Cuerda 4`, "poisson"))

# Geométrica ~ NO

fitdistr(datos$`Cuerda 3`, "geometric")
ks.test(datos$`Cuerda 3`, "pgeom", 0.042149631)

fitdistr(datos$`Cuerda 4`, "geometric")
ks.test(datos$`Cuerda 4`, "pgeom", 0.035351304)

# Negative binomial ~ SÍ

fitdistr(datos$`Cuerda 3`, "negative binomial")
ks.test(datos$`Cuerda 3`, "pnbinom", size = 3.3201871, mu = 22.725)
summary(goodfit(datos$`Cuerda 3`,type= "nbinomial",method= "MinChisq"))

fitdistr(datos$`Cuerda 4`, "negative binomial")
ks.test(datos$`Cuerda 4`, "pnbinom", size = 3.1573296, mu = 27.2875)
summary(goodfit(datos$`Cuerda 4`,type= "nbinomial",method= "MinChisq"))

### Funciones asociadas a la confiabiliadad

fdp3 <- dnbinom(datos$`Cuerda 3`, size = 3.3201871, mu = 22.725, log = FALSE)
fdp3_80 <- dnbinom(80, size = 3.3201871, mu = 22.725, log = FALSE)

fdp4 <- dnbinom(datos$`Cuerda 4`, size = 3.1573296, mu = 27.2875, log = FALSE)
fdp4_80 <- dnbinom(80, size = 3.1573296, mu = 27.2875, log = FALSE)

# Probabilidad de falla

Ft3 <- pnbinom(datos$`Cuerda 3`, size = 3.3201871, mu = 22.725, log = FALSE)
ggplot(datos, aes(x = `Cuerda 3`, y = Ft3)) +
  geom_line(color = "lightblue2", size = 1) +
  labs(title = "Probabilidad de falla - Cuerda 3")+
  theme_bw()
F3_80 <- pnbinom(80, size = 3.3201871, mu = 22.725, log = FALSE)

Ft4 <- pnbinom(datos$`Cuerda 4`, size = 3.1573296, mu = 27.2875, log = FALSE)
ggplot(datos, aes(x = `Cuerda 4`, y = Ft4)) +
  geom_line(color = "lightblue2", size = 1) +
  labs(title = "Probabilidad de falla - Cuerda 4")+
  theme_bw()
Ft4_80 <- pnbinom(80, size = 3.1573296, mu = 27.2875, log = FALSE)

#Confiabilidad

Rt3 <- 1 - Ft3 
ggplot(datos, aes(x = `Cuerda 3`, y = Rt3)) +
  geom_line(color = "lightblue2", size = 1) +
  labs(title = "Confiabilidad - Cuerda 3") +
  theme_bw()
Rt3_80 <- 1 - Ft3_80

Rt4 <- 1 - Ft4
ggplot(datos, aes(x = `Cuerda 4`, y = Rt4)) +
  geom_line(color = "lightblue2", size = 1) +
  labs(title = "Confiabilidad - Cuerda 4") +
  theme_bw()
Rt4_80 <- 1 - Ft4_80

#Tasa de falla

h3 <- fdp3/Rt3
ggplot(datos, aes(x = `Cuerda 3`, y = h3)) +
  geom_line(color = "lightblue2", size = 1) +
  labs(title = "Tasa de falla - Cuerda 3") +
  theme_bw()
h3_80 <- fdp3_80/Rt3_80

h4 <- fdp4/Rt4
ggplot(datos, aes(x = `Cuerda 4`, y = h4)) +
  geom_line(color = "lightblue2", size = 1) +
  labs(title = "Tasa de falla - Cuerda 4") +
  theme_bw()
h4_80 <- fdp4_80/Rt4_80




library(devtools)
library(tidyverse)
library(knitr)
library(shiny)
library(data.table)
library(ggplot2)

sp1=fread("Datafile/ex_spectrum1.csv")
sp2=fread("Datafile/ex_spectrum2.csv")

sp1

ggplot()+
  geom_point(data=sp1, aes(`Raman shift`, Intensity), col="red")+
  #geom_line(data=sp1, aes(`Raman shift`, Intensity), col="red")+
  geom_point(data=sp2, aes(`Raman shift`, Intensity), col="blue")
  #geom_line(data=sp1, aes(`Raman shift`, Intensity), col="red")


library(broom)
library(plotly)

##final=================
source("myfunc.R") # for the Pruby() function
Lor <- function(x, x0=0, FWHM=1){
  2/(pi*FWHM)/( 1 + ((x-x0)/(FWHM/2))^2 )
}

flist <- list.files(path="Data", pattern = "rubis")
# Load all data files and do the fits in a single pipe flow
data <- tibble(name = flist) %>%
  mutate(data = map(name,
                    ~read_table2(file      = file.path("Data", .),
                                 col_names = c("w", "Int"))),
         fit = map(data, 
                   ~ nls(data = .,
                         Int ~ y0 + A1*Lor(w,x1,FWHM1) + A2*Lor(w,x2,FWHM2),
                         start=list(y0    = 0.01,
                                    x1    = .$w[which.max(.$Int)] - 2,
                                    x2    = .$w[which.max(.$Int)] - 30,
                                    FWHM1 = 10, FWHM2 = 10,
                                    A1    = max(.$Int)*10,
                                    A2    = max(.$Int)*10))),
         tidied = map(fit, tidy),
         augmented = map(fit, augment),
         P = map_dbl(tidied, ~round(Pruby(.$estimate[.$term=='x1']),2))
  ) %>% 
  separate(name, c("sample","run", NA), convert=TRUE) %>% 
  relocate(P, .after = run)
data
###final end=================



### prac https://lmi.cnrs.fr/r/fitting.html#linear-fitting-with-lm=====

x  <- seq(-5,7,.1)
y  <- dnorm(x, sd = .5) + dnorm(x, mean=2, sd = 1) + runif(length(x))/10 - 0.05
df <- tibble(x=x, y=y)
ggplot(data=df, aes(x,y))+
  geom_point()+
  ggtitle("Some fake data we want to fit with 2 Gaussians")



myfunc <- function(x, y0, x0, A, B) {
  y0 + dnorm(x, sd=A) + dnorm(x, mean=x0, sd=B)
}
##y0: start point
##x:peak position1, considered peoak located at x=0 
##A:gaussian standard distribution (sd) at peak 1
##x0:peak poisition2 at xo 
##B:gaussian standard distribution (sd) at peak 2


# Fit the data using a user function
fit_nls <- nls(data=df,
               y ~ myfunc(x, y0, x0, A, B),
               start=list(y0=0, x0=1.5, A=.2, B=.2) # provide starting point
)
summary(fit_nls)

coef(fit_nls)

plot(x, y, pch=16)
lines(x, predict(fit_nls), col="red", lwd=2)

P1 <- ggplot(data=df, aes(x,y))+
  ggtitle("Retrieving the fit performed beforehand")+
  geom_point(size=2, alpha=.5) +
  geom_line(aes(y=predict(fit_nls)), color="red", size=1)

P2 <- ggplot(data=df, aes(x,y))+
  ggtitle("Doing the fit directly withing ggplot2")+
  geom_point(size=2, alpha=.5) +
  geom_smooth(method = "nls", 
              method.args = list(formula = y ~ myfunc(x, y0, x0, A, B),
                                 start=list(y0=0, x0=1.5, A=.2, B=.2)
              ), 
              data = df,
              se = FALSE,
              color="red")
P1
P2


fit_constr <- nls(data = df,
                  y ~ myfunc(x, y0, x0, A, B),
                  start = list(y0=0, x0=5, A=.2, B=.2),
                  upper = list(y0=Inf, x0=Inf, A=.4, B=1),
                  lower = list(y0=-Inf, x0=4, A=-Inf, B=-Inf),
                  algorithm = "port"
)
##forced linear regression peak located at x=0 & x=5 

# Plotting the resulting function in blue
ggplot(data=df, aes(x,y))+
  ggtitle("Beware of bad constraints!")+
  geom_point(size=2, alpha=.5) +
  geom_line(aes(y = predict(fit_constr)), color="royalblue", size=1)

##** nls can't affordable with large sd 
install.packages("minpack.lm")
library(minpack.lm)

fit_nlsLM <- nlsLM(data = df,
                   y ~ myfunc(x, y0, x0, A, B),
                   start = list(y0=0, x0=5, A=1, B=1)
)

summary(fit_nls)


###prac===
library(broom)
library(tidyverse)
library(ggplot2)
theme_set(theme_bw())

# Create fake data
a <- seq(-10,10,.1)
centers <- c(-2*pi,pi,pi/6)
widths  <- runif(3, min=0.5, max=1)
amp     <- runif(3, min=2, max=10)
noise   <- .3*runif(length(a))-.15
d <- tibble(x=rep(a,3),
            y=c(amp[1]*dnorm(a,mean=centers[1],sd=widths[1]) + sample(noise),
                amp[2]*dnorm(a,mean=centers[2],sd=widths[2]) + sample(noise),
                amp[3]*dnorm(a,mean=centers[3],sd=widths[3]) + sample(noise)),
            T=rep(1:3, each=length(a))
)
d
# Plot the data

d %>% ggplot(aes(x=x, y=y, color=factor(T))) + 
  geom_line()


d_fitted <- d %>% 
  nest(data = -T) %>%
  mutate(fit = purrr::map(data, ~ nls(data = .,
                                      y ~ y0 + A*dnorm(x, mean=x0, sd=FW), 
                                      start=list(A  = max(.$y),
                                                 y0 = .01, 
                                                 x0 = .$x[which.max(.$y)], 
                                                 FW = .7)
  )),
  tidied = purrr::map(fit, tidy),
  augmented = purrr::map(fit, augment)
  )

d_fitted

d_fitted %>% 
  unnest(augmented)


d_fitted %>% 
  unnest(augmented) %>% 
  ggplot(aes(x=x, color=factor(T)))+
  geom_point(aes(y=y), alpha=0.5, size=3) + 
  geom_line(aes(y=.fitted))









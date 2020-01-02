# practice of ridge plot 
#created by Donghui Wang 

/
library(tidyverse)
library(dplyr)


#try ggridge 
library(ggplot2)
library(ggridges)
library(haven)
library(cowplot) # 1.0.0


iris
ggplot(iris, aes(x = Sepal.Length, y = Species)) +
  geom_density_ridges2() +
  scale_y_discrete(expand = c(0.01, 0)) +
  scale_x_continuous(expand = c(0.01, 0)) +
  theme_ridges()


#aggregate level data 
iris %>% group_by(Species) %>%
  do(ggplot2:::compute_density(.$Sepal.Length, NULL)) %>%
  rename(Sepal.Length = x) -> iris_densities
head(iris_densities)

ggplot(iris_densities, aes(x = Sepal.Length, y = Species, height = density)) + 
  geom_density_ridges(stat = "identity", fill= "lightblue")

#https://cran.r-project.org/web/packages/ggridges/vignettes/introduction.html


#attitude data  

scaling <- read_dta("C:/Users/donghuiw/Dropbox/Website/US_project/cleaned_data/scaling.dta")
gss_r <- read_dta("C:/Users/donghuiw/Dropbox/Website/US_project/cleaned_data/gss_r.dta")

gss_newr <- mutate(gss_r, response = resp - 5) 

ggplot(gss_newr, aes(x = response, y = as.factor(year))) +
  geom_density_ridges2(scale = 3,  fill= "lightblue",colour = "white" , alpha = 0.7)  +   theme_minimal()+
  labs(title="Liking China (GSS)")+ylab("survey year")+ xlab("response") +
  xlim(-5, 5)






scaling

gss <- filter(scaling, varname == "GSS")

gssp <- ggplot(gss, aes(x = resp, y = as.factor(syear), height = pct))+
         geom_density_ridges(stat = "identity", fill= "lightblue" , scale = 2, colour = "white" , alpha = 0.7) +
  scale_y_discrete(expand = c(0.001, 0)) +
  scale_x_continuous(expand = c(0.01, 0)) +
  theme_minimal()+
  labs(title="GSS")+ylab("survey year")


pew <- filter(scaling,varname == "PEW")

pewp <- ggplot(pew, aes(x = resp, y = as.factor(syear), height = pct))+
  geom_density_ridges2(stat = "identity", fill= "lightblue" , scale = 2, colour = "white" , alpha = 0.7) +
  scale_y_discrete(expand = c(0.001, 0)) +
  scale_x_continuous(expand = c(0.01, 0)) +
  theme_minimal()+
  labs(title="PEW")+ylab("survey year")


gallup <- filter(scaling,varname == "USGALLUP_4")

 
gallupp <- ggplot(gallup, aes(x = resp, y = as.factor(enddate), height = pct))+
  geom_density_ridges2(stat = "identity",  fill= "lightblue" , scale = 2, colour = "white" , alpha = 0.7) +
  theme_minimal()+
  labs(title="GALLUP")+ylab("survey ending date")

?geom_density_ridges2


abc <- filter(scaling,varname == "ABC")
abc <-ggplot(abc, aes(x = resp, y = as.factor(enddate), height = pct))+
  geom_density_ridges2(stat = "identity",  fill= "lightblue" , scale = 2, colour = "white" , alpha = 0.7) +
  theme_minimal()+
  labs(title="ABC")+ylab("survey ending date")


plot_grid(gssp, pewp,gallupp, ncol =  3 )  





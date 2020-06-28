library(usmap)

source('data_prep.R')

library(ggplot2)


plot_usmap(data = County_Covid_Summary, values = "DeathRatio", color='grey') + 
  scale_fill_continuous(low="white", high='black',name = "COVID19 Mortality Rate", label = scales::comma) + 
  theme(legend.position = "right") +labs(title='COVID19 Moratlity Rate againt Confirmed Cases',legend.position = "right")


plot_usmap(data = County_Covid_Summary, values = "TestingRatio", color='grey') + 
  scale_fill_continuous(low="white", high='darkgreen',name = "COVID19 Mortality Rate", label = scales::comma) + 
  theme(legend.position = "right") +labs(title='COVID19 Testing Rates',legend.position = "right")


ggplotRegression <- function (fit) {
  
  require(ggplot2)
  
  ggplot(fit$model, aes_string(x = names(fit$model)[2], y = names(fit$model)[1])) + 
    geom_point() +
    stat_smooth(method = "lm", col = "red") +
    labs(title = paste("Adj R2 = ",signif(summary(fit)$adj.r.squared, 5),
                       "Intercept =",signif(fit$coef[[1]],5 ),
                       " Slope =",signif(fit$coef[[2]], 5),
                       " P =",signif(summary(fit)$coef[2,4], 5)))
}

fit1 <- lm(DeathRatio ~ Obesity_percent, data = County_Covid_Summary)
ggplotRegression(fit1)


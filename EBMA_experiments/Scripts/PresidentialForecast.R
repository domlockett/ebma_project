library(here)
library(EBMAforecast)
library(tidyverse)
library(readr)
library(conflicted)
library(colorRamps)
library(RColorBrewer)

theme_plot <- function(...) {
  theme_minimal() +
    theme(
      text = element_text(color = "#22211d"),
      axis.line = element_blank(),
      axis.text.x = element_blank(),
      axis.text.y = element_blank(),
      axis.ticks = element_blank(),
      axis.title.x = element_blank(),
      axis.title.y = element_blank(),
      ...
    )
}


conflict_prefer("filter", "dplyr")
data <- read_csv(here("Presidential Forecasting 2020", "Percent2PartyVote .csv"))

predCal <- data %>% 
  filter(Year !=2020) %>% 
  rename(NB = `N-B`,
         EW = `Erikson and Wlezien`,
         LB = `Lewis Beck-Tien`,
         DeSart = `Jay DeSart`,
         Graefe = `Andreas Graefe`,
         BVN = `Jerome Bruno Jerome Veronique and Richard Nadeau`,
         Lichtman = `Allan Lichtman`,
         RBNF = `Thomas Rietz Joyce Berg Forrest Nelson Robert Forsythe`) %>% 
  select(-c(Year,truth))



outCal <- data %>% 
  filter(Year !=2020) %>% 
  select(truth) %>% 
  as.data.frame()

this.ForecastData <- makeForecastData(.predCalibration = as.data.frame(predCal), .outcomeCalibration = outCal[,1], .modelNames= names(predCal))

thisEnsemble <- calibrateEnsemble(this.ForecastData, model="normal", useModelParams=FALSE, tol=0.000000000000000000001)

summary(thisEnsemble)


thisEnsembleBayes <- calibrateEnsemble(this.ForecastData, method = "gibbs", 
                                       iterations = 100000,
                                       burns = 30000,             
                                       thinning = 10,
                                       predType = "posteriorMean", 
                                       model="normal", 
                                       useModelParams=FALSE, 
                                       modelPriors = rep(0.1,15))

summary(thisEnsembleBayes)




############## cross-validation for differing alpha values
############## one election left out every time
############## run models for all alpha, calculate loo


data <- read_csv(here("Presidential Forecasting 2020", "Percent2PartyVote .csv"))

data <- data %>% 
  filter(Year !=2020) %>% 
  rename(NB = `N-B`,
         EW = `Erikson and Wlezien`,
         LB = `Lewis Beck-Tien`,
         DeSart = `Jay DeSart`,
         Graefe = `Andreas Graefe`,
         BVN = `Jerome Bruno Jerome Veronique and Richard Nadeau`,
         Lichtman = `Allan Lichtman`,
         RBNF = `Thomas Rietz Joyce Berg Forrest Nelson Robert Forsythe`) 

##### we only have 7 elections
##### 
years <- data$Year

Alpha_CV <- function(year, alpha, full_data){
  insamplePred <- full_data %>% 
    dplyr::filter(Year != year) %>% 
    dplyr::select(-c(Year,truth)) %>% 
    as.data.frame()
  
  outsamplePred <- full_data %>% 
    dplyr::filter(Year == year) %>% 
    dplyr::select(-c(Year,truth)) %>% 
    as.data.frame()
  
  insampleTruth <- full_data %>% 
    dplyr::filter(Year != year) %>% 
    dplyr::select((truth)) %>% 
    as.data.frame()
  
  outsampleTruth <- full_data %>% 
    dplyr::filter(Year == year) %>% 
    dplyr::select(truth) %>% 
    as.data.frame()
  
  
  this.ForecastData <-  makeForecastData(.predCalibration = insamplePred, .outcomeCalibration = insampleTruth[,1], .modelNames= names(insamplePred), .predTest = outsamplePred, .outcomeTest = outsampleTruth[,1])
  thisEnsembleBayes <- calibrateEnsemble(this.ForecastData, method = "gibbs", 
                                         iterations = 50000, burns = 30000, thinning = 10,
                                         predType = "posteriorMean", model="normal", 
                                         useModelParams=FALSE, 
                                         modelPriors = rep(alpha,dim(insamplePred)[2]))
  if(year != 2020){
  error <- outsampleTruth[,1] - mean(thisEnsembleBayes@posteriorPredTest)
  ebma <- mean(thisEnsembleBayes@posteriorPredTest)
  }
  if(year == 2020){
    error <- NA_real_
    ebma <- NA_real_
  }
  meanWeights <- as_tibble_row(apply(thisEnsembleBayes@posteriorWeights,2, mean), .name_repair = "minimal")
  names(meanWeights) <- names(insamplePred)
  ret <- (bind_cols("OutYear" = year, "Alpha" = alpha, "Error" = error, "EBMA" = ebma, meanWeights))
  return(ret)
}

set.seed(91514)
grid <- expand_grid(years =c(data$Year,2020), alpha = c(0.01, 0.05, 0.1, 0.2, 0.25, 0.3, 0.35, 0.4, 0.5, 0.6, 0.8, 1))


CV_results <- map2_dfr(.f = Alpha_CV, .x = grid$years, .y = grid$alpha, full_data =data )

rootMSE <- function(x){
  res <- sqrt(mean(x^2))
  return(res)
  }
  
result <- CV_results %>% 
  filter(OutYear !=2020) %>% 
  group_by(Alpha) %>% 
  summarise(rmse = rootMSE(Error),
            MAE = mean(abs(Error)))



insamplePred <- full_data %>% 
  dplyr::select(-c(Year,truth)) %>% 
  as.data.frame()

insampleTruth <- full_data %>% 
  dplyr::select((truth)) %>% 
  as.data.frame()

alpha <- 0.2

this.ForecastData <-  makeForecastData(.predCalibration = insamplePred, .outcomeCalibration = insampleTruth[,1], .modelNames= names(insamplePred))
thisEnsembleBayes <- calibrateEnsemble(this.ForecastData, method = "gibbs", 
                                       iterations = 50000, burns = 30000, thinning = 10,
                                       predType = "posteriorMean", model="normal", 
                                       useModelParams=FALSE, 
                                       modelPriors = rep(alpha,dim(insamplePred)[2]))

summary(thisEnsembleBayes)


plot_data <- CV_results %>% 
  filter(OutYear == 2020) %>% 
  pivot_longer(cols = c(names(insamplePred))) %>% 
  rename(Forecast = name)

colors <- tibble(name = unique(plot_data$Forecast)) %>% 
  arrange(name) %>% 
  mutate(color = case_when(name == "Abramowitz" ~ brewer.pal(4,"Set1")[1],
                           name == "NB" ~ brewer.pal(4,"Set1")[2],
                           name == "EW" ~ brewer.pal(4,"Set1")[3],
                           name == "RBNF" ~ brewer.pal(4,"Set1")[4],
                              TRUE ~ "#bdbdbd"),
         size = case_when(name == "Abramowitz" ~ 1.5,
                          name == "NB" ~ 1.5,
                          name == "EW" ~ 1.5,
                          name == "RBNF" ~ 1.5,
                          TRUE ~ 0.75),
         alpha = case_when(name == "Abramowitz" ~ 1,
                           name == "NB" ~ 1,
                           name == "EW" ~ 1,
                           name == "RBNF" ~ 1,
                          TRUE ~ 0.75),
         Forecast = case_when(name == "Abramowitz" ~ "Abramowitz",
                              name == "NB" ~ "NB",
                              name == "EW" ~ "EW",
                              name == "RBNF" ~ "RBNF"))



p <- ggplot(plot_data, aes(Alpha, value, color = Forecast, alpha = Forecast, size = Forecast)) + geom_line() 
p <- p + scale_color_manual(name = "", labels = c("Abramowitz", "NB", "EW", "RBNF", "Other Forecasts"), values = colors$color, breaks = c("Abramowitz", "NB", "EW", "RBNF", "Cuzan")) + scale_size_manual(name = "", labels = c("Abramowitz", "NB", "EW", "RBNF", "Other Forecasts"), values = colors$size, breaks = c("Abramowitz", "NB", "EW", "RBNF", "Cuzan")) + scale_alpha_manual(name = "", labels = c("Abramowitz", "NB", "EW", "RBNF", "Other Forecasts"), values = colors$alpha, breaks = c("Abramowitz", "NB", "EW", "RBNF", "Cuzan"))
p <- p + theme_plot() + guides(color = guide_legend(title.theme = (element_text(size=15)), title.position = "top", direction = "horizontal", nrow = 2), size = guide_legend(title.theme = (element_text(size=15)), title.position = "top", direction = "horizontal", nrow = 4), alpha = guide_legend(title.theme = (element_text(size=15)), title.position = "top", direction = "horizontal", nrow = 4))
p <- p + theme(legend.key.width = unit(3,"lines"), legend.position=c(0.65, 0.65), legend.key=element_rect(colour = 'white'), legend.text=element_text(size=15), legend.title = element_text(size=15))
p <- p +labs( y="Mean Posterior Weights", x="Dirichlet Prior")
p <- p + theme(axis.text.x=element_text(size=20), axis.text.y=element_text(size=20), axis.title.x = element_text(size = 20, face = "bold"),axis.title.y = element_text(angle = 90, size = 20, face = "bold"), plot.title = element_text(size=15))
plot(p)
ggsave(here("Figures", "Shrinkage.pdf"), height = 7, width = (7*1.618))

library(here)
library(EBMAforecast)
library(tidyverse)
library(readr)
library(conflicted)
library(colorRamps)
library(RColorBrewer)
library(furrr)
library(tictoc)
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
data <- read_csv(here( "Presidential Forecasting 2020", "Percent2PartyVote .csv")) %>% 
  select(-c(Cuzan, Hibbs, Holbrook, 'N-B'))


predCal <- data %>% 
  filter(Year !=2020) %>%
  rename(EW = `Erikson and Wlezien`,
         LB = `Lewis Beck-Tien`,
         DeSart = `Jay DeSart`,
         Graefe = `Andreas Graefe Issues and Leaders`,
         BVN = `Jerome Bruno Jerome Veronique and Richard Nadeau`,
         Lichtman = `Allan Lichtman`,
         RBNF = `Thomas Rietz Joyce Berg Forrest Nelson Robert Forsythe`) %>% 
  select(-c(Year,truth))



outCal <- data %>% 
  filter(Year !=2020) %>% 
  select(truth) %>% 
  as.data.frame()

years <- data$Year

Alpha_CV <- function(year, alpha, StartingVal){
  insamplePred <- dataPres %>% 
    dplyr::filter(Year != year & Year != 2020) %>%
    dplyr::select(-c(Year,truth)) %>% 
    as.data.frame()
  
  outsamplePred <- dataPres %>% 
    dplyr::filter(Year == year) %>% 
    dplyr::select(-c(Year,truth)) %>% 
    as.data.frame()
  
  insampleTruth <- dataPres %>% 
    dplyr::filter(Year != year & Year != 2020) %>%
    dplyr::select((truth)) %>% 
    as.data.frame()
  
  outsampleTruth <- dataPres %>% 
    dplyr::filter(Year == year) %>% 
    dplyr::select(truth) %>% 
    as.data.frame()
  if(StartingVal == 1){
    initW <- c(rep(1/11,11))
  }
  if(StartingVal == 2){
    initW <- c(rep(1/3,3),rep(0,8))
  }
  if(StartingVal == 3){
    initW <- c(rep(0,5),rep(1/6,6))
  }
  this.ForecastData <-  makeForecastData(.predCalibration = insamplePred, .outcomeCalibration = insampleTruth[,1], .modelNames= names(insamplePred), .predTest = outsamplePred, .outcomeTest = outsampleTruth[,1])
  thisEnsembleBayes <- calibrateEnsemble(this.ForecastData, method = "gibbs", 
                                         iterations = 250000, burns = 40000, thinning = 4,
                                         predType = "posteriorMean", model="normal", 
                                         useModelParams=FALSE, 
                                         modelPriors = rep(alpha,dim(insamplePred)[2]), 
                                         W = initW)
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
  ret <- (bind_cols("OutYear" = year, "Alpha" = alpha, StartingValue = StartingVal, "Error" = error, "EBMA" = ebma, meanWeights))
  return(ret)
}

set.seed(91514)
grid <- expand_grid(years = data$Year, alpha = c(0.01, 0.05, 0.1, 0.2, 0.225, 0.25, 0.275, 0.3, 0.325, 0.35, 0.375, 0.4, 0.45, 0.5, 0.6, 0.8, 1), weights = c(1,2, 3))

dataPres <- data

grid_list <- list(year = grid$years, alpha = grid$alpha, StartingVal = grid$weights)


plan(multiprocess)
tic()
CV_results <- future_pmap_dfr(grid_list, .f = Alpha_CV)
toc()
save(CV_results, file = here("Results", "CV_results.rda"))



load(here("Results", "CV_results.rda"))

dim(CV_results)
dim(grid)

insamplePred <- data %>%
  dplyr::filter(Year != 2020) %>%
  dplyr::select(-c(Year,truth)) %>%
  #rename('Andreas Graefe' = 'Andreas Graefe Issues and Leaders') %>%
  as.data.frame()

conflict_prefer("lag", "dplyr")

convergence <- CV_results %>%
  pivot_longer(cols = c(names(insamplePred))) %>%
  rename(Forecast = name) %>%
  mutate(Start = case_when(StartingValue == 1 ~ "first",
                           StartingValue == 2 ~ "second",
                           StartingValue == 3 ~ "third")) %>%
  select(c(OutYear, Start, value, Forecast, Alpha)) %>%
  pivot_wider(id_cols = c(Forecast, Alpha, OutYear), names_from = Start, values_from = value) %>%
  mutate(dif1 = abs(first - second),
         dif2 = abs(first - third),
         dif3 = abs(second - third)) %>%
  mutate(mad = dif1 + dif2 + dif3) %>%
  group_by(Alpha) %>%
  summarise(meanMad = mean(mad)) ####### i think above alpha 0.1 is probably okay

### now select best



rootMSE <- function(x){
  res <- sqrt(mean(x^2))
  return(res)
  }


result <- CV_results %>%
  filter(OutYear !=2020) %>%
  group_by(Alpha) %>%
  summarise(rmse = rootMSE(Error),
            MAE = mean(abs(Error)))

##picking alpha 0.225


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


























this.ForecastData <- makeForecastData(.predCalibration = as.data.frame(predCal[,c(1,2,15)]), .outcomeCalibration = outCal[,1], .modelNames= names(predCal[,c(1,2,15)]))

em.ensemble <- calibrateEnsemble(this.ForecastData,model="normal", predType="posteriorMean",method = "EM", useModelParams=FALSE, tol = 0.0000000000000000000000000000000000000000000000000000001)

summary(em.ensemble)



thisEnsembleBayes1 <- calibrateEnsemble(this.ForecastData,model="normal", method = "gibbs",predType="posteriorMean",useModelParams=FALSE, modelPriors = rep(1,15), W = initW[1,], iterations = 50000, burns = 20000, thinning = 5)

thisEnsembleBayes2 <- calibrateEnsemble(this.ForecastData,model="normal", method = "gibbs",predType="posteriorMean",useModelParams=FALSE, modelPriors = rep(1,3), W = initW[2,], iterations = 50000, burns = 20000, thinning = 5)
thisEnsembleBayes3 <- calibrateEnsemble(this.ForecastData,model="normal", method = "gibbs",predType="posteriorMean",useModelParams=FALSE, modelPriors = rep(1,3), W = initW[3,], iterations = 50000, burns = 20000, thinning = 5)
thisEnsembleBayes4 <- calibrateEnsemble(this.ForecastData,model="normal", method = "gibbs",predType="posteriorMean",useModelParams=FALSE, modelPriors = rep(1,3), W = initW[4,], iterations = 50000, burns = 20000, thinning = 5)
     colMeans(thisEnsembleBayes1@posteriorWeights)
     colMeans(thisEnsembleBayes2@posteriorWeights)
     colMeans(thisEnsembleBayes3@posteriorWeights)
     colMeans(thisEnsembleBayes4@posteriorWeights)



     forPlot <- tibble(weights = c(thisEnsembleBayes1@posteriorWeights[,1], thisEnsembleBayes1@posteriorWeights[,2], thisEnsembleBayes1@posteriorWeights[,3]), model = c(rep("Abramowitz", dim(thisEnsembleBayes1@posteriorWeights)[1]), rep("Campbell", dim(thisEnsembleBayes1@posteriorWeights)[1]), rep("RBNF", dim(thisEnsembleBayes1@posteriorWeights)[1])))
     initW[1,]
     plot <- ggplot(data=forPlot, aes(x = weights)) 
     plot <- plot + geom_density(alpha=0.4,size=1)  + facet_wrap(vars(model))
     plot <- plot +labs( y="Density", x="posterior Weight",  title = "Run 1: Starting W = 1,0,0")
     plot <- plot + theme(axis.text.x=element_text(size=15, face = "bold"), axis.text.y=element_text(size=15, face = "bold"), axis.title.x = element_text(size = 15),axis.title.y = element_text(angle = 90, size = 15), plot.title = element_text(size=15))
     plot(plot)
     ggsave(here("Figures", "Run1_2.pdf"), height = 7, width = (7*1.618))


     forPlot <- tibble(weights = c(thisEnsembleBayes2@posteriorWeights[,1], thisEnsembleBayes2@posteriorWeights[,2], thisEnsembleBayes2@posteriorWeights[,3]), model = c(rep("Abramowitz", dim(thisEnsembleBayes2@posteriorWeights)[1]), rep("Campbell", dim(thisEnsembleBayes2@posteriorWeights)[1]), rep("RBNF", dim(thisEnsembleBayes2@posteriorWeights)[1])))
     initW[2,]

     plot <- ggplot(data=forPlot, aes(x = weights)) 
     plot <- plot + geom_density(alpha=0.4,size=1)  + facet_wrap(vars(model))
     plot <- plot +labs( y="Density", x="posterior Weight", title = "Run 2: Starting W =0.3,0.3,0.3")
     plot <- plot + theme(axis.text.x=element_text(size=15, face = "bold"), axis.text.y=element_text(size=15, face = "bold"), axis.title.x = element_text(size = 15),axis.title.y = element_text(angle = 90, size = 15), plot.title = element_text(size=15))
     plot(plot)
     ggsave(here("Figures", "Run2_2.pdf"), height = 7, width = (14))


     forPlot <- tibble(weights = c(thisEnsembleBayes3@posteriorWeights[,1], thisEnsembleBayes3@posteriorWeights[,2], thisEnsembleBayes3@posteriorWeights[,3]), model = c(rep("Abramowitz", dim(thisEnsembleBayes3@posteriorWeights)[1]), rep("Campbell", dim(thisEnsembleBayes3@posteriorWeights)[1]), rep("RBNF", dim(thisEnsembleBayes3@posteriorWeights)[1])))
     initW[3,]

     plot <- ggplot(data=forPlot, aes(x = weights)) 
     plot <- plot + geom_density(alpha=0.4,size=1)  + facet_wrap(vars(model))
     plot <- plot +labs( y="Density", x="posterior Weight", title = "Run 3: Starting W =0.5,0.5,0")
     plot <- plot + theme(axis.text.x=element_text(size=15, face = "bold"), axis.text.y=element_text(size=15, face = "bold"), axis.title.x = element_text(size = 15),axis.title.y = element_text(angle = 90, size = 15), plot.title = element_text(size=15))
     plot(plot)
     ggsave(here("Figures", "Run3_2.pdf"), height = 7, width = (14))


     forPlot <- tibble(weights = c(thisEnsembleBayes4@posteriorWeights[,1], thisEnsembleBayes4@posteriorWeights[,2], thisEnsembleBayes4@posteriorWeights[,3]), model = c(rep("Abramowitz", dim(thisEnsembleBayes4@posteriorWeights)[1]), rep("Campbell", dim(thisEnsembleBayes4@posteriorWeights)[1]), rep("RBNF", dim(thisEnsembleBayes4@posteriorWeights)[1])))
     initW[4,]
     plot <- ggplot(data=forPlot, aes(x = weights)) 
     plot <- plot + geom_density(alpha=0.4,size=1)  + facet_wrap(vars(model))
     plot <- plot +labs( y="Density", x="posterior Weight", title = "Run 4: Starting W =0,0,1")
     plot <- plot + theme(axis.text.x=element_text(size=15, face = "bold"), axis.text.y=element_text(size=15, face = "bold"), axis.title.x = element_text(size = 15),axis.title.y = element_text(angle = 90, size = 15), plot.title = element_text(size=15))
     plot(plot)
     ggsave(here("Figures", "Run4_2.pdf"), height = 7, width = (14))




     this.ForecastData <- makeForecastData(.predCalibration = as.data.frame(predCal), .outcomeCalibration = outCal[,1], .modelNames= names(predCal))
     thisEnsembleBayes <- calibrateEnsemble(this.ForecastData,model="normal", method = "gibbs",predType="posteriorMean",useModelParams=FALSE, modelPriors = rep(0.1,15), iterations = 50000, burns = 20000, thinning = 5)

     forPlot <- as_tibble(thisEnsembleBayes@posteriorWeights) 
     names(forPlot) <- names(predCal)


     colMeans(thisEnsembleBayes@posteriorWeights)

     plot <- ggplot(data=forPlot, aes(x = Abramowitz)) 
     plot <- plot + geom_density(alpha=0.4,size=1)  
     plot <- plot +labs( y="Density", x="posterior Weight Abramowitz")
     plot <- plot + theme(axis.text.x=element_text(size=15, face = "bold"), axis.text.y=element_text(size=15, face = "bold"), axis.title.x = element_text(size = 15),axis.title.y = element_text(angle = 90, size = 15), plot.title = element_text(size=15))
     plot(plot)
     ggsave(here("Figures", "RBNF.pdf"), height = 7, width = (14))

     mean(forPlot$Abramowitz)




     plot(density(thisEnsembleBayes1@posteriorWeights[,1]))

     summary(thisEnsembleBayes)
     quantile(thisEnsembleBayes1@posteriorWeights[,1])
     plot(thisEnsembleBayes1@posteriorWeights[,1], type = "l")

     iterations = 75000,
                                       burns = 20000,             
                                       thinning = 10,
                                       predType = "posteriorMean", 
                                       model="normal", 
                                       useModelParams=FALSE, 
                                       modelPriors = rep(1,9))

summary(thisEnsembleBayes)
plot(thisEnsembleBayes@posteriorWeights[,1], type = "l")

test1 <- thisEnsembleBayes@posteriorWeights
test2 <- thisEnsembleBayes@posteriorWeights

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

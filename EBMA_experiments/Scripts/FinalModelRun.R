library(here)
library(EBMAforecast)
library(tidyverse)
library(readr)
library(conflicted)
library(colorRamps)
library(RColorBrewer)
library(tictoc)
library(kableExtra)
library(modelsummary)
library(coda)
library(tidybayes)

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


### from kableExtra
solve_enc <- function(x) {
  #may behave differently based on Sys.setlocale settings with respect to characters
  out <- enc2utf8(as.character(base::format(x, trim = TRUE, justify = 'none')))
  mostattributes(out) <- attributes(x)
  return(out)
}

conflict_prefer("filter", "dplyr")
data <- read_csv(here( "Presidential Forecasting 2020", "Percent2PartyVote .csv"))


## say alpha = 0.25
alpha = 0.225


predCal <- data %>% 
  filter(Year !=2020) %>%
  select(-c(Cuzan, Hibbs, Holbrook)) %>%
  rename(NB = `N-B`,
         EW = `Erikson and Wlezien`,
         LB = `Lewis Beck-Tien`,
         DeSart = `Jay DeSart`,
         Graefe = `Andreas Graefe Issues and Leaders`,
         BVN = `Jerome Bruno Jerome Veronique and Richard Nadeau`,
         Lichtman = `Allan Lichtman`,
         RBNF = `Thomas Rietz Joyce Berg Forrest Nelson Robert Forsythe`) %>% 
  select(-c(Year,truth, NB))

initW1 <- c(rep(1/11,11))
initW2 <- c(rep(1/4,4),rep(0,7))
initW3 <- c(rep(0,4),rep(1/7,7))


outCal <- data %>% 
  filter(Year !=2020) %>% 
  select(truth) %>% 
  as.data.frame()

this.ForecastData <- makeForecastData(.predCalibration = as.data.frame(predCal), .outcomeCalibration = outCal[,1], .modelNames= names(predCal))

set.seed(1234)
thisEnsembleBayes1 <- calibrateEnsemble(this.ForecastData, method = "gibbs",
                                        iterations = 250000, burns = 40000, thinning = 4,
                                        predType = "posteriorMean", model="normal",
                                        useModelParams=FALSE,
                                        modelPriors = rep(alpha,dim(predCal)[2]),
                                        W = initW1)
set.seed(5678)
thisEnsembleBayes2 <- calibrateEnsemble(this.ForecastData, method = "gibbs",
                                        iterations = 250000, burns = 40000, thinning = 4,
                                        predType = "posteriorMean", model="normal",
                                        useModelParams=FALSE,
                                        modelPriors = rep(alpha,dim(predCal)[2]),
                                        W = initW2)
set.seed(9101112)
thisEnsembleBayes3 <- calibrateEnsemble(this.ForecastData, method = "gibbs",
                                        iterations = 250000, burns = 40000, thinning = 4,
                                        predType = "posteriorMean", model="normal",
                                        useModelParams=FALSE,
                                        modelPriors = rep(alpha,dim(predCal)[2]),
                                        W = initW3)
#### let's look whether it converged
round(colMeans(thisEnsembleBayes1@posteriorWeights),3)
round(colMeans(thisEnsembleBayes2@posteriorWeights),3)
round(colMeans(thisEnsembleBayes3@posteriorWeights),3)

save(thisEnsembleBayes1, file = here("Results", "thisEnsembleBayes1.rda"))
save(thisEnsembleBayes2, file = here("Results", "thisEnsembleBayes2.rda"))
save(thisEnsembleBayes3, file = here("Results", "thisEnsembleBayes3.rda"))

### plot densities for abramowitz
forPlot <- tibble(weights = c(thisEnsembleBayes1@posteriorWeights[,1], thisEnsembleBayes2@posteriorWeights[,1], thisEnsembleBayes3@posteriorWeights[,1]), run = c(rep("Start 1", dim(thisEnsembleBayes1@posteriorWeights)[1]), rep("Start 2", dim(thisEnsembleBayes2@posteriorWeights)[1]), rep("Start 3", dim(thisEnsembleBayes3@posteriorWeights)[1])))
plot <- ggplot(data=forPlot, aes(x = weights, color = run))
plot <- plot + geom_density(alpha=0.4,size=1)  #+ facet_wrap(vars(model))
plot <- plot +labs( y="Density", x="posterior Weight",  title = "Posterior Weights Abramowitz Across Chains")
plot <- plot + theme(axis.text.x=element_text(size=15, face = "bold"), axis.text.y=element_text(size=15, face = "bold"), axis.title.x = element_text(size = 15),axis.title.y = element_text(angle = 90, size = 15), plot.title = element_text(size=15))
plot(plot)
ggsave(here("Figures", "PostW_abramowitz.pdf"), height = 7, width = (7*1.618))

weights1 <- as_tibble(thisEnsembleBayes1@posteriorWeights, .name_repair = "minimal")
weights2 <- as_tibble(thisEnsembleBayes2@posteriorWeights, .name_repair = "minimal")
weights3 <- as_tibble(thisEnsembleBayes3@posteriorWeights, .name_repair = "minimal")

names(weights1) <- names(weights2) <- names(weights3) <- names(predCal)
posteriorFull <- bind_rows(weights1,weights2,weights3)




mcmc.posterior <-as.mcmc.list(list(as.mcmc(thisEnsembleBayes1@posteriorWeights), as.mcmc(thisEnsembleBayes2@posteriorWeights), as.mcmc(thisEnsembleBayes3@posteriorWeights)))

tidy_posterior <- tidy_draws(mcmc.posterior)
names(tidy_posterior)[4:14] <- names(predCal)

posterior_summary <- tidy_posterior %>%
       gather_draws(Abramowitz, Campbell, EW, LB, Lockerbie, Fair, DeSart, Graefe, BVN, Lichtman, RBNF) %>%
  mean_qi() %>%
  select(-c(.width, .point, .interval)) %>%
  mutate(.value = round(.value,3),
         .lower = round(.lower, 3),
         .upper = round(.upper, 3)) %>%
  rename(Model = .variable,
         'Posterior Mean' = .value) %>%
  select(-c(.lower, .upper)) %>%
  mutate(Model = case_when(Model == "BVN" ~ "Bruno, Veronique and Nadeau",
                           Model == "EW" ~ "Erikson and Wlezien",
                           Model == "LB" ~ "Lewis-Beck and Tien",
                           Model == "RBNF" ~ "Rietz, Berg, Nelson and Forsythe",
                           TRUE ~ Model))



tab <- kable(posterior_summary, "latex", booktabs = T, linesep = "", caption = "Mean Posterior Model Weights", label = "weights")
#kable_styling(latex_options =c("scale_down"), position = "center")
#add_header_above(c(" " = 1, setNames(3, "Posterior Mean and 95% Credible Intervals")), bold = TRUE)






predTest <- data %>%
  filter(Year ==2020) %>%
  select(-c(Cuzan, Hibbs, Holbrook)) %>%
  rename(NB = `N-B`,
         EW = `Erikson and Wlezien`,
         LB = `Lewis Beck-Tien`,
         DeSart = `Jay DeSart`,
         Graefe = `Andreas Graefe Issues and Leaders`,
         BVN = `Jerome Bruno Jerome Veronique and Richard Nadeau`,
         Lichtman = `Allan Lichtman`,
         RBNF = `Thomas Rietz Joyce Berg Forrest Nelson Robert Forsythe`) %>%
  select(-c(Year,truth, NB))


#### out of sample forecast
postPredTest <- matrix(data=NA, nrow=dim(predTest)[1], ncol=dim(posteriorFull)[1])


W.mat <- as.matrix(posteriorFull)
for(i in 1:dim(W.mat)[1]){
  bmaPredTest <-array(plyr::aaply(predTest, 1, function(x) {sum(x* W.mat[i,], na.rm=TRUE)}), dim=c(1, 1,1))
  bmaPredTest <-  bmaPredTest/array(t(W.mat[i,]%*%t(1*!is.na(predTest))), dim=c(1, 1, 1))
  bmaPredTest[,,-1] <- NA
  postPredTest[,i] <- bmaPredTest[,1,]
}


posteriorPred <- as_tibble(t(postPredTest), .name_repair = "minimal")
names(posteriorPred) <- "PosteriorPrediction"


mean(posteriorPred$PosteriorPrediction)
median(posteriorPred$PosteriorPrediction)
quantile(posteriorPred$PosteriorPrediction, prob = c(0.025, 0.975))


save(posteriorPred, file = here("Results", "posteriorPred.rda"))

#### desnity of expenditure
plot <- ggplot(data=posteriorPred , aes(x = PosteriorPrediction))
plot <- plot + geom_density(alpha=0.4,size=2)
plot <- plot + theme_plot()
plot <- plot + geom_vline(aes(xintercept = mean(posteriorPred$PosteriorPrediction)), col = "red", size = 2)
plot <- plot +labs(title = "", y="Density", x="Incumbent Two-Party Vote Share")
plot <- plot + theme(axis.text.x=element_text(size=20, face = "bold"), axis.text.y=element_text(size=20, face = "bold"), axis.title.x = element_text(size = 18),axis.title.y = element_text(angle = 90, size = 18), plot.title = element_text(size=22))
plot(plot)
ggsave(here("Figures", "PosteriorPredDensity.pdf"))


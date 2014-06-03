##################################################
##### TEMPORAL VALIDATION PLOTS: R FUNCTIONS #####
##################################################
########################################################
### TV_Data: create an object for input in TV_Plots  ###
########################################################
TV_Data <- function(ObsPres_t1, PredPres_t1, ObsPres_t2, PredPres_t2){
  Range_Dataset <- data.frame(Site = 1:length(ObsPres_t1), ObsPres_t1 = ObsPres_t1, PredPres_t1 = PredPres_t1, ObsPres_t2 = ObsPres_t2, PredPres_t2 = PredPres_t2)        
  Range_Dataset$ObsChange <- NA 
  for (i in 1:length(Range_Dataset$ObsChange)){
    if (Range_Dataset$ObsPres_t1[i] == 1 & Range_Dataset$ObsPres_t2[i] == 0) Range_Dataset$ObsChange[i] <- -1
    if (Range_Dataset$ObsPres_t1[i] == 0 & Range_Dataset$ObsPres_t2[i] == 0) Range_Dataset$ObsChange[i] <- 0
    if (Range_Dataset$ObsPres_t1[i] == 1 & Range_Dataset$ObsPres_t2[i] == 1) Range_Dataset$ObsChange[i] <- 1
    if (Range_Dataset$ObsPres_t1[i] == 0 & Range_Dataset$ObsPres_t2[i] == 1) Range_Dataset$ObsChange[i] <- 2
  }   
  Range_Dataset$ObsChange <- factor(Range_Dataset$ObsChange, ordered = TRUE)
  Range_Dataset$Gains <- as.numeric(Range_Dataset$ObsChange == 2)
  Range_Dataset$Losses <- as.numeric(Range_Dataset$ObsChange == -1)
  Range_Dataset$PredChange <- Range_Dataset$PredPres_t2 - Range_Dataset$PredPres_t1
  PositiveChange <- Range_Dataset$PredChange > 0
  Range_Dataset$PropPredChange[PositiveChange] <- Range_Dataset$PredChange[PositiveChange]/(1 - Range_Dataset$PredPres_t1[PositiveChange])
  NegativeChange <- Range_Dataset$PredChange < 0 
  Range_Dataset$PropPredChange[Range_Dataset$PredChange == 0] <- 0
  Range_Dataset$PropPredChange[NegativeChange] <- Range_Dataset$PredChange[NegativeChange]/Range_Dataset$PredPres_t1[NegativeChange] 
  Range_Dataset <- Range_Dataset[order(Range_Dataset$PropPredChange),]
}

############################################################
### TV_Plot: generate and plot temporal validation plots ###
############################################################
TV_Plot <- function(Range_Dataset, ...){
  ## Load required libraries
  require(splines)
  ## Gain function
  Gains <- Range_Dataset$Gains[Range_Dataset$ObsChange != 1] 
  PropPredGain <- Range_Dataset$PropPredChange[Range_Dataset$ObsChange != 1]
  Gain_gam <- glm(Gains ~ ns(PropPredGain, df = 2), weights=rep(1, length(PropPredGain)), family=binomial)
  Simulated_x <- seq(min(Range_Dataset$PropPredChange), max(Range_Dataset$PropPredChange), length = 512)
  Simulated_gain <- predict(Gain_gam, newdata = data.frame(PropPredGain = Simulated_x), se.fit = TRUE, type = "response")
  Gain_Curve <- data.frame(x = Simulated_x, y = Simulated_gain$fit, sd = Simulated_gain$se.fit * sqrt(512))
  ## Loss function
  Losses <- Range_Dataset$Losses[Range_Dataset$ObsChange != 0]
  PropPredLoss <- Range_Dataset$PropPredChange[Range_Dataset$ObsChange != 0]
  Loss_gam <- glm(Losses ~ ns(PropPredLoss, df = 2), weights=rep(1, length(PropPredLoss)), family=binomial)
  Simulated_loss <- predict(Loss_gam, newdata = data.frame(PropPredLoss = Simulated_x), se.fit = TRUE, type = "response")
  Loss_Curve <- data.frame(x = Simulated_x, y = Simulated_loss$fit, sd = Simulated_loss$se.fit * sqrt(512))
  ## Change in observed probability of presence
  Summed_Curve <- data.frame(x = Simulated_x, y = -Loss_Curve$y + Gain_Curve$y, sd = sqrt(Loss_Curve$sd^2 + Gain_Curve$sd^2)/sqrt(512))
  ## Generate plot 
  plot(y ~ x, data = Summed_Curve, type = "n", ylab = "Change in observed probability of presence", 
       xlab = "Proportional change in modelled probability of presence", xlim = c(-1, 1), ylim = c(-1, 1), 
       tck = 0.02, las = 1, mgp = c(3, 0.5, 0), ...)
  ## Draw quadrant axes      
  abline(v = 0, col = grey(0.5), lwd = 1.5)
  abline(h = 0, col = grey(0.5), lwd = 1.5)
  ## Draw tick marks 
  arrows(c(-0.01, -0.01, -0.01, -0.01), c(-1, -0.5, 0.5, 1), c(0.01, 0.01, 0.01, 0.01), 
         c(-1, -0.5, 0.5, 1), col = grey(0.5), length = 0, lwd = 1.5)
  arrows(c(-1, -0.5, 0.5, 1), c(-0.01, -0.01, -0.01, -0.01), c(-1, -0.5, 0.5, 1), 
         c(0.01, 0.01, 0.01, 0.01), col = grey(0.5), length = 0, lwd = 1.5)
  ## Draw rug plots        
  rug(Range_Dataset$PropPredChange[Range_Dataset$ObsChange != 1], side = 3, col = grey(0.8), lwd = 2)
  rug(Range_Dataset$PropPredChange[Range_Dataset$ObsChange == 2], side = 3, col = "mediumblue", lwd = 2)
  rug(Range_Dataset$PropPredChange[Range_Dataset$ObsChange != 0], side = 1, col = grey(0.8), lwd = 2)   
  rug(Range_Dataset$PropPredChange[Range_Dataset$ObsChange == -1], side = 1, col = "tomato", lwd = 2)
  ## Draw curves
  # Gain function
  lines(Simulated_x, Gain_Curve$y, col = "mediumblue", lwd = 2)
  # Loss function 
  lines(Simulated_x, -Loss_Curve$y, col = "tomato", lwd = 2)
  # Ideal curve 
  lines(Summed_Curve$x, Summed_Curve$x, lty = "dashed")
  # Fitted curve 
  lines(Summed_Curve$x, Summed_Curve$y, lwd = 3)
  # Standard errors
  lines(Summed_Curve$x, Summed_Curve$y + 2*Summed_Curve$sd, col = "orange")
  lines(Summed_Curve$x, Summed_Curve$y - 2*Summed_Curve$sd, col = "orange")
}
##################################################################
### TV_Measures: derive AccTV, CorTV, and BiasTV from TV plots ###
##################################################################
TV_Measures <- function(Range_Dataset){
  ## Load required libraries
  require(splines)
  require(pracma)
  ## Gain function
  Gains <- Range_Dataset$Gains[Range_Dataset$ObsChange != 1] 
  PropPredGain <- Range_Dataset$PropPredChange[Range_Dataset$ObsChange != 1]
  Gain_gam <- glm(Gains ~ ns(PropPredGain, df = 2), weights=rep(1, length(PropPredGain)), family=binomial)
  Simulated_gain <- predict(Gain_gam, newdata = data.frame(PropPredGain = Range_Dataset$PropPredChange), se.fit = FALSE, type = "response")
  ## Loss function
  Losses <- Range_Dataset$Losses[Range_Dataset$ObsChange != 0]
  PropPredLoss <- Range_Dataset$PropPredChange[Range_Dataset$ObsChange != 0]
  Loss_gam <- glm(Losses ~ ns(PropPredLoss, df = 2), weights=rep(1, length(PropPredLoss)), family=binomial)
  Simulated_loss <- predict(Loss_gam, newdata = data.frame(PropPredLoss = Range_Dataset$PropPredChange), se.fit = FALSE, type = "response")
  ## Change in observed probability of presence
  Summed_Curve <- data.frame(x = Range_Dataset$PropPredChange, y = Simulated_gain - Simulated_loss)
  Gain_points <- Summed_Curve$y[Range_Dataset$ObsChange != 1]
  Loss_points <- Summed_Curve$y[Range_Dataset$ObsChange != 0]
  All_points <- data.frame(site = c(Range_Dataset$Site[Range_Dataset$ObsChange != 1], Range_Dataset$Site[Range_Dataset$ObsChange != 0]),
                           x = c(PropPredGain, PropPredLoss), y = c(Gain_points, Loss_points))
  Data_points <- All_points[match(unique(All_points$site), All_points$site),]
  ## Calculate precision 
  AccTV <- 1 - weighted.mean(abs(Data_points$y - Data_points$x), abs(Data_points$x))
  ## Calculate correlation 
  CorTV <- sqrt(summary(lm(Data_points$y ~ Data_points$x, weights = abs(Data_points$x)))$r.squared) * coef(lm(Data_points$y ~ Data_points$x))[2]/abs(coef(lm(Data_points$y ~ Data_points$x))[2])
  ## Calculate bias 
  Simulated_x <- seq(min(Range_Dataset$PropPredChange), max(Range_Dataset$PropPredChange), length = 512)
  Continuous_gain <- predict(Gain_gam, newdata = data.frame(PropPredGain = Simulated_x), se.fit = FALSE, type = "response")
  Continuous_loss <- predict(Loss_gam, newdata = data.frame(PropPredLoss = Simulated_x), se.fit = FALSE, type = "response")
  Summed_Curve <- data.frame(x = Simulated_x, y = Continuous_gain - Continuous_loss)
  BiasTV <- trapz(Summed_Curve$x, Summed_Curve$x) - trapz(Summed_Curve$x, Summed_Curve$y)      
  metrics <- c(AccTV, CorTV, BiasTV)
  names(metrics) <- c("AccTV", "CorTV", "BiasTV")
  return(metrics)
}

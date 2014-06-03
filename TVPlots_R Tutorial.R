#################################################
##### TEMPORAL VALIDATION PLOTS: R TUTORIAL #####
#################################################
# Set seed for random number generator
set.seed(2222)
# Source custom functions
source("Temporal Validation Plots_Supporting Information_R Functions.R")

######################################
#### Simulate simplified lanscape ####
######################################
#### Environment in t1
# number of samples in data sets
ns <- 900
### Temperature
temp_t1 <- runif(ns, min=0, max=1)
temp_t1 <- matrix(temp_t1, 30, 30)

### Precipitation
prec_t1 <- runif(ns, min=0, max=1)
prec_t1 <- matrix(prec_t1, 30, 30)

### Covar, a variable strongly correlated to temperature in t1 
covar_t1 <- temp_t1 + rnorm(30*30, 0, 0.1)
## rescale values to range between 0 and 1
range <- function(x){(x-min(x))/(max(x)-min(x))}
covar_t1 <- range(covar_t1)

############################################
#### Environmental Functional Responses ####
############################################
#### Truth ####
Truth_Response <- function(x, y, z) {0.5 * (x + y)}
### Simulate probability of presence in t1
Truth_Probt1 <- Truth_Response(x = temp_t1, y = prec_t1, z = covar_t1)
### Convert probability of presence to presence-absence 
## Function to conduct Bernouilli trials on modelled probabilities
Binarise <- function(mat){
  new_mat <- mat
  for (i in 1:ncol(mat)){
    new_mat[,i] <- rbinom(30, 1, mat[,i])
  }
  return(new_mat)
}
## Conduct Bernouilli trials based on true probability of presence in t1
Truth_Bint1 <- Binarise(Truth_Probt1)
### Produce summaries of predictions and observations for easy input into TV plot functions
Truth_Distribution <- data.frame(Site = 1:900)
Truth_Distribution$Probt1 <- as.vector(Truth_Probt1)
Truth_Distribution$Bint1 <- as.vector(Truth_Bint1)

#### Incomplete Model: probability of presence as a function of temperature only ####
### Estimate parameters from environment
Incomplete_Model <- lm(Truth_Distribution$Probt1 ~ as.vector(temp_t1))
Incomplete_Response <- function(x, y, z) { coef(Incomplete_Model)[1] + coef(Incomplete_Model)[2]*x }
### Simulate probability of presence in t1
Incomplete_Probt1 <- Incomplete_Response(x = temp_t1, y = prec_t1, z = covar_t1)
### Produce summaries of predictions and observations for easy input into TV plot functions
Incomplete_Distribution <- data.frame(Site = 1:900)
Incomplete_Distribution$Probt1 <- as.vector(Incomplete_Probt1)
Incomplete_Distribution$Bint1 <- Truth_Distribution$Bint1

#### Collinear Model: probability of presence as a function of precipitation and covar only ####
### Estimate parameters from environment
Collinear_Model <- lm(Truth_Distribution$Probt1 ~ as.vector(prec_t1) + as.vector(covar_t1))
Collinear_Response <- function(x, y, z) { coef(Collinear_Model)[1] + coef(Collinear_Model)[2]*y + coef(Collinear_Model)[3]*z }
### Simulate probability of presence in t1
Collinear_Probt1 <- Collinear_Response(x = temp_t1, y = prec_t1, z = covar_t1)
### Produce summaries of predictions and observations for easy input into TV plot functions
Collinear_Distribution <- data.frame(Site = 1:900)
Collinear_Distribution$Probt1 <- as.vector(Collinear_Probt1)
Collinear_Distribution$Bint1 <- Truth_Distribution$Bint1

#### Incomplete and Collinear Model: probability of presence as a function of covar only ####
### Estimate parameters from environment
IncomplANDColl_Model <- lm(Truth_Distribution$Probt1 ~ as.vector(covar_t1))
IncomplANDColl_Response <- function(x, y, z) { coef(IncomplANDColl_Model)[1] + coef(IncomplANDColl_Model)[2]*z }
### Simulate probability of presence in t1
IncomplANDColl_Probt1 <- IncomplANDColl_Response(x = temp_t1, y = prec_t1, z = covar_t1)
### Produce summaries of predictions and observations for easy input into TV plot functions
IncomplANDColl_Distribution <- data.frame(Site = 1:900)
IncomplANDColl_Distribution$Probt1 <- as.vector(IncomplANDColl_Probt1)
IncomplANDColl_Distribution$Bint1 <- Truth_Distribution$Bint1

######################################################################
#### FIGURE 1: ENVIRONMENT AND ENVIRONMENTAL FUNCTIONAL RESPONSES ####
######################################################################
Simulated_x <- seq(0, 1, 0.01)
Constant_x <- rep(0, length(Simulated_x))
### Set layout
par(mfrow = c(2,3))
## Plot Environment
par(mar = c(1, 4, 5, 0))
image(temp_t1, zlim = c(0,1), col = rev(heat.colors(20)), xaxt = "n", yaxt = "n", tck = 0.02, mgp = c(2, 0.5, 0))
par(mar = c(1, 2, 5, 2))
image(prec_t1, zlim = c(0,1), col = rev(heat.colors(20)), xaxt = "n", yaxt = "n", tck = 0.02, mgp = c(2, 0.5, 0))
par(mar = c(1, 0, 5, 4))
image(covar_t1, zlim = c(0,1), col = rev(heat.colors(20)), xaxt = "n", yaxt = "n", tck = 0.02, mgp = c(2, 0.5, 0))         
# Temperature
par(mar = c(5, 4, 1, 0))
plot(Simulated_x, Simulated_x, type = "n", ylab = "Probability of presence", xlab = "Temperature", tck = 0.02, mgp = c(2, 0.5, 0))
lines(Simulated_x, Collinear_Response(x = Simulated_x, y = Constant_x, z = Constant_x), lwd = 2, col = "deepskyblue3")
lines(Simulated_x, IncomplANDColl_Response(x = Simulated_x, y = Constant_x, z = Constant_x), lwd = 2, col = "green3")
lines(Simulated_x, Truth_Response(x = Simulated_x, y = Constant_x, z = Constant_x), lwd = 3)
lines(Simulated_x, Incomplete_Response(x = Simulated_x, y = Constant_x, z = Constant_x), lwd = 2, col = "orange")
# Precipitation
par(mar = c(5, 2, 1, 2))
plot(Simulated_x, Simulated_x, type = "n", ylab = "Probability of presence", xlab = "Precipitation", tck = 0.02, mgp = c(2, 0.5, 0))
lines(Simulated_x, Incomplete_Response(x = Constant_x, y = Simulated_x, z = Constant_x), lwd = 2, col = "orange")
lines(Simulated_x, IncomplANDColl_Response(x = Constant_x, y = Simulated_x, z = Constant_x) - 0.005, lwd = 2, col = "green3")
lines(Simulated_x, Truth_Response(x = Constant_x, y = Simulated_x, z = Constant_x), lwd = 3)
lines(Simulated_x, Collinear_Response(x = Constant_x, y = Simulated_x, z = Constant_x), lwd = 2, col = "deepskyblue3")
legend("topright", c("Truth", "Incomplete", "Collinear", "Incomplete and Collinear"), col = c("black","orange","deepskyblue3", "green3"), lty = c(1, 1, 1, 1), lwd = c(3, 2, 2, 2), bty = "n", y.intersp = 1)
# Tempcor
par(mar = c(5, 0, 1, 4))
plot(Simulated_x, Simulated_x, type = "n", ylab = "Probability of presence", xlab = "Covar", tck = 0.02, mgp = c(2, 0.5, 0))
lines(Simulated_x, Incomplete_Response(x = Constant_x, y = Constant_x, z = Simulated_x), lwd = 3, col = "orange")
lines(Simulated_x, Truth_Response(x = Constant_x, y = Constant_x, z = Simulated_x), lwd = 2)
lines(Simulated_x, Collinear_Response(x = Constant_x, y = Constant_x, z = Simulated_x) - 0.005, lwd = 2, col = "deepskyblue3")
lines(Simulated_x, IncomplANDColl_Response(x = Constant_x, y = Constant_x, z = Simulated_x) - 0.01, lwd = 2, col = "green3")

#################################
#### Range Change Simulation ####
#################################
#### Environmental change scenario 
### Medium increase in mean temperature, with a Medium random variation around that mean increase
temp_t2 <- temp_t1 + rnorm(30*30, 0.3, 0.25)
### Medium increase in mean precipitation, with a Medium random variation around that mean increase
prec_t2 <- prec_t1 + rnorm(30*30, -0.15, 0.5)
### Medium random change in the variable correlated to temperature
covar_t2 <- covar_t1 + rnorm(30*30, 0, 0.5)

##### Observed and modelled changes
#### Truth ####
### Simulate probability of presence in t2
Truth_Probt2 <- Truth_Response(x = temp_t2, y = prec_t2, z = covar_t2)
### Make sure probabilities range between 0 and 1
Truth_Probt2[Truth_Probt2 < 0] <- 0; Truth_Probt2[Truth_Probt2 > 1] <- 1
### Convert probabilities to binary presence-absence
Truth_Bint2 <- Binarise(Truth_Probt2)
Truth_Distribution$Probt2 <- as.vector(Truth_Probt2); Truth_Distribution$Bint2 <- as.vector(Truth_Bint2)

#### Incomplete Model ####
### Simulate probability of presence in t2
Incomplete_Probt2 <- Incomplete_Response(x = temp_t2, y = prec_t2, z = covar_t2)
### Make sure probabilities range between 0 and 1
Incomplete_Probt2[Incomplete_Probt2 < 0] <- 0; Incomplete_Probt2[Incomplete_Probt2 > 1] <- 1
### Produce summaries of predictions and observations for easy input into TV plot functions
Incomplete_Distribution$Probt2 <- as.vector(Incomplete_Probt2); Incomplete_Distribution$Bint2 <- Truth_Distribution$Bint2

#### Collinear Model ####
### Simulate probability of presence in t2
Collinear_Probt2 <- Collinear_Response(x = temp_t2, y = prec_t2, z = covar_t2)
### Make sure probabilities range between 0 and 1
Collinear_Probt2[Collinear_Probt2 < 0] <- 0; Collinear_Probt2[Collinear_Probt2 > 1] <- 1
### Produce summaries of predictions and observations for easy input into TV plot functions
Collinear_Distribution$Probt2 <- as.vector(Collinear_Probt2); Collinear_Distribution$Bint2 <- Truth_Distribution$Bint2

#### Incomplete AND Collinear Model ####
### Simulate probability of presence in t2
IncomplANDColl_Probt2 <- IncomplANDColl_Response(x = temp_t2, y = prec_t2, z = covar_t2)
### Make sure probabilities range between 0 and 1
IncomplANDColl_Probt2[IncomplANDColl_Probt2 < 0] <- 0; IncomplANDColl_Probt2[IncomplANDColl_Probt2 > 1] <- 1
### Produce summaries of predictions and observations for easy input into TV plot functions
IncomplANDColl_Distribution$Probt2 <- as.vector(IncomplANDColl_Probt2); IncomplANDColl_Distribution$Bint2 <- Truth_Distribution$Bint2

####################################################################
#### FIGURE2A-C: OBSERVATIONS, PREDICTIONS AND TV PLOT FOR TRUTH ###
####################################################################
### Generate TV_Data object to plot and analyse range dynamics
Truth_TV <- TV_Data(ObsPres_t1 = Truth_Distribution$Bint1,
                                 PredPres_t1 = Truth_Distribution$Probt1,
                                 ObsPres_t2 = Truth_Distribution$Bint2,
                                 PredPres_t2 = Truth_Distribution$Probt2)
### Re-order Truth_TV for convenience
Truth_TV_Sorted <- Truth_TV[order(Truth_TV$Site),]
### Observed changes
Truth_Change_Observed <- Truth_TV_Sorted$ObsChange
Truth_Change_Observed <- as.numeric(levels(Truth_Change_Observed))[Truth_Change_Observed]
### Modelled changes
Truth_Change_Predicted <- Truth_TV_Sorted$PropPredChange
#### Generate Plot
par(mfrow = c(2, 2), mai = c(0.2, 0.2, 0.2, 0.2))
### Plot observed changes
cols_obs <- c("tomato", "seashell", "lightblue", "mediumblue")
cols_pred <- colorRampPalette(c("tomato", grey(0.8), "mediumblue"))(10)
image(matrix(Truth_Change_Observed, nrow = 30, ncol = 30), col = cols_obs, xaxt = "n", yaxt = "n")
### Plot transferability plot
TV_Plot(Truth_TV)
### Plot legend
### Legend
frame()
library(fields)
image.plot(matrix(Truth_Change_Predicted, nrow = 30, ncol = 30), zlim = c(-1,1), axes = FALSE, box = "n", col = cols_pred, xaxt = "n", yaxt = "n", legend.only = TRUE, add = FALSE)
### Plot predicted changes
image(matrix(Truth_Change_Predicted, nrow = 30, ncol = 30), col = cols_pred, xaxt = "n", yaxt = "n")

#############################################################
#### FIGURE3D-F: TEMPORAL VALIDATION PLOTS FOR MODELS 1-3 ###
#############################################################
### Model1
Incomplete_TV <- TV_Data(ObsPres_t1 = Incomplete_Distribution$Bint1,
                                      PredPres_t1 = Incomplete_Distribution$Probt1,
                                      ObsPres_t2 = Incomplete_Distribution$Bint2,
                                      PredPres_t2 = Incomplete_Distribution$Probt2)

### Model2
Collinear_TV <- TV_Data(ObsPres_t1 = Collinear_Distribution$Bint1,
                                     PredPres_t1 = Collinear_Distribution$Probt1,
                                     ObsPres_t2 = Collinear_Distribution$Bint2,
                                     PredPres_t2 = Collinear_Distribution$Probt2)

### Model3
IncomplANDColl_TV <- TV_Data(ObsPres_t1 = IncomplANDColl_Distribution$Bint1,
                                          PredPres_t1 = IncomplANDColl_Distribution$Probt1,
                                          ObsPres_t2 = IncomplANDColl_Distribution$Bint2,
                                          PredPres_t2 = IncomplANDColl_Distribution$Probt2)
### Set layout
par(mfrow = c(2,3), mai = c(0.15, 0.15, 0.15, 0.15))
### Plot transferability plots
TV_Plot(Incomplete_TV)
TV_Plot(Collinear_TV)
TV_Plot(IncomplANDColl_TV)
### Plot predicted changes
image(matrix(Incomplete_TV[order(Incomplete_TV$Site),]$PropPredChange, nrow = 30, ncol = 30), col = cols_pred, xaxt = "n", yaxt = "n")
image(matrix(Collinear_TV[order(Collinear_TV$Site),]$PropPredChange, nrow = 30, ncol = 30), col = cols_pred, xaxt = "n", yaxt = "n")
image(matrix(IncomplANDColl_TV[order(IncomplANDColl_TV$Site),]$PropPredChange, nrow = 30, ncol = 30), col = cols_pred, xaxt = "n", yaxt = "n")

###############################################
#### TABLE 1: TEMPORAL VALIDATION MEASURES ####
###############################################
rbind(TV_Measures(Truth_TV),
      TV_Measures(Incomplete_TV),
      TV_Measures(Collinear_TV),
      TV_Measures(IncomplANDColl_TV))

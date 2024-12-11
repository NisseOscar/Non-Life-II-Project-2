library(ChainLadder)
library(readxl)
library(dplyr)
library(reshape2)

# Read excel and extract base data
base_data <- read_excel("Project2_base_data.xlsx")
sheets
# List sheets
sheets <- excel_sheets("Project2_base_data.xlsx")
triangle_A <- read_excel("Project2_base_data.xlsx", sheet = "A_cumulative_base")
triangle_B <- read_excel("Project2_base_data.xlsx", sheet = "B_cumulative_base")

# Create incremental triangles
triangle_A_inc <- triangle_A
triangle_B_inc <- triangle_B
for(i in 1:10){
  for(j in 2:10){
    triangle_A_inc[i, j] = triangle_A[i, j] - triangle_A[i, j - 1]
  }
}
for(i in 1:9){
  for(j in 2:9){
    triangle_B_inc[i, j] = triangle_B[i, j] - triangle_B[i, j - 1]
  }
}

# Make Mack Full Triangle
mack_A <- MackChainLadder(Triangle = triangle_A[, -1], est.sigma = "Mack")
mack_B <- MackChainLadder(Triangle = triangle_B[, -1], est.sigma = "Mack")

## Calculate the Mack Triangle Manually
triangle_A_2
triangle_A_2 <- triangle_A[,]
m_A <- 10
m_B <- 9
f_A <- sapply(1:(m_A-1),
            function(i){
              sum(triangle_A[c(1:(m_A-i)),i+1])/sum(triangle_A[c(1:(m_A-i)),i])
            })

f_B <- sapply(1:(m_B-1),
              function(i){
                sum(triangle_B[c(1:(m_B-i)),i+1])/sum(triangle_B[c(1:(m_B-i)),i])
              })

#Calculate cumulative claims triangle using Mack's chain ladder approach
mack_manual_A <- triangle_A
mack_manual_B <- triangle_B
for(i in 2:10){
  for(j in (12-i):10){
    mack_manual_A[i, j] = mack_manual_A[i, j - 1] * f_A[j - 1]
  }
}
for(i in 2:9){
  for(j in (11-i):9){
    mack_manual_B[i, j] = mack_manual_B[i, j - 1] * f_B[j - 1]
  }
}

mack_A$FullTriangle-mack_manual_A
mack_B$FullTriangle-mack_manual_B

#### Assignment b calculate incremental triangle
triangle_A_Mack_incremental<-mack_A$FullTriangle
for(i in 1:10){
  for(j in 2:10){
    triangle_A_Mack_incremental[i, j] = mack_A$FullTriangle[i, j] - mack_A$FullTriangle[i, j - 1]
  }
}
triangle_A_Mack_incremental
triangle_B_Mack_incremental <- mack_B$FullTriangle
for(i in 1:9){
  for(j in 2:9){
    triangle_B_Mack_incremental[i, j] = mack_B$FullTriangle[i, j] - mack_B$FullTriangle[i, j - 1]
  }
}
triangle_B_Mack_incremental

### Assignment c 

# Model A
# Translate trianges to tabulars
triangle_A_tab <- triangle_A_inc %>%melt(id.vars = "N#")
colnames(triangle_A_tab) <- c("origin", "dev", "Claims")
triangle_A_tab$origin <- as.factor(triangle_A_tab$origin)
triangle_A_tab_train <- triangle_A_tab[!is.na(triangle_A_tab$Claims), ]
triangle_A_tab_pred <- triangle_A_tab[is.na(triangle_A_tab$Claims), ]

# Estimate alpha and beta factors from known claims & estimated development factors
model_a <- glm(Claims ~ dev + origin-1, data = triangle_A_tab_train, family = poisson(link = "log"))
summary(model_a)

# calculate alpha and betas
sum_of_dev_a <- sum(exp(coef(model_a)[1:10]))
betas_a <- exp(coef(model_a)[1:10]) / sum_of_dev_a
alphas_a <- exp(coef(model_a)[11:19]) * sum_of_dev_a

# Predict  new data
triangle_A_tab_pred$Claims <- predict(model_a, newdata = triangle_A_tab_pred, type = "response")

# Compare full triangle
glm_triangle_A <- rbind(triangle_A_tab_train, triangle_A_tab_pred) %>%
  dcast(origin ~ dev, value.var = "Claims")
glm_triangle_A
triangle_A_Mack_incremental-glm_triangle_A

# Model B
# Format data to tabular
triangle_B_tab <- triangle_B_inc %>% melt(id.vars = "N#")
colnames(triangle_B_tab) <- c("origin", "dev", "Claims")
triangle_B_tab$origin <- as.factor(triangle_B_tab$origin)
triangle_B_tab_train <- triangle_B_tab[!is.na(triangle_B_tab$Claims), ]
triangle_B_tab_pred <- triangle_B_tab[is.na(triangle_B_tab$Claims), ]

# Fit model
model_b <- glm(Claims ~ dev + origin - 1, data = triangle_B_tab_train, family = poisson(link = "log"))
summary(model_b)

# Calculate alpha and beta
sum_of_dev_b <- sum(exp(coef(model_b)[1:9]))
betas_b <- exp(coef(model_b)[1:9]) / sum_of_dev_b
alphas_b <- exp(coef(model_b)[10:18]) * sum_of_dev_b

# Predict new data
triangle_B_tab_pred$Claims <- predict(model_b, newdata = triangle_B_tab_pred, type = "response")

# Compare full triangle
glm_triangle_B <- rbind(triangle_B_tab_train, triangle_B_tab_pred) %>%
  dcast(origin ~ dev, value.var = "Claims")
triangle_B_Mack_incremental-glm_triangle_B

# Assignment 4
esstimate_phi <- function(triangle, model){
  # Calculate the Pearson residuals
  pearson_res <- residuals(model, type = "pearson")
  
  # Calculate the Pearson Chi-squared statistic
  pearson_chi_sq <- sum(pearson_res^2)
  
  # Calculate the degrees of freedom
  n <- nrow(triangle) # Total number of observations
  r <- length(coef(model)) - 1 # Total number of parameters minus constraints
  
  # Estimate the overdispersion parameter phi
  phi <- pearson_chi_sq / (n - r)
  return(phi)
}

# Calculate the Pearson residuals
phi_A <- esstimate_phi(triangle_A_tab_train, model_a)
phi_B <- esstimate_phi(triangle_B_tab_train, model_b)


# Assignment 5
simulate_claim <- function(base_table,alphas, betas, phi){
  # Simulate the number of claims
  triangle_sim <- base_table
  for (i in 1:length(triangle_sim$Claims)){
    origin <- triangle_sim$origin[i]
    dev <- triangle_sim$dev[i]
    triangle_sim$Claims[i] <- rnbinom(1, size = (alphas[origin] * betas[dev]) / (phi - 1), mu = alphas[origin] * betas[dev])
  }
  return(triangle_sim)
}
simulate_claim(triangle_A_tab_train, alphas_a, betas_a, phi_A)

# Simulate 10 times and take estimates of alpha and beta
alphas_a_sim <- matrix(0, 10, 9)
betas_a_sim <- matrix(0, 10, 10)
phi_a_sim <- rep(0, 10)
for (i in 1:10){
  triangle_sim <- simulate_claim(triangle_A_tab_train, alphas_a, betas_a, phi_A)
  model_a_sim <- glm(Claims ~ dev + origin - 1, data = triangle_sim, family = poisson(link = "log"))
  sum_of_dev_a <- sum(exp(coef(model_a_sim)[1:10]))
  alphas_a_sim[i,] <- exp(coef(model_a_sim)[11:19]) * sum_of_dev_a
  betas_a_sim[i,] <- exp(coef(model_a_sim)[1:10]) / sum_of_dev_a
  phi_a_sim[i] <- esstimate_phi(triangle_sim, model_a_sim)
}


# Assignment 6
msep <- rep(0, 10)
for (i in 1:10) {
  triangle_sim <- simulate_claim(triangle_A_tab, alphas_a, betas_a, phi_A) %>% filter(!is.na(Claims))
  # Take out bottom of triangle
  triangle_sim_train <- triangle_sim %>% filter(as.numeric(dev) + as.numeric(origin) <= 11)
  triangle_sim_pred <- triangle_sim %>% filter(as.numeric(dev) + as.numeric(origin) > 11)
  model_a_sim <- glm(Claims ~ dev + origin - 1, data = triangle_sim_train, family = poisson(link = "log"))

  # Evaluate predictions
  triangle_sim_pred$pred <- predict(model_a_sim, newdata = triangle_sim_pred, type = "response")
  msep[i] <- mean((triangle_sim_pred$Claims - triangle_sim_pred$pred)^2)
}
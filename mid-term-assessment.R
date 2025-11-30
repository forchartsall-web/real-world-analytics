# EDA_2.R - Converted from EDA_2.Rmd
# Complete Exploratory Data Analysis with transformations and aggregation models

# 0. Setup ----
required_pkgs <- c("readr","dplyr","ggplot2","GGally","corrplot",
                   "car","MASS","gridExtra","scales")
missing_pkgs <- required_pkgs[!(required_pkgs %in% installed.packages()[,"Package"])]
if (length(missing_pkgs)) {
  install.packages(missing_pkgs, repos = "https://cloud.r-project.org")
}

library(readr); library(dplyr); library(ggplot2); library(GGally); library(corrplot)
library(car); library(MASS); library(gridExtra); library(scales)

data_path <- "ENB.txt"

# 1. Read data ----
raw <- read_table2(data_path, col_names = FALSE, skip = 1, progress = FALSE)
is_num_col <- sapply(raw, is.numeric)
df_num <- raw[, is_num_col]
if (ncol(df_num) > 6) {
  df_num <- df_num[ , (ncol(df_num)-5):ncol(df_num)]
}
colnames(df_num) <- c("X1","X2","X3","X4","X5","Y")
df <- as.data.frame(df_num)
dim(df)
head(df)

# 2. Basic summary / missing values / types ----
summary_stats <- summary(df)
print(summary_stats)

# 3. Univariate plots (distribution) ----
plot_list <- list()
for (v in names(df)) {
  p_hist <- ggplot(df, aes_string(x = v)) +
    geom_histogram(aes(y = ..density..), bins = 30, fill = "steelblue", color = "white") +
    geom_density(alpha = 0.3, color = "red") +
    labs(title = paste("Histogram & density -", v)) +
    theme_minimal()
  p_box <- ggplot(df, aes_string(y = v)) +
    geom_boxplot(fill = "orange", alpha = 0.7) +
    labs(title = paste("Boxplot -", v)) +
    theme_minimal()
  plot_list[[paste0(v,"_hist")]] <- p_hist
  plot_list[[paste0(v,"_box")]] <- p_box
  print(p_hist)
  print(p_box)
}

# 4. Pairwise relationships ----
GGally::ggpairs(df, columns = 1:6, progress = FALSE)

# Bivariate scatter plots with regression lines
src <- df
if (ncol(src) == 7) src <- src[, 2:7, drop = FALSE]
if (is.null(colnames(src)) || !all(colnames(src) %in% c("X1","X2","X3","X4","X5","Y"))) {
  colnames(src) <- c("X1","X2","X3","X4","X5","Y")
}
op <- par(mfrow = c(2, 3), mar = c(4, 4, 2, 1))
plot(src[,"X1"], src[,"Y"], xlab = "X1: Living room temp (°C)", ylab = "Y: Appliances (Wh)",
     pch = 19, col = rgb(0,0,1,0.4), main = "X1 vs Y")
abline(lm(Y ~ X1, data = as.data.frame(src)), col = "red", lwd = 2)
plot(src[,"X2"], src[,"Y"], xlab = "X2: Living room humidity (%)", ylab = "Y: Appliances (Wh)",
     pch = 19, col = rgb(0,0,1,0.4), main = "X2 vs Y")
abline(lm(Y ~ X2, data = as.data.frame(src)), col = "red", lwd = 2)
plot(src[,"X3"], src[,"Y"], xlab = "X3: Office temp (°C)", ylab = "Y: Appliances (Wh)",
     pch = 19, col = rgb(0,0,1,0.4), main = "X3 vs Y")
abline(lm(Y ~ X3, data = as.data.frame(src)), col = "red", lwd = 2)
plot(src[,"X4"], src[,"Y"], xlab = "X4: Office humidity (%)", ylab = "Y: Appliances (Wh)",
     pch = 19, col = rgb(0,0,1,0.4), main = "X4 vs Y")
abline(lm(Y ~ X4, data = as.data.frame(src)), col = "red", lwd = 2)
plot(src[,"X5"], src[,"Y"], xlab = "X5: Pressure (mmHg)", ylab = "Y: Appliances (Wh)",
     pch = 19, col = rgb(0,0,1,0.4), main = "X5 vs Y")
abline(lm(Y ~ X5, data = as.data.frame(src)), col = "red", lwd = 2)
par(op)

# 5. Correlation matrix ----
cor_mat <- cor(df, use = "pairwise.complete.obs")
round(cor_mat, 3)
corrplot::corrplot(cor_mat, method = "ellipse", type = "upper", tl.cex = 0.8)

# 6. Multicollinearity checks ----
X <- as.matrix(df[, c("X1","X2","X3","X4","X5")])
eigs <- eigen(cor(X))
cond_number <- sqrt(max(eigs$values) / min(eigs$values))
print(cond_number)
lm_full <- lm(Y ~ X1 + X2 + X3 + X4 + X5, data = as.data.frame(df_num))
vifs <- car::vif(lm_full)
print(vifs)

# Sample data for aggregation (650 rows)
set.seed(226156731)
num_row <- 650
the.data <- as.matrix(read.table("ENB.txt"))
my.data <- the.data[sample(1:nrow(the.data),num_row), 2:7]
colnames(my.data) <- c("X1","X2","X3","X4","X5","Y")
head(my.data)

# Min-max normalization and z-score functions
minmax <- function(x) (x - min(x))/(max(x)-min(x))
unit.z <- function(x) ((x-mean(x))/sd(x))

# Polynomial transformation functions (Shapiro-Wilk optimization)
normality_score_sw <- function(x, n) {
  x_t <- x ^ n
  x_t <- x_t[is.finite(x_t)]
  if (length(unique(x_t)) < 3) return(NA_real_)
  shapiro.test(x_t)$p.value
}

find_n_shapiro <- function(x, n_grid = seq(0, 2, by = 0.05)) {
  scores <- sapply(n_grid, function(n) normality_score_sw(x, n))
  valid <- !is.na(scores)
  if (!any(valid)) stop("No valid n in the grid (all transforms constant or invalid).")
  best_i <- which.max(scores[valid])
  list(
    n_opt = n_grid[valid][best_i],
    p_opt = scores[valid][best_i],
    grid = n_grid,
    scores = scores
  )
}

# Transform variables
I <- c('X1', 'X2', 'X3', 'X4', 'X5','Y')
variables_to_transform <- my.data[,I]
head(variables_to_transform)

res_swY <- find_n_shapiro(variables_to_transform[,"Y"])
variables_to_transform[,"Y"] <- variables_to_transform[,"Y"]^res_swY$n_opt
res_swX1 <- find_n_shapiro(variables_to_transform[,"X1"])
variables_to_transform[,"X1"] <- variables_to_transform[,"X1"]^res_swX1$n_opt
res_swX2 <- find_n_shapiro(variables_to_transform[,"X2"])
variables_to_transform[,"X2"] <- variables_to_transform[,"X2"]^res_swX2$n_opt
res_swX3 <- find_n_shapiro(variables_to_transform[,"X3"])
variables_to_transform[,"X3"] <- variables_to_transform[,"X3"]^res_swX3$n_opt
res_swX4 <- find_n_shapiro(variables_to_transform[,"X4"])
variables_to_transform[,"X4"] <- variables_to_transform[,"X4"]^res_swX4$n_opt
res_swX5 <- find_n_shapiro(variables_to_transform[,"X5"])
variables_to_transform[,"X5"] <- variables_to_transform[,"X5"]^res_swX5$n_opt
head(variables_to_transform)

# Save transformed data
data.transformed <- variables_to_transform
write.table(data.transformed, "balaji-transformed.txt")

# Load aggregation functions (requires AggWaFit718.R in working directory)
source("AggWaFit718.R")

# Prepare data for aggregation models
data.transformed_copy <- data.transformed
epsilon <- 1e-6
data.transformed_copy[data.transformed_copy == 0] <- epsilon

# Fit models
fit.QAM(data.transformed_copy, output.1="WAM.txt", stats.1 = "WAM-stats.txt")
fit.QAM(data.transformed_copy, output.1="powermean0.5.txt", stats.1 = "powermean-stats0.5.txt", g=PM05, g.inv=invPM05)
fit.QAM(data.transformed_copy, output.1="powermeanQM.txt", stats.1 = "powermean-statsQM.txt", g=QM, g.inv=invQM)
fit.QAM(data.transformed_copy, output.1="powermeanHM.txt", stats.1 = "powermean-statsHM.txt", g=HM, g.inv=invHM)
fit.QAM(data.transformed_copy, output.1="powermeanGM.txt", stats.1 = "powermean-statsGM.txt", g=GM, g.inv=invGM)
fit.OWA(data.transformed_copy, output.1="OWA.txt", stats.1 = "OWA-stats.txt")
fit.choquet(data.transformed_copy, output.1="choquet.txt", stats.1 = "choquet-stats.txt")

# Example prediction on new data
new.data <- matrix(c(23.5, 35.87125, 24.89, 32.93, 758.55), nrow = 1)
colnames(new.data) <- c("X1", "X2", "X3", "X4", "X5")

new_input_to_transform <- new.data
new_input_to_transform[,"X1"] <- new_input_to_transform[,"X1"]^res_swX1$n_opt
new_input_to_transform[,"X2"] <- new_input_to_transform[,"X2"]^res_swX2$n_opt
new_input_to_transform[,"X3"] <- new_input_to_transform[,"X3"]^res_swX3$n_opt
new_input_to_transform[,"X4"] <- new_input_to_transform[,"X4"]^res_swX4$n_opt
new_input_to_transform[,"X5"] <- new_input_to_transform[,"X5"]^res_swX5$n_opt

weights_QM <- c(0.934071643855891, 0, 0.0659283561441099, 0, 0)
result <- PM(new_input_to_transform, weights_QM, -1)
result_scaled <- result^(1/res_swY$n_opt)
print(result_scaled)

cat("Script completed successfully!\n")

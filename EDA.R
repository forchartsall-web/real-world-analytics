# eda_ENB.R
# Exploratory Data Analysis for ENB.txt
# Assumptions: first 5 fields are predictors X1..X5, last field is response Y.
# File path:
data_path <- "ENB.txt"

# --- 0. Setup ----
required_pkgs <- c("readr","dplyr","ggplot2","GGally","corrplot","car","MASS","gridExtra","scales")
missing_pkgs <- required_pkgs[!(required_pkgs %in% installed.packages()[,"Package"])]
if(length(missing_pkgs)) install.packages(missing_pkgs, repos = "https://cloud.r-project.org")
library(readr); library(dplyr); library(ggplot2); library(GGally); library(corrplot)
library(car); library(MASS); library(gridExtra); library(scales)

# --- 1. Read data ----
# The file appears to be whitespace-separated with optional quotes for ID in column 1.
# We'll try to read robustly by ignoring a possible first ID column and taking numeric columns.
raw <- read_table2(data_path, col_names = FALSE, skip=1, progress = FALSE)

# If first column is an ID (non-numeric), drop it. Keep only numeric columns.
is_num_col <- sapply(raw, is.numeric)
df_num <- raw[, is_num_col]
# If there are >6 numeric columns, keep the last 6 (common format: 5 predictors + Y).
if(ncol(df_num) > 6) {
  df_num <- df_num[ , (ncol(df_num)-5):ncol(df_num)]
}
# Name columns X1..X5 and Y
colnames(df_num) <- c("X1","X2","X3","X4","X5","Y")
df <- as.data.frame(df_num)

# Quick check
cat("Data dimensions:", dim(df), "\n")
print(head(df))

# --- 2. Basic summary / missing values / types ----
summary_stats <- summary(df)
print(summary_stats)
cat("Missing values per column:\n")
print(sapply(df, function(x) sum(is.na(x))))

# --- 3. Univariate plots (distribution) ----
# Create histogram + density + boxplot for each variable and save to files
plot_list <- list()
for(v in names(df)) {
  p_hist <- ggplot(df, aes_string(x = v)) +
    geom_histogram(aes(y = ..density..), bins = 30) +
    geom_density(alpha = 0.3) +
    labs(title = paste("Histogram & density -", v)) +
    theme_minimal()
  p_box <- ggplot(df, aes_string(y = v)) +
    geom_boxplot() + labs(title = paste("Boxplot -", v)) + theme_minimal()
  # save both to list
  plot_list[[paste0(v,"_hist")]] <- p_hist
  plot_list[[paste0(v,"_box")]]  <- p_box
  # Save files (png)
  ggsave(filename = paste0("plot_", v, "_hist.png"), plot = p_hist, width=6, height=4)
  ggsave(filename = paste0("plot_", v, "_box.png"),  plot = p_box,  width=4, height=4)
}

# Combine a few for quick view (first 3 pairs)
pdf("univariate_plots_sample.pdf", width=11, height=8.5)
grid.arrange(plot_list$X1_hist, plot_list$X1_box,
             plot_list$X2_hist, plot_list$X2_box,
             plot_list$X3_hist, plot_list$X3_box, ncol = 2)
dev.off()

cat("Univariate plots saved as plot_<var>_hist.png, plot_<var>_box.png and univariate_plots_sample.pdf\n")

# --- 4. Pairwise relationships ----
# Scatterplot matrix with linear smoothers
png("pairs_scatter_matrix.png", width=1200, height=1200)
GGally::ggpairs(df, columns = 1:6, progress = FALSE)    # includes Y
dev.off()
cat("Scatterplot matrix saved to pairs_scatter_matrix.png\n")

# --- 5. Correlation matrix ----
cor_mat <- cor(df, use = "pairwise.complete.obs")
print(round(cor_mat, 3))
# Visualize correlation matrix
png("corrplot.png", width=800, height=800)
corrplot::corrplot(cor_mat, method = "ellipse", type = "upper", tl.cex = 0.8)
dev.off()
cat("Correlation plot saved to corrplot.png\n")

# --- 6. Multicollinearity checks ----
# 6A: Condition number (eigen-based)
X <- as.matrix(df[, c("X1","X2","X3","X4","X5")])
eigs <- eigen(cor(X))
cond_number <- sqrt(max(eigs$values)/min(eigs$values))
cat("Condition number (sqrt(lambda_max/lambda_min)):", round(cond_number,3), "\n")

# 6B: VIFs from linear model
lm_full <- lm(Y ~ X1 + X2 + X3 + X4 + X5, data = df)
vifs <- car::vif(lm_full)
print(vifs)

# Save VIF table
write.csv(data.frame(variable = names(vifs), VIF = as.numeric(vifs)), "vif_table.csv", row.names = FALSE)
cat("VIF table written to vif_table.csv\n")

# --- 7. Suggested transformations (empirical + Box-Cox for Y) ----
# 7A: Box-Cox for Y (requires positive Y). If Y has non-positive values, skip.
if( all(df$Y > 0) ) {
  bc <- MASS::boxcox(lm_full, plotit = FALSE)
  lambda_est <- bc$x[which.max(bc$y)]
  cat("Box-Cox suggested lambda for Y:", round(lambda_est,3), "\n")
} else {
  lambda_est <- NA
  cat("Y contains non-positive values — skipping Box-Cox on Y.\n")
}

# 7B: Check skewness visually (we'll use simple numeric skew)
skewness_num <- function(x) { m3 <- mean((x-mean(x))^3); s3 <- sd(x)^3; m3/s3 }
skews <- sapply(df, skewness_num)
print(round(skews,3))

# 7C: Candidate transformations:
# - For right-skewed variables: log(x), sqrt(x)
# - For approximately normal already: keep raw
# We'll build df_trans with recommended transforms.
df_trans <- df
transform_notes <- list()
for(v in names(df)) {
  s <- skewness_num(df[[v]])
  if(s > 1) {
    # strongly right skew -> log
    if(all(df[[v]] > 0)) {
      df_trans[[v]] <- log(df[[v]])
      transform_notes[[v]] <- "log"
    } else {
      df_trans[[v]] <- sign(df[[v]]) * log(abs(df[[v]]) + 1)
      transform_notes[[v]] <- "signed_log"
    }
  } else if(s > 0.5) {
    # moderate right skew -> sqrt
    if(all(df[[v]] >= 0)) {
      df_trans[[v]] <- sqrt(df[[v]])
      transform_notes[[v]] <- "sqrt"
    } else {
      df_trans[[v]] <- sign(df[[v]]) * sqrt(abs(df[[v]]))
      transform_notes[[v]] <- "signed_sqrt"
    }
  } else {
    transform_notes[[v]] <- "none"
  }
}
print(transform_notes)

# Save transformed dataset
write.csv(df_trans, "ENB_transformed.csv", row.names = FALSE)
cat("Transformed dataset saved to ENB_transformed.csv\n")

# --- 8. Re-check correlations & VIF on transformed predictors (not transforming Y automatically) ----
X_t <- as.matrix(df_trans[, c("X1","X2","X3","X4","X5")])
cor_mat_t <- cor(cbind(X_t, Y = df_trans$Y), use = "pairwise.complete.obs")
png("corrplot_transformed.png", width=800, height=800)
corrplot::corrplot(cor_mat_t, method = "ellipse", type = "upper", tl.cex = 0.8)
dev.off()
cat("Correlation plot of transformed variables: corrplot_transformed.png\n")

lm_full_t <- lm(Y ~ X1 + X2 + X3 + X4 + X5, data = df_trans)
vifs_t <- car::vif(lm_full_t)
print(vifs_t)
write.csv(data.frame(variable = names(vifs_t), VIF = as.numeric(vifs_t)), "vif_table_transformed.csv", row.names = FALSE)
cat("VIF after transforms written to vif_table_transformed.csv\n")

# --- 9. Optional: If multicollinearity remains high, propose remedies ----
# Print brief suggestions:
cat("\nGuidance:\n")
cat("- VIF > 5 (or >10) indicates problematic multicollinearity. Consider removing or combining predictors,\n")
cat("  or using dimensionality reduction (PCA) or regularized models (Ridge/Lasso).\n")
cat("- If condition number >> 30 consider severe collinearity.\n")
cat("- Consider centering (subtract mean) or scaling; interaction terms only if justified.\n")

# --- 10. Save workspace summary ----
sink("eda_summary.txt")
cat("Summary statistics:\n"); print(summary_stats)
cat("\nCorrelation matrix:\n"); print(round(cor_mat,3))
cat("\nCondition number:", round(cond_number,3), "\n")
cat("\nVIFs (original):\n"); print(vifs)
cat("\nVIFs (transformed predictors):\n"); print(vifs_t)
cat("\nTransform notes:\n"); print(transform_notes)
sink()
cat("EDA text summary saved to eda_summary.txt\n")

# End of script


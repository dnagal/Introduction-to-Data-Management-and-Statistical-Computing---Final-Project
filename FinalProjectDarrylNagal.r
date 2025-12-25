#Final Project R Code - BIOSTAT 203A
#Darryl Nagal

#Uploading the dataset
long_term <- read.csv("~/Downloads/long-term-care-facility-staffing.csv")
install.packages("dplyr")
library(dplyr)

#Quick summary of the dataset
str(long_term)

long_term %>%
    count(TYPE_CNTRL)

#Data dimensions
dim(long_term)

#Checking if the dataset contains any missing values
sum(is.na(long_term))

#Checking the normality of the distributions for the healthcare workers of interest
#Histograms
#NA productive hours histogram
library(ggplot2) 

ggplot(long_term, aes(x = PRDHR_NA)) +
  geom_histogram(fill = "skyblue", color = "black", bins = 30) +
  facet_wrap(~ TYPE_CNTRL, scales = "free_y") +
  labs(
    title = "NA Productivity Hours by Ownwership Type",
    x = "NA Productivity Hours",
    y = "Count"
  ) +
  theme_minimal()

#LVN productive hours histogram
ggplot(long_term, aes(x = PRDHR_LVN)) +
  geom_histogram(fill = "skyblue", color = "black", bins = 30) +
  facet_wrap(~ TYPE_CNTRL, scales = "free_y") +
  labs(
    title = "LVN Productivity Hours by Ownwership Type",
    x = "LVN Productivity Hours",
    y = "Count"
  ) +
  theme_minimal()

#RN productive hours histogram
ggplot(long_term, aes(x = PRDHR_RN)) +
  geom_histogram(fill = "skyblue", color = "black", bins = 30) +
  facet_wrap(~ TYPE_CNTRL, scales = "free_y") +
  labs(
    title = "RN Productivity Hours by Ownwership Type",
    x = "RN Productivity Hours",
    y = "Count"
  ) +
  theme_minimal()

#Nurse Management productive hours histogram
ggplot(long_term, aes(x = PRDHR_MGT)) +
  geom_histogram(fill = "skyblue", color = "black", bins = 30) +
  facet_wrap(~ TYPE_CNTRL, scales = "free_y") +
  labs(
    title = "Nurse Management Productivity Hours by Ownwership Type",
    x = "Nurse Management Productivity Hours",
    y = "Count"
  ) +
  theme_minimal()

#Using Q-Q plots to also examine normality/skewnewss
#NA QQ plot
ggplot(long_term, aes(sample = PRDHR_NA)) +
  stat_qq(color = "skyblue") +
  stat_qq_line(color = "red") +
  facet_wrap(~ TYPE_CNTRL, scales = "free") +
  labs(
    title = "Q-Q Plot of NA Productivity Hours by Ownership Type",
    x = "Theoretical Quantiles",
    y = "Sample Quantiles"
  ) +
  theme_minimal()

#LVN QQ plot
ggplot(long_term, aes(sample = PRDHR_LVN)) +
  stat_qq(color = "skyblue") +
  stat_qq_line(color = "red") +
  facet_wrap(~ TYPE_CNTRL, scales = "free") +
  labs(
    title = "Q-Q Plot of LVN Productivity Hours by Ownership Type",
    x = "Theoretical Quantiles",
    y = "Sample Quantiles"
  ) +
  theme_minimal()

#RN QQ plot 
ggplot(long_term, aes(sample = PRDHR_RN)) +
  stat_qq(color = "skyblue") +
  stat_qq_line(color = "red") +
  facet_wrap(~ TYPE_CNTRL, scales = "free") +
  labs(
    title = "Q-Q Plot of RN Productivity Hours by Ownership Type",
    x = "Theoretical Quantiles",
    y = "Sample Quantiles"
  ) +
  theme_minimal()

#MGT QQ plot
ggplot(long_term, aes(sample = PRDHR_MGT)) +
  stat_qq(color = "skyblue") +
  stat_qq_line(color = "red") +
  facet_wrap(~ TYPE_CNTRL, scales = "free") +
  labs(
    title = "Q-Q Plot of Nurse Management Productivity Hours by Ownership Type",
    x = "Theoretical Quantiles",
    y = "Sample Quantiles"
  ) +
  theme_minimal()

#Using a spearman's rank correlation as a result of the skewed distributions 
cor_matrix <- cor(long_term[, c("PRDHR_NA", "PRDHR_LVN", "PRDHR_RN", "PRDHR_MGT")],
                  use = "pairwise.complete.obs",
                  method = "spearman")

cor_matrix

#printing the correlation table 
# Calculate Spearman correlation matrix
cor_matrix <- cor(
  long_term[, c("PRDHR_NA", "PRDHR_LVN", "PRDHR_RN", "PRDHR_MGT")],
  use = "pairwise.complete.obs",
  method = "spearman"
)

# install + load gt 
install.packages("gt")
library(gt)

# converting correlation matrix to a data frame and keep row names
cor_df <- as.data.frame(cor_matrix)
cor_df <- cbind(Variable = rownames(cor_df), cor_df)
rownames(cor_df) <- NULL

# building the GT table
cor_table <- gt(cor_df) |>
  tab_header(
    title = "Spearman Correlation Matrix",
    subtitle = "Productive Hours by Staff Type"
  ) |>
  fmt_number(
    columns = -Variable,
    decimals = 2
  ) |>
  tab_options(
    table.font.size = "large",
    data_row.padding = px(3),
    table.width = pct(80)
  ) |>
  cols_label(
    PRDHR_NA = "NA Hours",
    PRDHR_LVN = "LVN Hours",
    PRDHR_RN = "RN Hours",
    PRDHR_MGT = "Management Hours"
  )

# saving the table as a PNG
gtsave(
  cor_table,
  filename = "/Users/darrylnagal/Desktop/my_correlation_table3.png"
)

#With a correlation found, performing a KMO to determine if a PCA is appropriate
library(psych)

KMO(cor_matrix)
#overall KMO score determines that a PSA is appropriate

#Principal Component Analysis using the four different nursing productive hour categories
pca_result <- prcomp(long_term[, c("PRDHR_NA", "PRDHR_LVN", "PRDHR_RN", "PRDHR_MGT")],
                     scale. = TRUE)

library(ggfortify)
autoplot(pca_result, data = long_term, color = 'TYPE_CNTRL')

summary(pca_result)

pca_result$rotation

autoplot(pca_result, data = long_term, color = 'TYPE_CNTRL',
         loadings = TRUE, loadings.label = TRUE) + ggtitle("PCA of Nursing Productivity Measures")

#using p to create the png image
p <- autoplot(pca_result, data = long_term, color = 'TYPE_CNTRL',
         loadings = TRUE, loadings.label = TRUE) + ggtitle("PCA of Nursing Productivity Measures")

ggsave(
  "pca_plot.png",
  p,
  width = 7,
  height = 6,
  dpi = 300
)

#The PCA result will now be used to determine which of the focuses are prioritized
#by each ownership type 

#Printing the pca_result$rotation table for paper
# Extract PCA loadings (rotation matrix)
rotation_df <- as.data.frame(pca_result$rotation)

# Keep variable names
rotation_df <- cbind(Variable = rownames(rotation_df), rotation_df)
rownames(rotation_df) <- NULL

# Load gt
library(gt)

# Create a GT table
rotation_table <- gt(rotation_df) |>
  tab_header(
    title = "PCA Component Loadings",
    subtitle = "Loadings for PRDHR_NA, PRDHR_LVN, PRDHR_RN, and PRDHR_MGT"
  ) |>
  fmt_number(
    columns = c(PC1, PC2, PC3, PC4),   # prcomp outputs 4 PCs for 4 variables
    decimals = 3                       # PCA loadings usually shown to 3 decimals
  ) |>
  tab_options(
    table.font.size = "large",
    data_row.padding = px(3),
    table.width = pct(80)
  ) |>
  cols_label(
    Variable = "Variable",
    PC1 = "PC1 Loading",
    PC2 = "PC2 Loading",
    PC3 = "PC3 Loading",
    PC4 = "PC4 Loading"
  )

# Save it as a PNG
gtsave(
  rotation_table,
  filename = "/Users/darrylnagal/Desktop/PCA_Loadings_Table2.png"
)

#Checking the shape of the distribution for each principal component
#Histogram for pc1
png("pc1_histogram.png", width = 800, height = 600)

hist(pca_result$x[,1],
     main = "Histogram of PC1",
     xlab = "pca_result$x[,1]",
     col = "skyblue")

dev.off()

#Q-Q plot for pc1
qqnorm(pca_result$x[,1]); qqline(pca_result$x[,1])

#Histogram for pc2
hist(pca_result$x[,2],
     main = "Histogram of PC2",
     xlab = "pca_result$x[,2]",
     col = "skyblue")

#Q-Q plot for pc2
png("qqplot_pc2.png", width = 800, height = 600)  # open PNG device

qqnorm(
  pca_result$x[,2],
  main = "Q-Q Plot for PC2",
  xlab = "Theoretical Quantiles",
  ylab = "Sample Quantiles"
)
qqline(pca_result$x[,2], col = "red")

dev.off()

#Given the skewed distributions of both pc1 and pc2, perform a Kruskal-Wallis test
pca_result <- prcomp(long_term[, c("PRDHR_NA", "PRDHR_LVN", "PRDHR_RN", "PRDHR_MGT")], scale. = TRUE, center = TRUE)
pca_scores <- as.data.frame(pca_result$x)

pca_scores$ownership <- long_term$TYPE_CNTRL

kruskal.test(PC1 ~ ownership, data = pca_scores) #pc1 result
kruskal.test(PC2 ~ ownership, data = pca_scores) #pc2 result
#both have a p-value that are significant

#Post-hoc testing after significant Kruskal-Wallis
pairwise_PC1 <- pairwise.wilcox.test(pca_scores$PC1, pca_scores$ownership, p.adjust.method = "bonferroni")
pairwise_PC2 <- pairwise.wilcox.test(pca_scores$PC2, pca_scores$ownership, p.adjust.method = "bonferroni")

pairwise_PC1
#investor owned facilities are significantly different from not-for-profit in regard to pc1
#pc1 refers more to direct care services, thus there is a significant difference between 
#investor owned and not-for-profit facilities in regard to direct-care productivity hours
pairwise_PC2
#pc2 refers more to the productivity hours of nurse management and supervisors
#there is a significant difference found betewen investor-owned facilities and not-for-profits
#significant difference also found between investor-owned and governmental facilities 

#printing out PCA summary results
head(pca_result$x)

aggregate(pca_result$x, by = list(ownership = pca_scores$ownership), mean)
#loadings are negative given that the horizontal arrow moves left and the vertical arrow goes down
#regarding pc1, not-for-profit has the most positive score which suggests it has the lowest 
#productive hours regarding direct-care, add to the fact that a significant result was found 
#that not-for-profit significantly differs from investor-owned in this category
#governmental has the most negative value which suggests it is the most likely to have 
#more productivity hours in regard to direct-care

#regarding pc2, governmental once again has the most negative score which means that 
#it has the greatest focus also in regard to management productivity compared to the other
#ownership types. The owernship type least aligned with management productivity is investor-owned.
#Add in that there are significant differences between investor owned facilities compared to both 
#governmental and not-for-profit

#printing out the PCA summary results table for paper
pca_summary <- aggregate(
  pca_result$x,
  by = list(ownership = pca_scores$ownership),
  mean
)

# Load gt
library(gt)

# Build publication-ready table
pca_summary_table <- gt(pca_summary) |>
  tab_header(
    title = "Mean PCA Scores by Ownership Type",
    subtitle = "Group Means for Principal Components"
  ) |>
  cols_label(
    ownership = "Ownership Type",
    PC1 = "Mean PC1 Score",
    PC2 = "Mean PC2 Score",
    PC3 = "Mean PC3 Score",
    PC4 = "Mean PC4 Score"
  ) |>
  fmt_number(
    columns = c(PC1, PC2, PC3, PC4),
    decimals = 3
  ) |>
  tab_options(
    table.font.size = "large",
    data_row.padding = px(3),
    table.width = pct(80)
  )

# Save as PNG
gtsave(
  pca_summary_table,
  filename = "/Users/darrylnagal/Desktop/PCA_Group_Means_Table2.png"
)
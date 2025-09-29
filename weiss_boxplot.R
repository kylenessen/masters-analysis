# R SCRIPT FOR RE-ANALYZING MONARCH BUTTERFLY HABITAT DATA
# Data Source: Weiss, S. B., et al. (1991). Forest Canopy Structure at
# Overwintering Monarch Butterfly Sites. Conservation Biology, 5(2), 165-175.
# Table 1, Page 168.

# --------------------------------------------------------------------------
## Section 0: Install and Load Required Libraries
# --------------------------------------------------------------------------
# This script requires the 'dplyr' for data manipulation, 'ggplot2' for
# plotting, 'car' for Levene's test, and 'MASS' for LDA.
# If you don't have them, uncomment and run the line below:
# install.packages(c("dplyr", "ggplot2", "car", "MASS"))

library(dplyr)
library(ggplot2)
library(car)
library(MASS)

# --------------------------------------------------------------------------
## Section 1: Transcribe and Load Data from Table 1
# --------------------------------------------------------------------------
# The data from Table 1 is transcribed into a text string and read into a
# data frame. This makes the script fully reproducible.

table_data <- "
Site,Status,ISF,DSF,Winter_DSF
Ellwood_Main,Perm,.214,.205,.182
Cementerio,Perm,.192,.138,.039
Cementerio_2,Perm,.207,.162,.037
Tecolote,Perm,.198,.227,.183
Arroyo_10m_1,Perm,.190,.177,.101
Arroyo_10m_2,Perm,.226,.249,.202
Arroyo_30m_1,Trans,.215,.214,.059
Arroyo_20m,Trans,.191,.197,.070
Arroyo_30m_2,Trans,.232,.250,.150
W_Ellwood,Trans,.214,.216,.068
N_Ellwood_1988,Trans,.261,.277,.127
N_Ellwood,Trans,.220,.227,.208
E_Ellwood,Trans,.201,.071,.048
Ellwood_Ck_Bot,Trans,.197,.203,.122
Ellwood_2p,Trans,.187,.180,.050
Ellwood_10d,Trans,.197,.250,.168
Ellwood_12g,Trans,.160,.193,.126
Coronado,Trans,.282,.297,.309
Llano_Ave,Trans,.230,.268,.351
Wilcox_Mesa,Trans,.283,.274,.230
Honda_Valley,Trans,.215,.279,.177
Butterfly_Lane,Trans,.225,.225,.155
Arroyo_70m,Unocc,.110,.162,.314
Arroyo_50m_a,Unocc,.196,.159,.108
Arroyo_50m,Unocc,.305,.118,.059
Arroyo_60m,Unocc,.396,.298,.096
Forest_20m,Unocc,.137,.129,.071
Forest_40m,Unocc,.265,.242,.216
Music_Acad_NW,Former,.308,.366,.347
Music_Acad_S,Former,.265,.483,.566
Music_Acad_E,Former,.409,.316,.251
Music_Acad_Int,Former,.199,.235,.300
Thinned_Grove,Former,.476,.593,.506
"

monarch_sites <- read.csv(text = table_data, header = TRUE)

# --------------------------------------------------------------------------
## Section 2: Data Preparation and Grouping
# --------------------------------------------------------------------------
# Create a new 'Group' variable to consolidate the statuses.
# The 'case_when' function from dplyr is perfect for this.
monarch_sites <- monarch_sites %>%
  mutate(
    Group = case_when(
      Status == "Perm" ~ "Permanent",
      Status == "Trans" ~ "Transient",
      Status %in% c("Unocc", "Former") ~ "Unoccupied/Former"
    )
  )

# Convert the new 'Group' column to a factor with a logical order for plots
monarch_sites$Group <- factor(
  monarch_sites$Group,
  levels = c("Permanent", "Transient", "Unoccupied/Former")
)

print("First few rows of the prepared data frame:")
head(monarch_sites)
cat("\n")

# --------------------------------------------------------------------------
## Section 3: Descriptive Statistics
# --------------------------------------------------------------------------
# Calculate summary statistics for each group to see the central tendency
# and spread of the data.

summary_stats <- monarch_sites %>%
  group_by(Group) %>%
  summarise(
    Count = n(),
    Mean_ISF = mean(ISF),
    SD_ISF = sd(ISF),
    Mean_DSF = mean(DSF),
    SD_DSF = sd(DSF),
    Mean_Winter_DSF = mean(Winter_DSF),
    SD_Winter_DSF = sd(Winter_DSF)
  )

print("--- DESCRIPTIVE STATISTICS BY GROUP ---")
print(summary_stats)
cat("\n")


# --------------------------------------------------------------------------
## Section 4: Visualization
# --------------------------------------------------------------------------
# Box plots provide a clear visual of the distribution for each group.
# A scatter plot helps visualize the relationship between ISF and DSF.

# Box plot for Indirect Site Factor (ISF)
isf_plot <- ggplot(monarch_sites, aes(x = Group, y = ISF, fill = Group)) +
  geom_boxplot() +
  labs(title = "Indirect Site Factor (ISF) by Occupancy Status",
       x = "Site Status",
       y = "ISF Value") +
  theme_minimal()

# Box plot for Direct Site Factor (DSF)
dsf_plot <- ggplot(monarch_sites, aes(x = Group, y = DSF, fill = Group)) +
  geom_boxplot() +
  labs(title = "Annual Direct Site Factor (DSF) by Occupancy Status",
       x = "Site Status",
       y = "DSF Value") +
  theme_minimal()

# Scatter plot to replicate Figure 2a
scatter_plot <- ggplot(monarch_sites, aes(x = ISF, y = DSF, color = Group)) +
  geom_point(size = 3, alpha = 0.8) +
  labs(title = "Site Factors of Monarch Overwintering Sites",
       subtitle = "Replication of Weiss et al. (1991), Figure 2a",
       x = "Indirect Site Factor (ISF)",
       y = "Annual Direct Site Factor (DSF)") +
  theme_minimal() +
  scale_color_brewer(palette = "Set1")

# Print the plots
print(isf_plot)
print(dsf_plot)
print(scatter_plot)

# --------------------------------------------------------------------------
## Section 5: Inferential Statistics - Hypothesis Testing
# --------------------------------------------------------------------------

### --- 5a. Analysis of Variance (ANOVA) ---
# To test if the mean site factors are different across groups.
print("--- ANOVA: COMPARING GROUP MEANS ---")

# ANOVA for ISF
isf_aov <- aov(ISF ~ Group, data = monarch_sites)
print("ANOVA Results for ISF:")
print(summary(isf_aov))
# Post-hoc test if ANOVA is significant
print("Tukey HSD Post-Hoc Test for ISF:")
print(TukeyHSD(isf_aov))

# ANOVA for DSF
dsf_aov <- aov(DSF ~ Group, data = monarch_sites)
print("ANOVA Results for DSF:")
print(summary(dsf_aov))
# Post-hoc test if ANOVA is significant
print("Tukey HSD Post-Hoc Test for DSF:")
print(TukeyHSD(dsf_aov))
cat("\n")


### --- 5b. Levene's Test ---
# To test if the variance (spread) of site factors is different across groups.
# This directly tests the "narrow range" hypothesis.
print("--- LEVENE'S TEST: COMPARING GROUP VARIANCES ---")

# Levene's Test for ISF
print("Levene's Test Results for ISF:")
print(leveneTest(ISF ~ Group, data = monarch_sites))

# Levene's Test for DSF
print("Levene's Test Results for DSF:")
print(leveneTest(DSF ~ Group, data = monarch_sites))
cat("\n")


# --------------------------------------------------------------------------
## Section 6: Predictive Modeling - Linear Discriminant Analysis (LDA)
# --------------------------------------------------------------------------
# Can we use ISF and DSF to predict a site's status?

print("--- LINEAR DISCRIMINANT ANALYSIS (LDA) ---")

# Build the LDA model
lda_model <- lda(Group ~ ISF + DSF, data = monarch_sites)
print("LDA Model Summary:")
print(lda_model)

# Assess the accuracy of the model on the training data
predictions <- predict(lda_model, monarch_sites)
predicted_groups <- predictions$class

# Create a confusion matrix to see how well the model performed
confusion_matrix <- table(Predicted = predicted_groups, Actual = monarch_sites$Group)
print("Confusion Matrix:")
print(confusion_matrix)

# Calculate overall accuracy
accuracy <- sum(diag(confusion_matrix)) / sum(confusion_matrix)
print(paste("Overall Model Accuracy:", round(accuracy * 100, 2), "%"))

# --- END OF SCRIPT ---

# Load ggplot2 if you haven't already
library(ggplot2)

# Create the updated box plot
isf_boxplot_updated <- ggplot(monarch_sites, aes(x = Group, y = ISF, fill = Group)) +
  geom_boxplot() +
  # Convert the y-axis labels to percentages for clarity
  scale_y_continuous(labels = scales::percent_format(accuracy = 1)) +
  labs(
    y = "Percent Open Sky (ISF)", # Updated, more intuitive axis label
    x = "Site Status",
    caption = "Data adapted from Weiss et al. (1991)" # Added caption to cite the source
  ) +
  theme_minimal() +
  theme(plot.title = element_text(hjust = 0.5)) # Center the plot title

# Print the plot
print(isf_boxplot_updated)



##=============================================================================
##
## This code was written in  RStudio 2022.07.1+554 "Spotted Wakerobin"
## release. Functions and package compatibility may be subject to
## change.
##
##=============================================================================

## Libraries
pkgs <- c(
          "foreign","readr","lavaan","semPlot","psych","apaTables","memisc",
          "car","readxl","knitr","e1071","effsize","ltm","mice",
          "naniar","finalfit","writexl","semTools","MASS","simsem","simr",
          "ordinal","dplyr","tidyr","Hmisc","weights","ggplot2"
          )
for (p in pkgs) {
  if (!require(p, character.only = TRUE)) {
    install.packages(p)
    library(p, character.only = TRUE)
    }
  }

## Load data
Data <- read_excel("Data.xlsx", col_names = TRUE)

## Filter courses (PED115 or PEDBCH)
fag_data <- Data %>% 
  filter(Course == "PEDBCH")

##=============================================================================
##
## Models & plots
##
##=============================================================================

## Average proportions
fag_data %>%
  group_by(Sex, Grade) %>%
  summarise(avg_prop = mean(Proportion)) %>%
  arrange(Sex, desc(avg_prop))
## Likelihood of grade ... by sex
fag_data$is_A <- ifelse(fag_data$Grade == "A", 1, 0)
## Ordinal trend checks
polr_model <- polr(factor(Grade_num) ~ Sex, data = fag_data, weights = Proportion)
summary(polr_model)
## Cumulative model
polr_model <- polr(factor(Grade_num) ~ Sex, data = fag_data, weights = Proportion)
## Create a new data frame for prediction
newdata <- expand.grid(Sex = unique(fag_data$Sex))
## Predict cumulative probabilities
cum_probs <- predict(polr_model, newdata, type = "prob")
## Convert to tidy format for ggplot
cum_probs_df <- cbind(newdata, cum_probs) %>%
  pivot_longer(cols = -Sex, names_to = "Grade", values_to = "Probability")
## True S-plot
cum_probs_cum <- cum_probs_df %>%
  group_by(Sex)  %>%
  arrange(Grade) %>%
  mutate(Cumulative = cumsum(Probability))
cum_probs_cum$Grade <- factor(
  cum_probs_cum$Grade,
  levels = 1:6,
  labels = c("F","E","D","C","B","A"),
  ordered = TRUE
)
ggplot(cum_probs_cum, aes(x = Grade, y = Cumulative, 
                          color = Sex, group = Sex)) +
  geom_smooth(method = "loess", se = FALSE, size = 1.3) +
  labs(
    y = "Cumulative probability",
    x = "Grade"
  ) +
  scale_color_manual(values = c("F" = "darkorange",   
                                "M" = "cyan4")) +
  theme_minimal()

##=============================================================================
##
## END
##
##=============================================================================

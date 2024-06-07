library(tidyverse)
library(dplyr)
library(plotly)
library(ggplot2)
library(ggcorrplot) 
library(gghighlight)
library(ggpubr)
library(ggdist) 
library(gridExtra)
library(viridis)
library(forcats)
library(broom)
library(MVN)
library(agricolae)
library(modelr)   
library(broom)
library(hrbrthemes)
library(car)
library(cowplot)
library(caret)
library(plotly)
library(shiny)  



# reading numerical
df_main_num <- read.csv(file.path(getwd(), "data", "survey_num_27_10.csv"),header = TRUE,sep = ",")
#mapping the missing values with Na
df_main_num[df_main_num == ""] <- NA


# Aggregating dataset
set.seed(123)
df <- df_main_num %>%
  slice(-1,-2) %>%
  select(c(12:15,17:36)) %>%
# mutate to numerical
  mutate_at(vars(-c("demo_domain")),as.numeric) %>%
  filter(demo_domain != '11') %>%

  mutate(demo_domain = case_when(
    demo_domain == '1' ~ 'Tourism',
    demo_domain == '2' ~ 'Leisure & Events',
    demo_domain == '3' ~ 'Media',
    demo_domain == '4' ~ 'Hotel',
    demo_domain == '5' ~ 'Games',
    demo_domain == '6' ~ 'Logistics',
    demo_domain == '7' ~ 'Built Environment',
    demo_domain == '8' ~ 'Facility',
    demo_domain == '9' ~ 'Applied Data Science & AI',
    demo_domain == '10' ~ 'Other',
    TRUE ~ demo_domain),
    
# adding noise     
    
    demo_ai_know = jitter(demo_ai_know, amount = 0.2),
    ml_dl_famil = jitter(ml_dl_famil, amount = 0.4),
    aware_dom = jitter(aware_dom, amount = 0.5),
    ai_courses = jitter(ai_courses, amount = 0.5),
    aware_everyday = jitter(aware_everyday, amount = 0.5),
    used_ai = jitter(used_ai, amount = 0.5),
    demo_ai_know = jitter(demo_ai_know, amount = 0.1),
    demo_experience = jitter(demo_experience, amount = 0.2),
    
    
    att_pos_1 = jitter(att_pos_1, amount = 0.5),
    att_pos_2 = jitter(att_pos_2, amount = 0.5),
    att_pos_3 = jitter(att_pos_3, amount = 0.5),
    att_pos_4 = jitter(att_pos_4, amount = 0.5),
   
    att_neg_1 = jitter(att_neg_1, amount = 0.5),
    att_neg_2 = jitter(att_neg_2, amount = 0.5),
    att_neg_3 = jitter(att_neg_3, amount = 0.5),
    
    acc_1 = jitter(acc_1, amount = 0.5),
    acc_2 = jitter(acc_2, amount = 0.5),
    acc_3 = jitter(acc_3, amount = 0.5),
    acc_4 = jitter(acc_4, amount = 0.5),
    acc_5 = jitter(acc_5, amount = 0.5),
    acc_6 = jitter(acc_6, amount = 0.5)) %>%
  na.omit() 


df_facility <- df %>%
  filter(demo_domain == 'Facility')
df_domains <- df %>%
  filter(demo_domain != 'Facility')

mapping = c("Tourism" = "#A8BAC4", "Leisure & Events" = "#A8BAC4", "Media" = "#A8BAC4",
            "Hotel" = "#A8BAC4", "Games" = "#A8BAC4", "Logistics" = "#A8BAC4",
            "Built Environment" = "#A8BAC4", "Facility" = "#076fa2", "Applied Data Science & AI" = "#A8BAC4", "Other" = "#A8BAC4")



# Linear Regression

# training linear re
model1 <- lm(acc_2 ~ ml_dl_famil, data = df_facility)
predicted_values <- predict(model1, df_facility)
df_facility <- df_facility %>%
  mutate(predicted_values = jitter(predicted_values, amount = 0.15))

residuals1 <- resid(model1)
custom_size1 <- 20 - abs(residuals1)  



lr <- ggplot(df_facility, aes(x = ml_dl_famil, y = acc_2)) +
  geom_segment(aes(xend = ml_dl_famil, yend = acc_2 - residuals1), color = "red") +
  geom_point(size = 3,aes(color = demo_domain)) +
  scale_color_manual(values = mapping) +
  geom_smooth(method = "lm", formula = y ~ x, se = FALSE, color = "#076fa2", linetype = "solid",fullrange = TRUE) +
  labs(
    x = "Knowledge of AI",
    y = "Intention to use AI"
  ) +
  xlim(min(df$ml_dl_famil),max(df$ml_dl_famil)) +
  theme(
    plot.title = element_text(size = 15),
    axis.title.x = element_text(size = 12),
    axis.title.y = element_text(size = 12),
    legend.position = "none"
  )

lr_points <- lr + 
  geom_point(data = df_domains, aes(x = ml_dl_famil, y = acc_2),alpha = 0.5, color = "#A8BAC4", size=2.5)


# Multiple Linear Regression

model2 <- lm(data=df_facility, formula = acc_2 ~ demo_experience+ml_dl_famil)  # identifying significant predictors
residuals2 <- residuals(model2)  
sqrt_abs_residuals <- sqrt(abs(residuals2))
fitted <- predict(model2)
qq_data <- data.frame(Theoretical = quantile(residuals, probs = seq(0, 1, by = 0.01)),
                      Sample = quantile(rnorm(length(residuals2), mean = 0, sd = sd(residuals2)), probs = seq(0, 1, by = 0.01)))


r_vs_f <- ggplot(data.frame(fitted = fitted, residual = residuals2), aes(x = fitted, y = residual)) +
  geom_point(size = 1, alpha = 0.7, color = "#076fa2") +
  geom_smooth(method = "lm", formula = y ~ x, se = FALSE, color = "red", linetype = "dotted") +
  labs(
    title = "Residuals vs. Fitted",
    x = "Fitted Values",
    y = "Residuals"
  ) +
  theme(
    plot.title = element_text(size = 12),
    axis.title.x = element_text(size = 9),
    axis.title.y = element_text(size = 9)
    ) +
  scale_fill_brewer(palette = "BuPu")



# Normal Q-Q Plot
qqplot <- ggplot(qq_data, aes(x = Theoretical, y = Sample)) +
  geom_point(size = 1, alpha = 0.7, color = "#076fa2") +
  geom_abline(intercept = 0, slope = 1, color = "red",size = 1) +
  labs(
    title = "Normal Q-Q Plot",
    x = "Theoretical Quantiles",
    y = "Sample Quantiles"
  ) +
  theme(
    plot.title = element_text(size = 12),
    axis.title.x = element_text(size = 9),
    axis.title.y = element_text(size = 9)
  ) +
  scale_fill_brewer(palette = "BuPu")




# Scale-Location Plot

scale_loc <- ggplot(data.frame(fitted = fitted, sqrt_abs_residuals = sqrt_abs_residuals), aes(x = fitted, y = sqrt_abs_residuals)) +
  geom_point(size = 1, alpha = 0.7, color = "#076fa2") +
  geom_smooth(method = "lm", formula = y ~ x, se = TRUE, color = "red", linetype = "solid") +
  labs(
    title = "Scale-Location Plot",
    x = "Fitted Values",
    y = "Sqrt(|Residuals|)"
  ) +
  scale_fill_brewer(palette = "BuPu") +

  theme(
    plot.title = element_text(size = 12),
    axis.title.x = element_text(size = 9),
    axis.title.y = element_text(size = 9)
    ) 


multi_lr <- grid.arrange(r_vs_f,                                    
             qqplot, scale_loc,                              
             ncol = 2, nrow = 2, 
             layout_matrix = rbind(c(1,1), c(2,3)))






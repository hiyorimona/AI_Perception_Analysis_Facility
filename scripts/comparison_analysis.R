library(tidyverse)
library(ggplot2)
library(forcats)
library(dplyr)
library(broom)
library(MVN)
library(agricolae)
library(ggcorrplot) 
library(pwr)
library(BSDA)
library(gridExtra)
require(moonBook)
require(webr)
library(nortest)
library(pwr)
library(MKpower)
library(gridExtra)
library(ggdist)
library(pwrss)
library(TOSTER)






# Reading dataset
df_main_num <- read.csv("C:/Users/NITRO/Desktop/2023-24a-fai2-adsai-SimonaDimitrova222667/scripts/survey_num_27_10.csv",header = TRUE,sep = ",")
df_main_num[df_main_num == ""] <- NA

# Aggregating facility data
df_facility <- df_main_num %>%
  slice(-1:-2) %>%
  select(c(12:15,17:36)) %>% 
  
  filter(demo_domain == '8') %>%
  mutate_at(vars(-c("demo_gender","demo_domain", "demo_experience")),as.integer) %>%
  na.omit()

# 2 Sample T-test
t_test <- t.test(df_facility$used_ai, df_facility$acc_5)
df <- t_test$parameter
t_value <- t_test$statistic
p_value <- t_test$p.value           
conf_interval <- t_test$conf.int


x <- seq(-41, 40, length=1000)  
pdf_values <- dt(x, df)
data <- data.frame(x, pdf_values)

t2_test <- ggplot(data, aes(x)) +
  geom_line(aes(y = pdf_values), color = "black") +
  geom_vline(xintercept = t_value, linetype = "dashed", color = "red", size = 0.5) +
  geom_ribbon(aes(x = x, ymin = ifelse(x < t_test$conf.int[1] | x > t_test$conf.int[2], dt(x, df), 0), ymax = 0), fill = "#076fa2", alpha = 0.2) +
  
  geom_text(aes(x = t_test$statistic, y = 0.3, label = paste("t-score =",round(t_test$statistic,2)), angle = 90, vjust = 1.5), color = "black") +
  geom_text(aes(x = -20, y = 0.2, label = paste("95% CI \n [", round(t_test$conf.int[1], 2), ", ", round(t_test$conf.int[2], 2), "]")), vjust = -1, color = "red") +
  labs(title = "Two-Sample t test",
       x = "t statistic",
       y = "Probability Density"
  ) +
  xlim(c(-42,5)) 

# Power analysis


alpha <- 0.05
effect_size <- 0.5
desired_power <- 0.80

sample_size <- nrow(df_facility)
achieved_power <- 0

while (achieved_power < desired_power) {
  t_test_pwr <- pwr.t.test(
    d = effect_size,
    n = sample_size,
    sig.level = alpha,
    power = NULL,
    type = "two.sample"
  )
  
  achieved_power <- t_test_pwr$power  # Correct the variable name
  sample_size <- sample_size + 1
}

label <- cat("Sample size needed to achieve 80% power:", sample_size - 1, "\n")

plot(pwr_plt)
grid.arrange(t2_test,pwr_plt)  

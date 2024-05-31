library(tidyverse)
library(ggplot2)
library(forcats)
library(plotly)
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

devtools::install_github("cardiomoon/webr")




df_main_num <- read.csv("C:/Users/NITRO/Desktop/2023-24a-fai2-adsai-SimonaDimitrova222667/scripts/survey_num_25_10.csv",header = TRUE,sep = ",")
df_main_num[df_main_num == ""] <- NA



df_domain <- df_main_num %>%
  slice(-1:-2) %>%
  select(c(12:15,17:36)) %>% 
  
  filter(demo_domain != '8') %>%
  mutate_at(vars(-c("demo_gender","demo_domain", "demo_experience")),as.integer) %>%
  mutate(demo_ai_know = case_when(
    demo_ai_know == 13 ~ 5,
    demo_ai_know == 14 ~ 4,
    demo_ai_know == 15 ~ 3,
    demo_ai_know == 16 ~ 2,
    demo_ai_know == 17 ~ 1, 
    TRUE ~ demo_ai_know)) %>%
  na.omit()


df_facility <- df_main_num %>%
  slice(-1:-2) %>%
  select(c(12:15,17:36)) %>% 
  
  filter(demo_domain == '8') %>%
  mutate_at(vars(-c("demo_gender","demo_domain", "demo_experience")),as.integer) %>%
  na.omit()

x_facility <- c(df_facility$used_ai)
x_domains <- c(df_facility$acc_5)

CI_t <- function (x, ci = 0.687)
    {
    `%>%` <- magrittr::`%>%`
    Margin_Error <- qt(ci + (1 - ci)/2, df = length(x) - 1) * sd(x)/sqrt(length(x))
    df_out <- data.frame( sample_size=length(x), Mean=mean(x), sd=sd(x),
                          Margin_Error=Margin_Error,
                          'CI lower limit'=(mean(x) - Margin_Error),
                          'CI Upper limit'=(mean(x) + Margin_Error)) %>%
      tidyr::pivot_longer(names_to = "Measurements", values_to ="values", 1:6 )
    return(df_out)
}

CI_z <- function (x, ci = 0.95)
{
  `%>%` <- magrittr::`%>%`
  standard_deviation <- sd(x)
  sample_size <- length(x)
  Margin_Error <- abs(qnorm((1-ci)/2))* standard_deviation/sqrt(sample_size)
  df_out <- data.frame( sample_size=length(x), Mean=mean(x), sd=sd(x),
                        Margin_Error=Margin_Error,
                        'CI lower limit'=(mean(x) - Margin_Error),
                        'CI Upper limit'=(mean(x) + Margin_Error)) %>%
    tidyr::pivot_longer(names_to = "Measurements", values_to ="values", 1:6 )
  return(df_out)

  }

# One-Sample Z-test 


cl <- .6827
mean_value <- mean(df_domain$acc_3) 
sd_value <- sd(df_domain$acc_3) 
z_value <- qnorm((1 + cl) / 2)
p_value <- 2 * (1 - pnorm(abs(z_value)))

lower_bound <- mean_value - qnorm((1 + cl) / 2) * sd_value
upper_bound <- mean_value + qnorm((1 + cl) / 2) * sd_value

dnorm_one_sd <- function(x){
  norm_one_sd <- dnorm(x, mean = mean_value, sd = sd_value)
  norm_one_sd[x <= lower_bound | x >= upper_bound] <- NA
  return(norm_one_sd)
}


ztest1 <- ggplot(df_domain, aes(x = acc_3)) + stat_function(fun = function(x) dnorm(x, mean = mean_value, sd = sd_value)) + 
    stat_function(fun = dnorm_one_sd, geom = "area", fill = "#076fa2", alpha = 0.2) +
    geom_vline(xintercept = c(lower_bound,upper_bound), linetype = "dashed", color = "red") +

    geom_text(aes(x = 1, y = 0.25, 
                  label = paste(cl * 100,"% CI \n [", round(lower_bound, 2), ", ", round(upper_bound, 2), "]")), vjust = -1, color = "red") +
    scale_x_continuous("Values") +
    scale_y_continuous("Density") +
    theme_minimal() +
    xlim(c(min(df_domain$acc_3) - 1.5, max(df_domain$acc_3) + 3))


# Two-sample Z-test 

x_facility <- c(df_facility$acc_3)
x_domains <- c(df_domain$acc_3)

z_test <- z.test(x_facility, sigma.x=0.5, x_domains, sigma.y=0.5, conf.level=0.95)
df <- t_test$parameter
z_value <- z_test$statistic
p_value <- z_test$p.value           
conf_interval <- z_test$conf.int

x_values <- seq(-40, 40, length = 1000) 

ztest2 <- ggplot(data.frame(x = x_values), aes(x = x)) +
  geom_line(stat = "function", fun = dt, args = list(df = df), color = "black") +
  labs(x = "z-value", y = "Density") +
  #geom_ribbon(aes(x = x, ymin = 0, ymax = ifelse(x < -abs(t_value) | x > abs(t_value), dt(x, df), 0)),
  #           fill = "red", alpha = 0.3) +
  geom_vline(xintercept = z_test$conf.int, size =1 ,color = "red", linetype = "dotted")+
  geom_vline(xintercept = z_test$statistic, size =1 ,color = "black", linetype = "solid")+
  geom_text(aes(x = z_test$statistic, y = 0.1, label = paste("z-score = ",round(z_test$statistic,2)), angle = 90, vjust = 1.5), color = "black") +
  
  geom_text(aes(x = 1.5, y = 0.3, label = paste("95% CI \n [", round(t_test$conf.int[1], 2), ", ", round(z_test$conf.int[2], 2), "]")), vjust = -1, color = "red") +
  #geom_ribbon(aes(x = x, ymin = 0, ymax = ifelse(x >= t_test$conf.int[1] & x <= t_test$conf.int[2], dt(x, df), 0)),fill = "#076fa2",alpha = 0.2) +
  geom_ribbon(aes(x = x, ymin = ifelse(x < z_test$conf.int[1] | x > z_test$conf.int[2], dt(x, df), 0), ymax = 0), fill = "#076fa2", alpha = 0.2) +
  theme_minimal() +
  xlim(c(-40,40))

# Two-sample T-test

t_test <- t.test(df_facility$used_ai, df_facility$acc_5)
df <- t_test$parameter
t_value <- t_test$statistic
p_value <- t_test$p.value           
conf_interval <- t_test$conf.int


x <- seq(-41, 40, length=1000)  # Adjust the range and length as needed
pdf_values <- dt(x, df)
data <- data.frame(x, pdf_values)

tt_test <- ggplot(data, aes(x)) +
  geom_line(aes(y = pdf_values), color = "black") +
  geom_vline(xintercept = t_value, linetype = "dashed", color = "red", size = 1) +
  geom_ribbon(aes(x = x, ymin = ifelse(x < t_test$conf.int[1] | x > t_test$conf.int[2], dt(x, df), 0), ymax = 0), fill = "#076fa2", alpha = 0.2) +
  geom_text(aes(x = t_test$statistic, y = 0.1, label = paste("t-score =",round(t_test$statistic,2)), angle = 90, vjust = 1.5), color = "black") +
  geom_text(aes(x = -20, y = 0.2, label = paste("95% CI \n [", round(t_test$conf.int[1], 2), ", ", round(t_test$conf.int[2], 2), "]")), vjust = -1, color = "red") +
  labs(x = "t statistic",
       y = "Probability Density",
       ) +
  theme_minimal() +
  xlim(c(-42,5)) 
  

pwr.t.test(d=(0-10)/16.03,power=.8,sig.level=.05,type="two.sample",alternative="two.sided")


if(FALSE) {

# Paired T-test

paired_t_test <- t.test(df_facility$used_ai, df_facility$acc_5, paired=TRUE)
df <- paired_t_test$parameter
t_value <- paired_t_test$statistic
p_value <- paired_t_test$p.value           
conf_interval <- paired_t_test$conf.int

x <- seq(-43, 40, length=1000)  # Adjust the range and length as needed
pdf_values <- dt(x, df)
data <- data.frame(x, pdf_values)


paired_tt <- ggplot(data, aes(x)) +
  geom_line(aes(y = pdf_values), color = "black") +
  geom_vline(xintercept = t_value, linetype = "dashed", color = "red", size = 1) +
  geom_ribbon(aes(x = x, ymin = ifelse(x < paired_t_test$conf.int[1] | x > paired_t_test$conf.int[2], dt(x, df), 0), ymax = 0), fill = "#076fa2", alpha = 0.2) +
  geom_text(aes(x = paired_t_test$statistic, y = 0.1, label = paste("t-score =",round(paired_t_test$statistic,2)), angle = 90, vjust = 1.5), color = "black") +
  geom_text(aes(x = -20, y = 0.2, label = paste("95% CI \n [", round(paired_t_test$conf.int[1], 2), ", ", round(paired_t_test$conf.int[2], 2), "]")), vjust = -1, color = "red") +
  labs(x = "t statistic",
       y = "Probability Density",
  ) +
  theme_minimal() +
  xlim(c(-44,5)) 


df_results <- data.frame(Group = factor(rep(c("Group A", "Group B"), each = length(df_facility$used_ai)), 
                                        Value = c(df_facility$used_ai, df_facility$acc_5)))

wilcox_test_result <- wilcox.test(df_facility$used_ai, df_facility$acc_5, exact = FALSE)
p <- ggplot(df_results, aes(x = Group, y = Value, fill = Group)) +
  geom_violin() +
  labs(title = "Mann-Whitney U Test Results",
       x = "Groups",
       y = "Values")
p


x_values <- seq(-60, 40, length = 100) 
  
ttest2 <- ggplot(data.frame(x = x_values), aes(x = x)) +
  geom_line(stat = "function", fun = dt, args = list(df = df), color = "black") +
  labs(x = "t-value", y = "Density") +
  #geom_ribbon(aes(x = x, ymin = 0, ymax = ifelse(x < -abs(t_value) | x > abs(t_value), dt(x, df), 0)),
   #           fill = "red", alpha = 0.3) +
  geom_vline(xintercept = t_test$conf.int, size =1 ,color = "red", linetype = "dotted")+
  geom_vline(xintercept = t_test$statistic ,size = 0.5 ,color = "black", linetype = "solid")+
  geom_text(aes(x = t_test$statistic, y = 0.1, label = paste("t-score =",round(t_test$statistic,2)), angle = 90, vjust = 1.5), color = "black") +
  
  geom_text(aes(x = 1.5, y = 0.3, label = paste("95% CI \n [", round(t_test$conf.int[1], 2), ", ", round(t_test$conf.int[2], 2), "]")), vjust = -1, color = "red") +
  #geom_ribbon(aes(x = x, ymin = 0, ymax = ifelse(x >= t_test$conf.int[1] & x <= t_test$conf.int[2], dt(x, df), 0)),fill = "#076fa2",alpha = 0.2) +
  geom_ribbon(aes(x = x, ymin = ifelse(x < t_test$conf.int[1] | x > t_test$conf.int[2], dt(x, df), 0), ymax = 0), fill = "#076fa2", alpha = 0.2) +
  theme_minimal() +
  xlim(c(-40,40))

ggarrange(ttest2,ztest2)

}


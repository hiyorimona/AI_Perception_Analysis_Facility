library(tidyverse)
library(ggplot2)
library(forcats)
library(plotly)
library(gghighlight)
library(hrbrthemes)
library(viridis)



df_main <- read.csv(file.path(getwd(), "data", "survey_text_27_10.csv"),header = TRUE,sep = ",")
df_main[df_main == ""] <- NA
x_order <- c("Strongly agree", "Somewhat agree", "Neither agree nor disagree","Somewhat disagree","Strongly disagree")  
df_num <- read.csv(file.path(getwd(), "data", "survey_num_27_10.csv"),header = TRUE,sep = ",")

df_domain_par <- df_main %>%
  slice(-1:-2) %>%
  select(demo_domain) %>%
  filter(demo_domain != "Click to write Choice 10") %>%
  na.omit() %>%
  group_by(demo_domain) %>%
  summarise(n = length(demo_domain)) %>%
  arrange(desc(n)) 


participants <- ggplot(df_domain_par) +
  geom_col(aes(n, reorder(demo_domain, n)), fill = "#076fa2", width = 0.6) +
  gghighlight(demo_domain == "Facility") +
    scale_x_continuous(
    limits = c(0, 105),
    breaks = seq(0, 105, by = 10), 
    expand = c(0, 0), 
    position = "top") +
  
  scale_y_discrete(expand = expansion(add = c(0,.5))) +
  theme(
    panel.background = element_rect(fill = "white"),
    panel.grid.major.x = element_line(color = "#A8BAC4", size = 0.3),
    axis.ticks.length = unit(0, "mm"),
    axis.title = element_blank(),
    axis.line.y.left = element_line(color = "black"))


participants

mapping = c("Tourism" = "#A8BAC4", "Leisure & Events" = "#A8BAC4", "Media" = "#A8BAC4",
            "Hotel" = "#A8BAC4", "Games" = "#A8BAC4", "Logistics" = "#A8BAC4",
            "Built Environment" = "#A8BAC4", "Facility" = "#076fa2", "Applied Data Science & AI" = "#A8BAC4", "Other" = "#A8BAC4")


df_domain <- df_main_num %>%
  slice(-1:-2) %>%
  select(c(12:15,17:36)) %>% 
  
  filter(demo_domain != '11') %>%
  
  mutate_at(vars(-c("demo_role","demo_gender","demo_age","demo_experience","demo_domain")),as.integer) %>%
 
  mutate(acc_5 = case_when(
    acc_5 == 15 ~ 1,
    acc_5 == 16 ~ 2,
    acc_5 == 17 ~ 3,
    acc_5 == 18 ~ 4,
    acc_5 == 19 ~ 5,
    TRUE ~ acc_5),
    
    demo_domain = case_when(
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
    
    demo_role = case_when(
      demo_role == 1 ~ "Student",
      demo_role == 2 ~ "Educator",
      demo_role == 8 ~ "Supporting Staff",
      demo_role == 6 ~ "Management",
      TRUE ~ demo_gender),
    
    demo_gender = case_when(
      demo_gender == 1 ~ "Male",
      demo_gender == 2 ~ "Female",
      demo_gender == 3 ~ "Non-binary",
      demo_gender == 4 ~ "Prefer not to say",
      TRUE ~ demo_gender),
    
    demo_age = case_when(
      demo_age == 7 ~ "> 65",
      demo_age == 2 ~ "18-24",
      demo_gender == 5 ~ "45-54",
      demo_age == 4 ~ "35-44",
      demo_age == 3 ~ "25-34",
      demo_age == 6 ~ "55-64",
      TRUE ~ demo_age),
    
    
    demo_experience = case_when(
      demo_experience == '1' ~ '2 - 5 years',
      demo_experience == '2' ~ '0 - 6 months',
      demo_experience == '3' ~ '6 - 12 months',
      demo_experience == '4' ~ '1 - 2 years',
      demo_experience == '5' ~ '5 - 10 years',
      demo_experience == '6' ~ '10 - 20 years',
      demo_experience == '7' ~ '20 + years',
      TRUE ~ demo_experience),
    type_f = ifelse(demo_domain == "Facility", "Facility", "Other")) %>%
  na.omit()

    if(FALSE) {
    demo_ai_know = case_when(
      demo_ai_know == 13 ~ "Extremely good",
      demo_ai_know == 14 ~ "Somewhat good",
      demo_ai_know == 15 ~ "Neither good nor bad",
      demo_ai_know == 16 ~ "Somewhat bad",
      demo_ai_know == 17 ~ "Extremely bad",
      TRUE ~ demo_ai_know)
    }
mapping2 <- c("Extremely good", 
             "Somewhat good",
             "Neither good nor bad",
             "Somewhat bad",
             "Extremely bad")

boxplt <- ggplot(df_domain,aes(x=used_ai, y=demo_domain, fill=type_f)) +
  geom_boxplot(width = 0.5, position = position_dodge(width = 0.75)) +
  scale_fill_manual(values = alpha(c("#076fa2" ,"#A8BAC4"), 0.8))+
  labs(
    x = "",
    y = ""
  ) +
  theme_minimal() +
  theme(
    legend.position="none",
  )



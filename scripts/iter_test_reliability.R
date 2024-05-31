library(irr)

df_general_num <- read.csv("C:/Users/NITRO/Desktop/2023-24a-fai2-adsai-SimonaDimitrova222667/scripts/survey_num_27_10.csv",header = TRUE,sep = ",")
df_general_num[df_general_num == ""] <- NA


df_main_num <- df_general_num %>%
  slice(-1:-2) %>%
  select(c(12:15,17:36)) %>%
  na.omit()


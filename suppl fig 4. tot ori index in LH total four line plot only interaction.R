
###pull packages out of the library

library(readxl)
library(emmeans)  ### estimated marginal means and p values
library(sjstats)  ### partial eta squared and cohens f effect size 
library(lme4)     ### estimated the multi level model (random intercept for participants)
library(lmerTest) ### gives more comprehsive anova output with p values
library(MuMIn)   ### R2 for the model

###turn off scientfic notation

options(scipen = 999)

###load the folder files into a list
diskpath <- "put your parent folder path here" # replace your parent folder path here
savepath <- file.path(diskpath,"/CPT_TOT_manuscript/data processing/fig/15_ori_index_LH")

setwd(savepath)

df_list <- c("female_ori_index_before_event_15min_bin_corrected.csv","male_ori_index_before_event_15min_bin_corrected.csv")

# Open a connection to a text file to save the captured output
output_file <- file("tot event ori index four line plot mixed_model_report_corrected.txt", "w")

# Open a text file to save the code report
sink(output_file, type = "output")

for (df_name in df_list) {
  dt <- read.csv(df_name)
  #attach(dt)
  
  cat("***********************\n")
  title <- "total orientation index"
  gander<- dt[2, "gander"]
  cat(paste0(gander,"_", title , " statistic results: "), "\n")
  cat("***********************\n")
  
  dteff <- lmer(ori_index_event ~ as.factor(min_15bin)*as.factor(event_type) + (1|id_cohort), data=dt) 
  print(anova(dteff)) 
  
  cat("-------------eta squared results----------------\n")
  print(effectsize::eta_squared(dteff, partial = TRUE))
  
  print("\n")
  cat("                       \n")
  cat("------based on time bin post hoc results without interaction-------\n")
  print(emmeans(dteff, list(pairwise ~ min_15bin), adjust = "bonferroni"))  #post hoc (two correction egs)
  #print(emmeans(dteff, list(pairwise ~ ses_order), adjust = "tukey"))
  
  cat("------based on dosage post hoc results without interaction-------\n")
  print(emmeans(dteff, list(pairwise ~ event_type), adjust = "bonferroni"))  #post hoc (two correction egs)
  #print(emmeans(dteff, list(pairwise ~ ses_order), adjust = "tukey"))
  
  cat("-------post hoc results with interaction-------\n")
  print(emmeans(dteff, pairwise~min_15bin | event_type), adjust = "bonferroni") #if there was an interaction your code would be this


  cat("-------post hoc results with interaction-------\n")
  print(emmeans(dteff, pairwise~event_type | min_15bin), adjust = "bonferroni") #if there was an interaction your code would be this
  
  
  outputpath <- paste0(savepath, "/ggplot/", gander, "_", sprintf("%s_qqplot_corrected.png", title))
  png(outputpath, units="px", width=800, height=800, res=150)
  qqnorm(resid(dteff), main=sprintf("/%s_qqplot", title))
  dev.off()
}

# Close the output connection
sink(type = "message")
close(output_file)
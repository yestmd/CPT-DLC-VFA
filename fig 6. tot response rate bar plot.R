
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
savepath <- file.path(diskpath,"/CPT_TOT_manuscript//data processing//fig//15_ori_index_LH_total//15_ori_index_LH_total_adjust_scale")

setwd(savepath)

df_list <- c("male_response_rate.csv","female_response_rate.csv")

# Open a connection to a text file to save the captured output
output_file <- file("tot rr mixed_model_report_response_rate.txt", "w")

# Open a text file to save the code report
sink(output_file, type = "output")

for (df_name in df_list) {
  dt <- read.csv(df_name)
  #attach(dt)
  
  cat("***********************\n")
  title <- "total ori index"
  gander<- dt[2, "gander"]
  cat(paste0(gander,"_", title , " statistic results: "), "\n")
  cat("***********************\n")
  
  dteff <- lmer(response_rate ~ as.factor(min_15bin) + (1|id_cohort), data=dt) 
  print(anova(dteff)) 
  
  cat("-------------eta squared results----------------\n")
  print(effectsize::eta_squared(dteff, partial = TRUE))
  
  print("\n")
  cat("                       \n")
  cat("------based on time bin post hoc results without interaction-------\n")
  print(emmeans(dteff, list(pairwise ~ min_15bin), adjust = "bonferroni"))  #post hoc (two correction egs)
  
  #cat("-------post hoc results with interaction-------\n")
  #print(emmeans(dteff, pairwise~blind_or_not | min_15bin), adjust = "bonferroni") #if there was an interaction your code would be this
  
  outputpath <- paste0(savepath, "/ggplot/", gander, "_", sprintf("%s_qqplot_corrected_remove_outliner.png", title))
  png(outputpath, units="px", width=800, height=800, res=150)
  qqnorm(resid(dteff), main=sprintf("/%s_qqplot", title))
  dev.off()
}

# Close the output connection
sink(type = "message")
close(output_file)
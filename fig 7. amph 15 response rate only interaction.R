
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
savepath <- file.path(diskpath, "/CPT_TOT_manuscript/data processing/fig/15_amph_ori_index_LH_V2_total/for_publication")

setwd(savepath)

df_list <- list.files(pattern=".csv", all.files=FALSE, full.names=FALSE)

# Open a connection to a text file to save the captured output
output_file <- file("rr mixed_model_report only interaction.txt", "w")

# Open a text file to save the code report
sink(output_file, type = "output")

for (df_name in df_list) {
  dt <- read.csv(df_name)
  #attach(dt)
  
  cat("***********************\n")
  title <- "amph response rate"
  gander<- dt[2, "gander"]
  cat(paste0(gander,"_", title , " statistic results: "), "\n")
  cat("***********************\n")
  
  dt$min_15bin <- factor(dt$min_15bin)

  dt$dosage <- factor(dt$dosage,
                      levels = c("AMPH-vehicle","AMPH-0.3mpk", "AMPH-0.6mpk", "AMPH-1mpk"),
                      labels = c("AMPH-vehicle","AMPH-0.3mpk", "AMPH-0.6mpk", "AMPH-1mpk"))
  #print(summary(dt))
  cat("-------model with interaction-------\n")
  dteff <- lmer(response_rate ~ min_15bin*dosage + (1|animal_id), data=dt) 
  print(anova(dteff))
  
  cat("-------------eta squared results----------------\n")
  print(effectsize::eta_squared(dteff, partial = TRUE))
  
  cat("-------post hoc for model with interaction-------\n")
  print(emmeans(dteff, list(pairwise ~ min_15bin), adjust = "bonferroni"))  #post hoc (two correction egs)
  print(emmeans(dteff, list(pairwise ~ dosage), ref=1, adjust = "bonferroni"))  #post hoc (two correction egs)
  print(emmeans(dteff, pairwise~dosage | min_15bin), adjust = "bonferroni") #if there was an interaction your code would be this
  
  
  folderpath <- file.path(savepath, "/ggplot")
  outputpath <- paste0(folderpath, "/", gander, "_amph_rr_interaction_", sprintf("%s_qqplot.png", title))
  png(outputpath, units="px", width=800, height=800, res=150)
  qqnorm(resid(dteff), main=sprintf("%s %s with interaction qqplot", gander, title))
  dev.off()
  
}

# Close the output connection
sink(type = "message")
close(output_file)
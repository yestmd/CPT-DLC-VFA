
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
savepath <- file.path(diskpath, "CPT_TOT_manuscript/data processing/fig/amph_tot_15")

setwd(savepath)

df_list <- list.files(pattern=".csv", all.files=FALSE, full.names=FALSE)

# Open a connection to a text file to save the captured output
output_file <- file("amph mixed_model_report only interaction.txt", "w")

# Open a text file to save the code report
sink(output_file, type = "output")

for (df_name in df_list) {
  dt <- read.csv(df_name)
  #attach(dt)
  
  cat("***********************\n")
  title <- dt[2, "param"]
  gander<- dt[2, "gander"]
  cat(paste0(gander,"_", title , " statistic results: "), "\n")
  cat("***********************\n")
  
  dt$param <- factor(dt$param)

  
  dt$dosage <- factor(dt$dosage,
                      levels = c("AMPH-vehicle","AMPH-0.3mpk", "AMPH-0.6mpk", "AMPH-1mpk"),
                      labels = c("AMPH-vehicle","AMPH-0.3mpk", "AMPH-0.6mpk", "AMPH-1mpk"))
  #print(summary(dt))
  
  cat("-------model with interaction-------\n")
  dteff <- lmer(value ~ param*dosage + (1|group_animal_id), data=dt)  
  print(anova(dteff)) 
  
  cat("-------------eta squared results----------------\n")
  print(effectsize::eta_squared(dteff, partial = TRUE))
  
  cat("-------post hoc for model with interaction-------\n")
  print(emmeans(dteff, list(pairwise ~ param), adjust = "bonferroni"))
  print(emmeans(dteff, list(pairwise ~ dosage), ref=1, adjust = "bonferroni"))
  print(emmeans(dteff, pairwise~dosage | param), adjust = "bonferroni")
  print(emmeans(dteff, pairwise~param | dosage), adjust = "bonferroni")

  folderpath <- file.path(savepath, "qqplot")
  
  outputpath <- paste0(folderpath, "/", gander, sprintf("_%s_itact.png", title))
  png(outputpath, units="px", width=800, height=800, res=150)
  qqnorm(resid(dteff), main=sprintf("%s %s with interaction qqplot", gander, title))
  dev.off()

  
}

# Close the output connection
sink(type = "message")
close(output_file)
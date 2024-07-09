library(readxl)
library(emmeans)  ### estimated marginal means and p values
library(sjstats)  ### partial eta squared and cohens f effect size 
library(lme4)     ### estimated the multi level model (random intercept for participants)
library(lmerTest) ### gives more comprehsive anova output with p values
library(MuMIn)   ### R2 for the model

###turn off scientfic notation
options(scipen = 999)

###load the folder files into a list
diskpath <-  "put your parent folder path here" # replace your parent folder path here
savepath <- file.path(diskpath, "CPT_TOT_manuscript/data processing/fig/stage 3 training")

setwd(savepath)
df_list <- list.files(pattern=".csv", all.files=FALSE, full.names=FALSE)

# Open a connection to a text file to save the captured output
output_file <- file("stage 3 mixed_model_report only interaction.txt", "w")

# Open a text file to save the code report
sink(output_file, type = "output")

for (df_name in df_list) {
  dt <- read.csv(df_name)
  #attach(dt)
  
  cat("***********************\n")
  title <- dt[2, "param"]
  cat(paste0(title, " statistic results: "), "\n")
  cat("***********************\n")
  dt$ses_order <- factor(dt$ses_order,
                         levels = c(1,2,3,4,5,6,7),
                         labels = c("session_order_1", "session_order_2", "session_order_3",
                                    "session_order_4","session_order_5","session_order_6", 
                                    "session_order_7"))
  
  dt$gander <- factor(dt$gander,
                      levels = c("male","female"),
                      labels = c("male", "female"))
  #print(summary(dt))
  
  
  cat("-------model with interaction-------\n")
  dteff <- lmer(value ~ ses_order*gander + (1|id), data=dt) 
  print(anova(dteff)) 
  
  cat("-------------eta squared results----------------\n")
  print(effectsize::eta_squared(dteff, partial = TRUE))
  
  cat("-------post hoc for model with interaction-------\n")
  print(emmeans(dteff, list(pairwise ~ ses_order), adjust = "bonferroni"))
  print(emmeans(dteff, list(pairwise ~ gander), adjust = "bonferroni"))
  print(emmeans(dteff, pairwise ~ses_order | gander), adjust = "bonferroni")
  print(emmeans(dteff, pairwise ~gander | ses_order), adjust = "bonferroni")
  
  folderpath <- file.path(savepath, "r_qqplot")
  
  outputpath <- paste0(folderpath, "/", sprintf("_%s_itact.png", title))
  png(outputpath, units="px", width=800, height=800, res=150)
  qqnorm(resid(dteff), main=sprintf("%s with interaction qqplot", title))
  dev.off()
  
}

# Close the output connection
sink(type = "message")
close(output_file)
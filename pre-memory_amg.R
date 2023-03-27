##################### Analysis: Mind Wandering & Learning ######################
# student: Ana-Maria Gore

# clear workspace 
rm(list=ls())

# install packages
library(dplyr)
pacman::p_load(data.table, tidyverse, dplyr, 'nlme', sjPlot, rstatix, lme4, 
               effects, car, readr, pastecs, psych, psychTools, papaja, cowplot, 
               MASS, ggcorrplot, pwr) #require

# import and merge data
files_testing1 <- list.files('/Users/ana-maria/Documents/ResMas/Internship/Data/Testing', 
                    pattern = ".csv$", recursive = TRUE, full.names = TRUE)
files_testing2 <- list.files('/Users/ana-maria/Documents/ResMas/Internship/Data/Testing2', 
                    pattern = ".csv$", recursive = TRUE, full.names = TRUE)
files_learning <- list.files('/Users/ana-maria/Documents/ResMas/Internship/Data/Learning',
                    pattern = ".csv$", recursive = TRUE, full.names = TRUE)
  
testing1 <- read_csv(files_testing1) %>% bind_rows()
testing2 <- read_csv(files_testing2) %>% bind_rows()
learning <- read_csv(files_learning) %>% bind_rows()

# rename columns
names(testing1)[names(testing1) == "Participant number"] <- "participant_nr"
names(testing2)[names(testing2) == "Participant number"] <- "participant_nr"
names(testing1)[names(testing1) == "Accuracy"] <- "accuracy1"
names(testing2)[names(testing2) == "Accuracy"] <- "accuracy2"
names(learning)[names(learning) == "Participant number"] <- "participant_nr"

# descriptives
summary(learning) # age
sd(trial1$Age)
mean(trial1$Age)
trial1 <- learning %>%
  filter(Trials == 1)
table(trial1$Gender)/nrow(trial1)*100 # gender

?nrow
# calculate accuracy per participant
accuracy1 <- testing1 %>% 
  group_by(participant_nr) %>% 
  summarise(accuracy1 = sum(accuracy1))
accuracy2 <- testing2 %>% 
  group_by(participant_nr) %>% 
  summarise(accuracy2 = sum(accuracy2))
accuracy_df <- merge(accuracy1, accuracy2)
change_df <- data.frame(accuracy_df, accuracy1[,2] - accuracy2[,2])
names(change_df)[names(change_df) == "accuracy1.1"] <- "change" #rename column

# create dataset
testing_merged <- merge(testing1, testing2, by = c("participant_nr", "Englishword"))
testing_merged$difference <- testing_merged['accuracy1'] - testing_merged['accuracy2'] #difference in accuracy between 2 tests
mean(testing_merged$difference[,1]) #close to 0

# center to the mean
center_apply <- function(x) {
  apply(x, 2, function(y) y - mean(y))
}

learning$NOptions_c <- center_apply(learning['NOptions']) 
learning$Correct_c <- center_apply(learning['Correct']) 

# interaction between number of options / frames, correctness and accuracy (doesn't have to)


################################# RPEs #########################################

# create datasets
srpe_learning <- learning %>% dplyr::select(Englishword, participant_nr, SRPE, NOptions_c, Correct_c, Correct, NOptions)
accuracy_testing <- testing_merged %>% dplyr::select(Englishword, participant_nr, accuracy1, accuracy2)
srpe_accuracy_df <- merge(accuracy_testing, srpe_learning)

#check assuptions
head(srpe_accuracy_df)
srpe_accuracy_df <- as.numeric(srpe_accuracy_df)

# multicolinearity
#The Q-Q plots showed that the random effects were normally distributed, 
#and there seemed to be no dispersion problems or high multicollinearity between the fixed effects.
qqPlot(srpe_accuracy_df$SRPE)
qqPlot(srpe_accuracy_df$NOptions_c)
qqPlot(srpe_accuracy_df$Correct_c)

#model
srpe_acc1_model <- lme(accuracy1 ~ SRPE, random= ~1|participant_nr, data=srpe_accuracy_df)
summary(srpe_acc1_model)
srpe_acc2_model <- lme(accuracy2 ~ SRPE, random= ~1|participant_nr, data=srpe_accuracy_df, method="ML")
summary(srpe_acc2_model)

acc1_model <- lmer(accuracy1 ~ SRPE + (1 | participant_nr), data = srpe_accuracy_df, REML = FALSE)
summary(acc1_model)$coefficients

AIC(srpe_acc1_model,srpe_acc2_model)

srpe_acc_model <- lmer(accuracy1 ~ SRPE * NOptions_c * Correct_c + (1 | participant_nr), data = srpe_accuracy_df, REML = FALSE)
summary(srpe_acc_model)

chisq_test(srpe_accuracy_df$accuracy1, srpe_accuracy_df$SRPE)
chisq_test(srpe_accuracy_df$mean_acc1, srpe_accuracy_df$SRPE)

#model accuracy1
acc1_gm <- glmer(accuracy1 ~ (1 | participant_nr), data = srpe_accuracy_df, family=binomial(link = "logit"))
acc1_srpe_gm <- glmer(accuracy1 ~ SRPE + (1 | participant_nr), data = srpe_accuracy_df, family=binomial(link = "logit"))
summary(acc1_srpe_gm)
drop1(acc1_srpe_gm, test="Chisq")
anova(acc1_gm, acc1_srpe_gm, test='Chisq')
confint(acc1_srpe_gm)

acc1_urpe_gm <- glmer(accuracy1 ~ URPE + (1 | participant_nr), data = srpe_accuracy_df, family=binomial(link = "logit"))
summary(acc1_urpe_gm)
drop1(acc1_urpe_gm, test="Chisq")
confint(acc1_urpe_gm)

anova(acc1_srpe_gm, acc1_urpe_gm)

#model accuracy2
acc2_srpe_gm <- glmer(accuracy2 ~ SRPE + (1 | participant_nr), data = srpe_accuracy_df, family=binomial(link = "logit"))
summary(acc2_srpe_gm)
drop1(acc2_srpe_gm, test="Chisq")
confint(acc2_srpe_gm)

acc2_urpe_gm <- glmer(accuracy2 ~ URPE + (1 | participant_nr), data = srpe_accuracy_df, family=binomial(link = "logit"))
summary(acc2_urpe_gm)
drop1(acc2_urpe_gm, test="Chisq")
confint(acc2_urpe_gm)

anova(acc2_srpe_gm, acc2_urpe_gm)

# Diff in accuracy btw rewarded & unrewarded
srpe_accuracy_df$accuracy1 <- as.numeric(srpe_accuracy_df$accuracy1)
srpe_accuracy_df %>% 
  filter(Correct == 1) %>% 
  mean(accuracy1)
srpe_accuracy_df %>% 
  filter(Correct == 0) %>% 
  summary(accuracy1)

#reward effect on accuracy
chisq_test(srpe_accuracy_df$accuracy2, srpe_accuracy_df$Correct)

l0 <- lmer(accuracy1 ~ (1|participant_nr), data=srpe_accuracy_df)
l1 <- lmer(accuracy1 ~ Correct_c + (1|participant_nr), data=srpe_accuracy_df)
summary(l1)
anova(l0, l1)
drop1(l1,test="Chisq")
l2 <- lmer(accuracy2 ~ Correct_c + (1|participant_nr), data=srpe_accuracy_df)
drop1(l2,test="Chisq")

#options effect on accuracy
l11 <- lmer(accuracy1 ~ NOptions_c + (1|participant_nr), data=srpe_accuracy_df)
drop1(l11,test="Chisq")
l22 <- lmer(accuracy2 ~ NOptions_c + (1|participant_nr), data=srpe_accuracy_df)
drop1(l22,test="Chisq")

srpe_accuracy_df %>% 
  filter(NOptions == 1) %>% 
  summary(accuracy1)
srpe_accuracy_df %>% 
  filter(NOptions == 2) %>% 
  summary(accuracy1)
srpe_accuracy_df %>% 
  filter(NOptions == 4) %>% 
  summary(accuracy1)

#interaction effect
l03 <- lmer(accuracy1 ~ Correct_c + NOptions_c + (1|participant_nr), data=srpe_accuracy_df)
l3 <- lmer(accuracy1 ~ Correct_c + NOptions_c + Correct_c*NOptions_c + (1|participant_nr), data=srpe_accuracy_df)
drop1(l3,test="Chisq")
anova(l03, l3)
l04 <- lmer(accuracy2 ~ Correct_c + NOptions_c + (1|participant_nr), data=srpe_accuracy_df)
l4 <- lmer(accuracy2 ~ Correct_c*NOptions_c + (1|participant_nr), data=srpe_accuracy_df)
drop1(l4,test="Chisq")
anova(l04,l4)

# For each SRPE the average recall accuracy and the 95% confidence interval is estimated.
spre_df1 <- srpe_accuracy_df %>% 
  group_by(SRPE) %>% 
  summarise(mean_acc1 = mean(accuracy1) * 100,
            n1 = n(),
            sd1 = sd(accuracy1))
            
spre_df2 <- srpe_accuracy_df %>% 
  group_by(SRPE) %>% 
  summarise(mean_acc2 = mean(accuracy2) * 100,
            n2 = n(),
            sd2 = sd(accuracy2))
spre_df <- merge(spre_df1, spre_df2)

spre_df <- spre_df %>%
  mutate(margin1 = qt(0.95,df=n1-1)*sd1/sqrt(n1)*100,
         margin2 = qt(0.95,df=n2-1)*sd2/sqrt(n2)*100)

round(pastecs::stat.desc(spre_df),2)

#plot
srpe_acc1_plot <- ggplot(spre_df, mapping = aes(x = SRPE, y = mean_acc1)) +
  geom_pointrange(aes(ymin = mean_acc1-margin1, ymax = mean_acc1+margin1), fatten = 1.4, color = 'cyan3')+
  geom_smooth(method = 'glm', fill='cyan3', color = 'cyan3', size = 0.6) +
  scale_y_continuous("Correct (%)", limits=c(0, 100), n.breaks = 10) +
  scale_x_continuous(breaks = c(-0.50, -0.25, 0.00, 0.50, 0.75), limits=c(-0.50, 0.75)) +
  theme_apa()
srpe_acc2_plot <- ggplot(spre_df, mapping = aes(x = SRPE, y = mean_acc2)) +
  geom_pointrange(aes(ymin = mean_acc1-margin2, ymax = mean_acc1+margin2), fatten = 1.4, color = 'coral')+
  geom_smooth(method = 'glm', fill='coral', color = 'coral', size = 0.6) +
  scale_y_continuous("Correct (%)", limits=c(0, 100), n.breaks = 10) +
  scale_x_continuous(breaks = c(-0.50, -0.25, 0.00, 0.50, 0.75)) +
  theme_apa()
plot_grid(srpe_acc1_plot, srpe_acc2_plot, labels = "AUTO")

# For each URPE the average recall accuracy and the 95% confidence interval is estimated.
srpe_accuracy_df$URPE <- abs(srpe_accuracy_df$SRPE)
upre_df1 <- srpe_accuracy_df %>% 
  group_by(URPE) %>% 
  summarise(mean_acc1 = mean(accuracy1) * 100,
            n1 = n(),
            sd1 = sd(accuracy1))
upre_df2 <- srpe_accuracy_df %>% 
  group_by(URPE) %>% 
  summarise(mean_acc2 = mean(accuracy2) * 100,
            n2 = n(),
            sd2 = sd(accuracy2))
upre_df <- merge(upre_df1, upre_df2)
upre_df <- upre_df %>%
  mutate(margin1 = qt(0.95,df=n1-1)*sd1/sqrt(n1)*100,
         margin2 = qt(0.95,df=n2-1)*sd2/sqrt(n2)*100)

#plot
urpe_acc1_plot <- ggplot(upre_df, mapping = aes(x = URPE, y = mean_acc1)) +
  geom_pointrange(aes(ymin = mean_acc1-margin1, ymax = mean_acc1+margin1), fatten = 1.4, color = 'cyan3')+
  geom_smooth(method = 'glm', fill='cyan3', color = 'cyan3', size = 0.6) +
  scale_y_continuous("Correct (%)", limits=c(0, 100), n.breaks = 10) +
  scale_x_continuous(breaks = c(0.00, 0.25, 0.50, 0.75)) +
  theme_apa()
urpe_acc2_plot <- ggplot(upre_df, mapping = aes(x = URPE, y = mean_acc2)) +
  geom_pointrange(aes(ymin = mean_acc1-margin2, ymax = mean_acc1+margin2), fatten = 1.4, color = 'coral')+
  geom_smooth(method = 'glm', fill='coral', color = 'coral', size = 0.6) +
  scale_y_continuous("Correct (%)", limits=c(0, 100), n.breaks = 10) +
  scale_x_continuous(breaks = c(0.00, 0.25, 0.50, 0.75)) +
  theme_apa()
plot_grid(urpe_acc1_plot, urpe_acc2_plot, labels = "AUTO")


# plot
plot.lme(acc1_spre_gm)
plot.lme(acc1_model)

plot_model(acc1_model)
sjPlot::plot_model(acc1_model, 
                   show.values=TRUE, show.p=TRUE,
                   title="Effect of SRPE on accuracy 1")

vignette("ggplot2-specs")

ggplot(srpe_accuracy_df, mapping = aes(x = SRPE, y = accuracy2)) +
  geom_smooth(method = 'glm', method.args = list(family = "binomial")) +
  scale_y_continuous(limits=c(0, 1)) +
  theme_apa() 

#post hoc


#effect
effects_spre <- effects::effect(term= "SRPE", mod= srpe_acc1_model)
summary(effects_spre)
effects_spre_df <- as.data.frame(effects_spre) # Save the effects values as a df

# calculate accuracy for trials with positive rpe

urpe_diff0 <- srpe_accuracy_df %>% filter(URPE != 0) 
acc_urpe_df <- urpe_diff0 %>% group_by(participant_nr) %>% summarise(accuracy_urpe_1 = sum(accuracy1),
                                                                     accuracy_urpe_2 = sum(accuracy2))
srpe_positif <- srpe_accuracy_df %>% filter(SRPE > 0) 
acc_srpe_df <- srpe_positif %>% group_by(participant_nr) %>% summarise(accuracy_srpe_1 = sum(accuracy1),
                                                                       accuracy_srpe_2 = sum(accuracy2))
                                                                     
################################# ggplot theme #########################################
.First <- function() {
  # Load packages
  library(tidyverse)
  
  # Set theme
  apa_theme <- theme(
    plot.margin = unit(c(1, 1, 1, 1), "cm"),
    plot.background = element_rect(fill = "white", color = NA),
    plot.title = element_text(size = 22, face = "bold",
                              hjust = 0.5,
                              margin = margin(b = 15)),
    axis.line = element_line(color = "black", size = .5),
    axis.title = element_text(size = 18, color = "black",
                              face = "bold"),
    axis.text = element_text(size = 15, color = "black"),
    axis.text.x = element_text(margin = margin(t = 10)),
    axis.title.y = element_text(margin = margin(r = 10)),
    axis.ticks = element_line(size = .5),
    panel.grid = element_blank(),
    legend.position = c(0.20, 0.8),
    legend.background = element_rect(color = "black"),
    legend.text = element_text(size = 15),
    legend.margin = margin(t = 5, l = 5, r = 5, b = 5),
    legend.key = element_rect(color = NA, fill = NA)
  )
  
  theme_set(theme_minimal(base_size = 12) +
              apa_theme)
}

theme_set(theme_minimal)
################################# SART #########################################

# import and merge data
files <- list.files('/Users/ana-maria/Documents/ResMas/Internship/Data/SART', 
                    pattern = ".csv$", recursive = TRUE, full.names = TRUE)
data <- read_csv(files) %>% bind_rows()

# select only test trials
data <- data %>% filter(block == "1" | block == "2")

#convert to dataframe
data <- as.data.frame(data)

#rename columns
names(data)[names(data) == "Participant number"] <- "participant_nr"

#transform rt to numeric
data$sartRT <- as.numeric(data$sartRT)

# separate 

# z z-transformation of RTs within-subject
data <- data %>%
  group_by(participant_nr) %>%
  mutate(sartRT_z = (sartRT - mean(sartRT, na.rm = T))/sd(sartRT, na.rm = T))

# absolute
data$sartRT_z_abs <- abs(data$sartRT_z)

#The Q-Q plots showed that the random effects were normally distributed, 
#and there seemed to be no dispersion problems or high multicollinearity between the fixed effects.
qqPlot(data$sartRT_z_abs)

# Values for trials without responses (omission errors and correct omissions to 
# target mountain scenes) were linearly interpolated from the reaction times of 
# the two surrounding trials

# Coefficient of Variability (CV): SD of the 8 trials before TP/ MEAN 
# CV = SDRT/MRT

data['rtcv'] <- NA
split_data <- split(data, as.factor(data$participant_nr))
rtcv_data <- data.frame(NULL)
for (participant in split_data) {
  participant <- as.data.frame(participant)
  for (i in 1:length(participant$trial_number)) {
    if (i > 8) {
      participant$rtcv[i] <- sd(participant$sartRT_z_abs[c(i-8:1)], na.rm = T) / mean(participant$sartRT_z_abs[c(i-8:1)], na.rm = T)
    } else {
      participant$rtcv[i] <- sd(participant$sartRT_z_abs[c(i:9)], na.rm = T) / mean(participant$sartRT_z_abs[c(i:8)], na.rm = T)
    }
  }
  rtcv_data <- rbind(rtcv_data, participant)
}

#nope
for (i in 1:length(data$sartRT)) {
  group_data <- data %>% group_by(participant_nr)
  if (i > 8) {
    data$rtcv[i] <- sd(group_data$sartRT_z_abs[c(i-8:1)], na.rm = T) / mean(group_data$sartRT_z_abs[c(i-8:1)], na.rm = T)
    } else {
      data$rtcv[i] <- sd(group_data$sartRT_z_abs[c(i:9)], na.rm = T) / mean(group_data$sartRT_z_abs[c(i:8)], na.rm = T)
      }
  }

# rtcv plot per participant
split_data <- split(rtcv_data, as.factor(data$participant_nr))
ggplot(split_data$`13`, aes(x = trial_number, y = rtcv)) + geom_line()

rtcv_data %>%
  filter(participant_nr == 13) %>%
  ggplot(aes(x = trial_number, y = rtcv)) + geom_line()

# outliers
hist(rtcv_data$rtcv,
     xlab = "rtcv",
     main = "Histogram of rtcv",
     breaks = sqrt(nrow(rtcv_data))
)
ggplot(rtcv_data) +
  aes(x = "", y = rtcv) +
  geom_boxplot(fill = "#0c4c8a") +
  theme_minimal()

ggplot(rtcv_data, aes(x = trial_number, y = rtcv)) + geom_line()

max(rtcv_data$rtcv, na.rm = T)
row <- which(rtcv_data$rtcv == max(rtcv_data$rtcv, na.rm = T), arr.ind=TRUE) # find row outlier
rtcv_data <- rtcv_data %>% #remove outlier
  filter(row_number() != row)

out <- boxplot.stats(data$rtcv)$out
out_ind <- which(data$rtcv %in% c(out))
out_ind

out <- boxplot.stats(data$sartRT_z)$out
out_ind <- which(data$sartRT_z %in% c(out))
length(out_ind)

# smoothing
kernsm(y, h = 1, kern = "Gaussian", m = 0, nsector = 1, sector = 1,
       symmetric = FALSE, unit = c("SD","FWHM"))
?kernsm

################################# Pupil #########################################

# import and merge data
eye_files <- list.files('/Users/ana-maria/Documents/ResMas/Internship/Data/SART', 
                             pattern = ".txt$", recursive = TRUE, full.names = TRUE)

read_eye <- function(nr_participant){
  eye_data <- read_delim(eye_files[nr_participant], 
                         col_names = c("trial", "time", "eye_movement", "pupil", "block"))
  eye_data$participant_nr <- nr_participant
  eye_data$pupil_z <- scale(eye_data$pupil)
  new_block <- which(eye_data$trial == 1 & eye_data$time == 0)
  for (i in 1:new_block[2]) {eye_data$block[i] <- 0}
  for (i in new_block[2]:new_block[3]) {eye_data$block[i] <- 1}
  for (i in new_block[3]:nrow(eye_data)) {eye_data$block[i] <- 2}
  eye_data <- eye_data %>% filter(eye_data$block != 0)
  eye_data <- eye_data %>% group_by(block, trial) %>% mutate(max = max(pupil, na.rm = T),
                                                      max_z = max(pupil_z, na.rm = T))
  eye_data <- eye_data %>% group_by(block, trial) %>% filter(time < 2000) %>% 
    mutate(baseline = mean(pupil, na.rm = T),
           baseline_z = mean(pupil_z, na.rm = T))
  #eye_data <- eye_data[c(1211:79618),] #remove practice trials
  eye_data <- eye_data %>% mutate(phasic = max - baseline,
                                  phasic_z = max_z - baseline_z)
  eye_data <- eye_data %>% filter(time == 0)
  eye_data <- as.data.frame(eye_data)
  return(eye_data)
}

#check
eye_1 <- read_eye(1)
summary(eye_1)

#read all files
eye_data <- data.frame(NULL)
for (i in 1:length(eye_files)) {
  if (i != 10) {
    eye_df <- read_eye(i)
  }
  eye_data <- rbind(eye_data, eye_df)
}

#outliers
hist(eye_data$phasic_z, na.rm = T)
which(eye_data$phasic_z == max(eye_data$phasic_z, na.rm = T), arr.ind=TRUE)
which(eye_data$phasic_z > 12, arr.ind=TRUE)

# previous value of eye_data$max[4300] = 5.007065
eye_data$max[4300] <- median(eye_data$max, na.rm = T) # change with the median
eye_data$max_z[4300] <- median(eye_data$max_z, na.rm = T) # change with the median
eye_data$phasic[4300] <- eye_data$max[4300] - eye_data$baseline[4300]
eye_data$phasic_z[4300] <- eye_data$max_z[4300] - eye_data$baseline_z[4300]

write.csv(eye_data, "eye_data.csv", row.names=FALSE, quote=FALSE) 
save(eye_data,file="eye_data.Rda")
load("eye_data.Rda")

eye_data
?write.csv
########
# z z-transformation of pupil within-subject
eye_data <- eye_data %>%
  group_by(participant_nr) %>%
  mutate(pupil_z = (pupil - mean(pupil, na.rm = T))/sd(pupil, na.rm = T))
# absolute
eye_data$pupil_z_abs <- abs(eye_data$pupil_z)
summary(eye_data)

# phasic response of the pupil was calculated by averaging the eight trials and 
# subtracting the mean baseline value of the pupil (i.e., – 500 ms to stimulus 
# presentation) from the maximum response of the pupil to the stimulus.
# https://doi.org/10.3758/s13414-019-01865-7

eye_data <- eye_data %>%
  group_by(trial) %>%
  filter(time < 2000) %>% # determine when
  mutate(baseline = mean(pupil))


# Finally, our measure of phasic pupil response was calculated by subtracting the 
# normalized terminal pupil diameter (at target onset) from the normalized baseline 
# pupil diameter (at cue onset), consistent with current recommendations 
# (Mathôt et al., 2018) and several recent empirical studies (Eldar et al., 2013; 
# Gilzenrat et al., 2010; Laeng et al., 2011; Mathôt et al., 2014). 
# https://link.springer.com/article/10.3758/s13414-020-02232-7

eye_data['phasic'] <- NA
split_eye_data <- split(eye_data, as.factor(eye_data$participant_nr))
phasic_data <- data.frame(NULL)
for (participant in split_eye_data) {
  participant <- as.data.frame(participant)
  for (i in 1:length(participant$pupil)) {
    if (i > 8) {
      participant$phasic[i] <- sd(participant$pupil[c(i-8:1)], na.rm = T) / mean(participant$pupil, na.rm = T)
    } else {
      participant$phasic[i] <- sd(participant$pupil[c(i:9)], na.rm = T) / mean(participant$pupil, na.rm = T)
    }
  }
  phasic_data <- rbind(rtcv_data, phasic_data)
}

############################## Descriptives ####################################
descriptive_table1 <- round(pastecs::stat.desc(rtcv_data),2)
descriptive_table1


devtools::install_github("rempsyc/rempsyc")
library(rempsyc)

descriptive_table1 <- psych::describe(rtcv_data)
desc_rtcv <- rempsyc::nice_table(descriptive_table1)
rempsyc::save_as_docx(desc_rtcv, path = "/Users/ana-maria/Documents/ResMas/Internship/report/tables.docx")

descriptive_table2 <- psych::describe(eye_data)
desc_eye <- rempsyc::nice_table(descriptive_table2)
rempsyc::save_as_docx(desc_eye, path = "/Users/ana-maria/Documents/ResMas/Internship/report/desc_eye.docx")

descriptive_table3 <- psych::describe(change_df)
desc_test <- rempsyc::nice_table(descriptive_table3)
rempsyc::save_as_docx(desc_test, path = "/Users/ana-maria/Documents/ResMas/Internship/report/desc_test.docx")

median(eye_data$pupil, na.rm =T)
 
################################ MW Time #######################################

#create a common df for rtcv and phasic
rtcv0 <- rtcv_data %>% dplyr::select(participant_nr, block, trial_number, rtcv)
names(rtcv0)[names(rtcv0) == "trial_number"] <- "trial"
phasic0 <- eye_data %>% dplyr::select(participant_nr, block, trial, phasic, phasic_z)
for (i in 1:nrow(phasic0)) {
  if (phasic0$participant_nr[i] >= 27) {
    phasic0$participant_nr[i] <- phasic0$participant_nr[i]+1 
  }
}
mw_df <- merge(rtcv0, phasic0, by = c("participant_nr", "block", "trial"))
mw_df <- mw_df[order(mw_df$participant_nr, mw_df$block, mw_df$trial),]

mw_df_28 <- mw_df %>% filter(participant_nr != 28) 
desc_rtcv <- mw_df %>% group_by(participant_nr) %>%
  summarise(mean_rtcv = mean(rtcv, na.rm = T))
psych::describe(desc_rtcv)
desc_phasic <- mw_df_28 %>% group_by(participant_nr) %>%
  summarise(mean_phasic = mean(phasic_z, na.rm = T))
psych::describe(desc_phasic)

library(depmixS4)  
hist(mw_df$phasic_z)
hist(mw_df$rtcv)
split_data <- split(mw_df, as.factor(mw_df$participant_nr))
split_10 <- split(rtcv0, as.factor(rtcv0$participant_nr))

# mw data
mw_data <- data.frame()

# model rtcv
for (i in 1:length(split_data)) {
  split <- as.data.frame(split_data[i])
  names(split) = names(mw_df)
  #
  mod_rtcv <- depmix(response = rtcv ~ 1, data = split, nstates = 2,
                     trstart = runif(4))
  set.seed(1)
  states_rtcv <- posterior(fit(mod_rtcv, emc=em.control(rand=FALSE)))
  if (sum(states_rtcv$state == 1) > sum(states_rtcv$state == 2)) {
    mw_data[i,"mw_rtcv"] <- sum(states_rtcv$state == 2)
    mw_data[i,"on_rtcv"] <- sum(states_rtcv$state == 1)
  }else{
    mw_data[i,"mw_rtcv"] <- sum(states_rtcv$state == 1)
    mw_data[i,"on_rtcv"] <- sum(states_rtcv$state == 2)
  }
}

ggplot(split, aes(x = trial, y = rtcv, color = states_rtcv$state)) +
  geom_line() +
  theme_classic() 

#model phasic  
for (i in 1:length(split_data)) {
  try({
  split <- as.data.frame(split_data[i]) #3,4,7,10,15,17
  names(split) = names(mw_df)
  split <- na.omit(split)
  #
  mod_phasic <- depmix(response = phasic_z ~ 1, data = split, nstates = 2,
                       trstart = runif(4))
  set.seed(1)
  states_phasic <- posterior(fit(mod_phasic, emc=em.control(rand=F)))
  if (sum(states_phasic$state == 1) > sum(states_phasic$state == 2)) {
    mw_data[i,"mw_phasic"] <- sum(states_phasic$state == 2)
    mw_data[i,"on_phasic"] <- sum(states_phasic$state == 1)
  }else{
    mw_data[i,"mw_phasic"] <- sum(states_phasic$state == 1)
    mw_data[i,"on_phasic"] <- sum(states_phasic$state == 2)
  }
  })
}
ggplot(split, aes(x = trial, y = phasic, color = states_phasic$state)) +
  geom_line() +
  theme_classic() 

# model rtcv + phasic 
for (i in 1:length(split_data)) {
  split <- as.data.frame(split_data[i])
  names(split) = names(mw_df)
  #
  mod_rtcv_phasic <- depmix(list(rtcv~1, phasic_z~1), data = split, nstates = 2,
                            family=list(gaussian(),multinomial("identity")), trstart = runif(4))
  set.seed(1)
  states_rtcv_phasic <- posterior(fit(mod_rtcv_phasic, emc=em.control(rand=FALSE)))
  if (sum(states_rtcv_phasic$state == 1) > sum(states_rtcv_phasic$state == 2)) {
    mw_data[i,"mw_rtcv_phasic"] <- sum(states_rtcv_phasic$state == 2)
    mw_data[i,"on_rtcv_phasic"] <- sum(states_rtcv_phasic$state == 1)
  }else{
    mw_data[i,"mw_rtcv_phasic"] <- sum(states_rtcv_phasic$state == 1)
    mw_data[i,"on_rtcv_phasic"] <- sum(states_rtcv_phasic$state == 2)
  }
}
ggplot(split, aes(x = trial, y = rtcv, color = states_rtcv_phasic$state)) +
  geom_line() +
  theme_classic() 

# final data frame
mw_data_1 <- mw_data
save(mw_data,file="mw_data.Rda")
load("mw_data.Rda")
save(mw_data,file="mw_data1.Rda")

mw_data$participant_nr <- unique(mw_df$participant_nr)
mw_data$participant_nr[30] <- 10
mw_change <- merge(mw_data, change_df, by = "participant_nr")
save(mw_change,file="mw_change.Rda")
load("mw_change.Rda")

#transform to percentages
mw_change<- mw_change %>%
  mutate(mw_rtcv_p = (mw_rtcv / (mw_rtcv + on_rtcv))*100,
         mw_phasic_p = (mw_phasic / (mw_phasic + on_phasic))*100,
         mw_rtcv_phasic_p = (mw_rtcv_phasic / (mw_rtcv_phasic + on_rtcv_phasic))*100)
mw_change_18_28 <- mw_change_28 %>% filter(participant_nr != 18 & participant_nr != 28) # outlier
mw_change <- mw_change %>% mutate(change2 = accuracy2 - accuracy1)
mw_change <- mw_change %>% mutate(accuracy1_p = accuracy1 * 100 / 84,
                                  accuracy2_p = accuracy2 * 100 / 84,
                                  change_p = accuracy2_p - accuracy1_p)
#not normalized phasic
phasic1 <- mw_data %>% dplyr::select(participant_nr, mw_phasic_1, on_phasic_1)
mw_change_28 <- merge(mw_change_28, phasic1, by = "participant_nr")
mw_change_28 <- mw_change_28 %>%
  mutate(mw_phasic_1_p = (mw_phasic_1 / (mw_phasic_1 + on_phasic_1))*100)

# descriptives
round(pastecs::stat.desc(mw_change),2)

# add accuracy only for positive rpe

mw_change <- merge(acc_srpe_df, mw_change, by = "participant_nr")
mw_change <- merge(acc_urpe_df, mw_change, by = "participant_nr")
mw_change <- mw_change %>% mutate(change_srpe = accuracy_srpe_2 - accuracy_srpe_1,
                                  change_urpe = accuracy_urpe_2 - accuracy_urpe_1)
                                  
# Shapiro-Wilk normality test 
shapiro.test(mw_change$mw_rtcv) # => W = 0.83578, p-value = 0.0003168
shapiro.test(mw_change_18_28$mw_rtcv) # W = 0.82011, p-value = 0.0002467
shapiro.test(mw_change$change_p) # NORMAL W = 0.95187, p-value = 0.1897
shapiro.test(mw_change$mw_phasic_p) # W = 0.82108, p-value = 0.0002043
shapiro.test(mw_change$mw_rtcv_phasic_p) # NORMAL W = 0.97296, p-value = 0.6422
hist(rtcv0$rtcv)
hist(phasic0$phasic_z)
max(phasic0$phasic_z)

boxplot(mw_change$change, ylab = "change_p")

#cor btw mw var
cor.test(mw_change_28$mw_rtcv_p, mw_change_28$mw_phasic_p, method=c("spearman"), use = "pairwise.complete.obs")
cor.test(mw_change_18_28$mw_rtcv_phasic_p, mw_change_18_28$mw_rtcv_p, method=c("spearman"), use = "pairwise.complete.obs")
cor.test(mw_change_28$mw_rtcv_p, mw_change_28$mw_rtcv_phasic_p, method=c("spearman"), use = "pairwise.complete.obs")
round(cor(mw_change, use = "pairwise.complete.obs"),2)

#cor w acc
cor.test(mw_change_28$mw_rtcv_p, mw_change_28$accuracy2_p, method=c("spearman"))
ggplot(mw_change_28, aes(x=mw_rtcv_phasic_p, y=change_p)) + 
  geom_point(size = 1.2) +
  geom_smooth(method = "lm", se = F, color = "blue", size = 0.7) +
  theme_apa() +
  xlab("MW time") +
  ylab("Recall Accuracy")

cor.test(mw_change_28$mw_phasic_1_p, mw_change_28$change_p, method=c("pearson"))
ggplot(mw_change_28, aes(x=mw_phasic_1_p, y=change_p)) + 
  geom_point(size = 1.2) +
  geom_smooth(method = "lm", se = F, color = "black", size = 0.7) +
  theme_apa()

# test
cor.test(mw_change$mw_rtcv, mw_change$change2, method=c("spearman"))
cor.test(mw_change$mw_phasic, mw_change$change2, method=c("spearman"), na.action="na.exclude")
cor.test(mw_change$mw_rtcv_phasic, mw_change$change2, method=c("spearman"), na.action="na.exclude")

cor.test(mw_change_28$mw_rtcv_p, mw_change_28$change_p, method=c("spearman"))
cor.test(mw_change_28$mw_phasic_p, mw_change_28$change_p, method=c("spearman"))
cor.test(mw_change_28$mw_rtcv_phasic_p, mw_change_28$change_p, method=c("pearson"), na.action="na.exclude")

lm_rtcv <- lm(change_p ~ mw_rtcv_p, data = mw_change_28)
summary(lm_rtcv)
lm_phasic <- lm(change_p ~ mw_phasic_p, data = mw_change_28)
summary(lm_phasic)
lm_rtcv_phasic <- lm(change_p ~ mw_rtcv_phasic_p, data = mw_change_28)
summary(lm_rtcv_phasic)
plot(lm_rtcv_phasic)

library(apaTables)
apa.reg.table (lm_phasic, lm_rtcv_phasic, filename = "Table1_APA.doc", table.number = 1)

row <- which(mw_change_28$mw_phasic_1_p == max(mw_change_28$mw_phasic_1_p, na.rm = T), arr.ind=TRUE) # find row outlier
row <- which(mw_change$phasic > 200, arr.ind=TRUE) # find row outlier

ggplot(mw_change_28, aes(x=mw_rtcv_phasic_p, y=change)) + 
  geom_point(size = 1.2) +
  geom_smooth(method = "lm", se = F, color = "black", size = 0.7) +
  theme_apa() 

plot(change_p ~ mw_rtcv_p, data = mw_change)
abline(lm_rtcv)

lm0 <- lmer(accuracy2_p ~ (1|accuracy1_p), data=mw_change_10_28)
lm1 <- lmer(accuracy2_p ~ mw_rtcv_p * mw_phasic_p + (1|accuracy1_p), data=mw_change_10_28)
summary(lm1)
anova(lm0, lm4)
lm2 <- lmer(accuracy2_p ~ mw_rtcv_p + (1|accuracy1_p), data=mw_change_10_28)
summary(lm2)
drop1(lm1, test="Chisq")
anova(lm0, lm1, lm2, lm3, lm4)
lm3 <- lmer(accuracy2_p ~ mw_phasic_p + (1|accuracy1_p), data=mw_change_10_28)
summary(lm3)
drop1(lm3, test="Chisq", na.action = na.omit)
lm4 <- lmer(accuracy2_p ~ mw_rtcv_phasic_p + (1|accuracy1_p), data=mw_change_10_28)
summary(lm4)
drop1(lm4, test="Chisq")

library(nlme)

fit <- fit(model, emc=em.control(rand=FALSE))
summary(fit)
# predict the states 
est.states <- posterior(fit)
head(est.states)
as.data.frame(table(est.states$state))


#state 1 means on-task
#state 2 = MW

############################## Main analysis ###################################
psych::describe(mw_change_28)
#test only on positive rpe

cor.test(mw_change_28$mw_rtcv_p, mw_change_28$change_urpe, method=c("spearman"), use = "pairwise.complete.obs")
cor.test(mw_change_28$mw_phasic_p, mw_change_28$change_urpe, method=c("spearman"))
cor.test(mw_change_28$mw_rtcv_phasic_p, mw_change_28$change_urpe, method=c("pearson"), na.action="na.exclude")

cor.test(mw_change_28$mw_rtcv_p, mw_change_28$change_srpe, method=c("spearman"))
cor.test(mw_change_28$mw_phasic_p, mw_change_28$change_srpe, method=c("spearman"))
cor.test(mw_change_28$mw_rtcv_phasic_p, mw_change_28$change_srpe, method=c("pearson"))

corrs <- mw_change_28 %>% dplyr::select(mw_rtcv_p, mw_phasic_p, mw_rtcv_phasic_p, change_p, change_urpe, change_srpe)
corr_sp <- round(cor(corrs, use = "pairwise.complete.obs", method = "spearman"), 2)
corr_p <- round(cor(corrs, use = "pairwise.complete.obs", method = "pearson"), 2)
ggcorrplot(corr_sp)
ggcorrplot(corr_sp,
           #hc.order = TRUE, 
           #type = "lower",
           outline.color = "white",
           ggtheme = theme_bw(),
           #colors = c("#F8696B", "#FFEB84", "#63BE7B"),
           legend.title = "Correlation",
           #lab = TRUE,
           #lab_size = 3,
           tl.cex = 8,
           #tl.srt = 0,
           #title = "Correlation Between MW time and Recall Accuracy (RA)"
           ) +
  scale_x_discrete(labels = c("MW RTCV", "MW phasic", "MW time", "RA", "RA URPE", "RA SRPE"))+
  scale_y_discrete(labels = c("MW RTCV", "MW phasic", "MW time", "RA", "RA URPE", "RA SRPE"))

#power analysis
pwr.r.test(n = 29, r = 0.1)
pwr.f2.test(u =1, v = 27, f2 = 0.02)


?pwr.r.test
lm_rtcv_urpe <- lm(change_urpe ~ mw_rtcv, data = mw_change_18_28)
summary(lm_rtcv_urpe)
lm_phasic_urpe <- lm(change_urpe ~ mw_phasic, data = mw_change_18_28)
summary(lm_phasic_urpe)
lm_rtcv_phasic_urpe <- lm(change_urpe ~ mw_rtcv_phasic, data = mw_change_28)
summary(lm_rtcv_phasic_urpe)

lm_rtcv_srpe <- lm(change_srpe ~ mw_rtcv, data = mw_change_18_28)
summary(lm_rtcv_srpe)
lm_phasic_srpe <- lm(change_srpe ~ mw_phasic, data = mw_change_28)
summary(lm_phasic_srpe)
lm_rtcv_phasic_srpe <- lm(change_srpe ~ mw_rtcv_phasic, data = mw_change_28)
summary(lm_rtcv_phasic_srpe)

library(apaTables)
apa.reg.table (lm_rtcv_phasic_srpe, filename = "Table1_APA.doc", table.number = 1)

mw_change_no10 <- mw_change %>% filter(participant_nr!=10)
lm0 <- lmer(accuracy_srpe_2 ~ (1|accuracy_srpe_1), data=mw_change_10_28)
lm1 <- lmer(accuracy_urpe_2 ~ mw_rtcv_p * mw_phasic_p + (1|accuracy_urpe_1), data=mw_change_10_28)
summary(lm1)
lm2 <- lmer(accuracy_urpe_2 ~ mw_rtcv + (1|accuracy_urpe_1), data=mw_change_10_28)
summary(lm2)
drop1(lm2, test="Chisq")
anova(lm0, lm4)
lm3 <- lmer(accuracy_urpe_2 ~ mw_phasic + (1|accuracy_urpe_1), data=mw_change_10_28)
summary(lm3)
drop1(lm3, test="Chisq", na.action = na.omit)
lm4 <- lmer(accuracy_srpe_2 ~ mw_rtcv_phasic + (1|accuracy_srpe_1), data=mw_change_10_28)
summary(lm4)
drop1(lm4, test="Chisq")

#compare accuracies
compare_acc <- mw_change_28 %>% dplyr::select(accuracy1_p, accuracy2_p)
long_acc <- melt(compare_acc)

chisq.test(long_acc$variable, long_acc$value)

############################### References #####################################

citation('lme4')
citation('depmixS4')




library(ggplot2)
library(ggsignif)
library(factoextra)
library(tidyverse)
library(hrbrthemes)
library(viridis)
library(comprehenr)
library(afex)
library(brms)
library(effects)
library(sjPlot)

###
#step 1: read in all the data, make sure only relevant data is used
###

setwd("C:/Users/fgoetmae/OneDrive - UGent/Documents/Projects/Semantic/data/SpatialSemantic/Full")

#read in all relevant files
data <- read.csv("data2.csv")
info <- read.csv("info_all.csv")
#only use participants that have semantic data
info <- info[info$subjectID %in% data$subjectID,]

nr_participants <- length(unique(data$subjectID))
nr_blocks = 10
nr_trials = 21

info$performance <- info$score_se
#but first we need to add all AQ, age, gender data to the data dataframe
#group <- c()
age <- c()
pc1 <- c()
gender <- c()
SDS <- c()
srs <- c()
PAQ <- c()
for (p in 1:length(unique(data$subjectID))){
  participant = unique(data$subjectID)[p]
  #group <- append(group, to_vec(for (i in 1:(nr_trials*nr_blocks)) info[info$subjectID == participant,]$group))
  age <- append(age, to_vec(for (i in 1:(nr_trials*nr_blocks)) info[info$subjectID == participant,]$Age))
  pc1 <- append(pc1, to_vec(for (i in 1:(nr_trials*nr_blocks)) info[info$subjectID == participant,]$CATI))
  gender <- append(gender, to_vec(for (i in 1:(nr_trials*nr_blocks)) info[info$subjectID == participant,]$Sex))
  SDS <- append(SDS, to_vec(for (i in 1:(nr_trials*nr_blocks)) info[info$subjectID == participant,]$SDS))
  srs <- append(srs, to_vec(for (i in 1:(nr_trials*nr_blocks)) info[info$subjectID == participant,]$ASRS))
  PAQ <- append(PAQ, to_vec(for (i in 1:(nr_trials*nr_blocks)) info[info$subjectID == participant,]$PAQ))
}
#data["group"] <- group
data["age"] <- age
data["PCA"] <- pc1
data["gender"] <- gender
data["SDS"] <- SDS
data["SRS"] <- srs
data["PAQ"] <- PAQ
#for lindear trends, we use the logdistance and the logtrial
#add the log of trial_nr and distance to the dataframes
data["logtrial"] <- log(data["trial_nr"]+0.00001,10) 
data["logdistance"] <- ifelse(data$distance < 100, log(data$distance +0.00001, 10), log(12, 10)) #make sure no inf values are in here
data["logdistance_prev"] <- log(data["distance_prev"]+0.00001, 10)


options(contrasts = c("contr.sum", "contr.poly"))

#rescale factors for L(M)Models
info_Wfilter <- info[!is.na(info$CATI),]
info_Wfilter$PCA <- scale(info_Wfilter$CATI)
info_Wfilter$gender <- as.factor(info_Wfilter$Sex)
info_Wfilter$age <- scale(info_Wfilter$Age)
info_Wfilter$SDS <- scale(info_Wfilter$SDS)
info_Wfilter$SRS <- scale(info_Wfilter$ASRS)
info_Wfilter$PAQ <- scale(info_Wfilter$PAQ)

data_Wfilter <- data[data$subjectID %in% info_Wfilter$subjectID,]
data_Wfilter$PCA <- scale(data_Wfilter$PCA)
data_Wfilter$gender <- as.factor(data_Wfilter$gender)
data_Wfilter$age <- scale(data_Wfilter$age)
data_Wfilter$SDS <- scale(data_Wfilter$SDS)
data_Wfilter$SRS <- scale(data_Wfilter$SRS)
data_Wfilter$PAQ <- scale(data_Wfilter$PAQ)
data_Wfilter$trial_nr <- scale(data_Wfilter$trial_nr)
data_Wfilter$logtrial <- scale(data_Wfilter$logtrial)
data_Wfilter$block_nr <- scale(data_Wfilter$block_nr)


setwd("../../../analysis/Experiment2/Semantic")
####
#extra informative figures
###
#trend of behavioral measures over trials
#novel clicks
d <- aggregate(data$new_click, list(subjectID = data$subjectID, trial_nr = data$trial_nr), FUN=mean, digits=3)
d$x <- d$x * nr_blocks #to rescale from a percentage to a number
m <- d %>%
  group_by(trial_nr) %>% 
  summarise(mean_x = mean(x), lower_ci = mean_x - sd(x) / sqrt(nr_participants), upper_ci = mean_x + sd(x)/sqrt(nr_participants))
ggplot() +
  geom_point(data = d, aes(x = trial_nr, y = x), color = "grey", alpha = 0.08, position = position_jitter(width=1,height=.1)) +  # Individual data points
  geom_line(data = m, aes(x = trial_nr, y = mean_x), color = "darkolivegreen") +  # Mean line
  geom_ribbon(data = m, aes(x = trial_nr, ymin = lower_ci, ymax = upper_ci), fill = "darkolivegreen", alpha = 0.3) +  # Confidence interval ribbon
  theme_classic()
ggsave("nov(trial).png", device = "png", height = 5/0.8, width = 6/0.8)
#high value clicks
d <- aggregate(data$HV_click, list(subjectID = data$subjectID, trial_nr = data$trial_nr), FUN=mean)
d$x <- d$x * nr_blocks #to rescale from a percentage to a number
m <- d %>%
  group_by(trial_nr) %>% 
  summarise(mean_x = mean(x), lower_ci = mean_x - sd(x) / sqrt(nr_participants), upper_ci = mean_x + sd(x)/sqrt(nr_participants))
ggplot() +
  geom_point(data = d, aes(x = trial_nr, y = x), color = "grey", alpha = 0.08, position = position_jitter(width=1,height=.1)) +  # Individual data points
  geom_line(data = m, aes(x = trial_nr, y = mean_x), color = "darkolivegreen") +  # Mean line
  geom_ribbon(data = m, aes(x = trial_nr, ymin = lower_ci, ymax = upper_ci), fill = "darkolivegreen", alpha = 0.3) +  # Confidence interval ribbon
  theme_classic()
ggsave("HV(trial).png", device = "png", height = 5/0.8, width = 6/0.8)
#distance from previous click
d <- aggregate(data$distance_prev, list(data$subjectID, trial_nr = data$trial_nr), FUN=mean, digits = 4)
m <- d %>%
  group_by(trial_nr) %>% 
  summarise(mean_x = mean(x), lower_ci = mean_x - sd(x) / sqrt(nr_participants), upper_ci = mean_x + sd(x)/sqrt(nr_participants))
ggplot() +
  geom_point(data = d, aes(x = trial_nr, y = x), color = "grey", alpha = 0.08, position = position_jitter(width=1,height=.1)) +  # Individual data points
  geom_line(data = m, aes(x = trial_nr, y = mean_x), color = "darkolivegreen") +  # Mean line
  geom_ribbon(data = m, aes(x = trial_nr, ymin = lower_ci, ymax = upper_ci), fill = "darkolivegreen", alpha = 0.3) +  # Confidence interval ribbon
  theme_classic()
ggsave("Dprev(trial).png", device = "png", height = 5/0.8, width = 6/0.8)
ggplot() +
  geom_point(data = d, aes(x = trial_nr, y = x), alpha = 0.2, position = position_jitter(width=1,height=.1)) +  # Individual data points
  geom_line(data = m, aes(x = trial_nr, y = mean_x), color = "blue") +  # Mean line
  geom_ribbon(data = m, aes(x = trial_nr, ymin = lower_ci, ymax = upper_ci), fill = "blue", alpha = 0.3) +  # Confidence interval ribbon
  theme_classic() + scale_x_log10()
ggsave("DprevLOGLOG(trial).png", device = "png", height = 5/0.8, width = 6/0.8)
#distance from hv cell
d <- aggregate(data$distance, list(data$subjectID, trial_nr = data$trial_nr), FUN=mean, digits = 4)
m <- d %>%
  group_by(trial_nr) %>% 
  summarise(mean_x = mean(x), lower_ci = mean_x - sd(x) / sqrt(nr_participants), upper_ci = mean_x + sd(x)/sqrt(nr_participants))
ggplot() +
  geom_point(data = d, aes(x = trial_nr, y = x), color = "grey", alpha = 0.08, position = position_jitter(width=1,height=.1)) +  # Individual data points
  geom_line(data = m, aes(x = trial_nr, y = mean_x), color = "darkolivegreen") +  # Mean line
  geom_ribbon(data = m, aes(x = trial_nr, ymin = lower_ci, ymax = upper_ci), fill = "darkolivegreen", alpha = 0.3) +  # Confidence interval ribbon
  theme_classic()
ggsave("D(trial).png", device = "png", height = 5/0.8, width = 6/0.8)
ggplot() +
  geom_point(data = d, aes(x = trial_nr, y = x), alpha = 0.2, position = position_jitter(width=1,height=.1)) +  # Individual data points
  geom_line(data = m, aes(x = trial_nr, y = mean_x), color = "blue") +  # Mean line
  geom_ribbon(data = m, aes(x = trial_nr, ymin = lower_ci, ymax = upper_ci), fill = "blue", alpha = 0.3) +  # Confidence interval ribbon
  theme_classic() + scale_x_log10()
ggsave("DLOG(trial).png", device = "png", height = 5/0.8, width = 6/0.8)

###
###
#Section 1: Score
###
###
print(mean(info_Wfilter$performance))
print(sd(info_Wfilter$performance))

#A2: within group comparison
nrow(info_Wfilter)
cor.test(info_Wfilter$performance,info_Wfilter$PCA , method="pearson")
cor.test(info_Wfilter$performance,info_Wfilter$PCA , method="spearman", exact = FALSE)
ggplot(info_Wfilter, aes(x = PCA, y = performance)) + geom_point() + geom_smooth(method = 'lm', color = "deepskyblue4", fill = "deepskyblue") +
  #annotate("text", x = max(info_Wfilter$PCA)-2, y = max(drop_na(info_Wfilter, performance)$performance - 1), label = paste("p = ", toString(round(c$p.value, digits = 3)))) +
  theme_classic() 
ggsave("p(PCA).png", device = "png", height = 5/0.8, width = 6/0.8)

###
###
#Section 2: behavioral measurements for exploration
###
###
#for plotting purposes: create summary dataframe showing one trial per group/autistic traits bin
#bin the PCA data in same bins as model: -3, -1, 0 (0.1), 2, 3
bin_centers <- c(-2, -1, 0.06, 1, 2) #edit based on fit bins 
# find the closest bin center for each value in list
find_closest_bin <- function(value, bin_centers) {
  distances <- abs(bin_centers - value)
  closest_bin <- which.min(distances)
  return(closest_bin)
}
#data_Wfilter$PCAbin <- v[sapply(data_Wfilter$PCA, find_closest_bin, bin_centers = bin_centers)]
data_Wfilter$PCAbin <- unlist(sapply(data_Wfilter$PCA, find_closest_bin, bin_centers = bin_centers))
#data_Wfilter %>% mutate(PCAbin = cut(PCA, breaks = breaks))
name_W <- data_Wfilter %>% group_by(PCAbin, trial_nr)
plot_dfW <- name_W %>% summarise(Novclicks = mean(new_click), HVclicks = mean(HV_click), D = mean(distance), Dprev = mean(distance_prev))

###
#Novel clicks
###
#within group
#modelfull <- glmer(new_click ~ (PCA + trial_nr + SDS + SRS + PAQ)^2 + (1+trial_nr|subjectID), data = data_Wfilter, family = binomial, control = glmerControl(optimizer = "bobyqa"))
modelfull <- glmer(new_click ~ (PCA + trial_nr + SDS + SRS + PAQ)^2 + gender + age + (1+trial_nr|subjectID), data = data_Wfilter, family = binomial, control = glmerControl(optimizer = "bobyqa"))
summary(modelfull)
z <- as.data.frame(effect("PCA:trial_nr", modelfull))
plot_dfW$PCA <- unique(as.factor(z$PCA))[plot_dfW$PCAbin]
ggplot() +
  geom_ribbon(data = z, aes(x = trial_nr, ymin = lower, ymax = upper, group = PCA, fill = as.factor(PCA)), alpha = 0.3, show.legend = FALSE) +
  geom_line(data = z, aes(x = trial_nr, y = fit, group = PCA, color = as.factor(PCA)), size = 1, show.legend = TRUE) +
  geom_point(data = plot_dfW, aes(x = trial_nr, y = Novclicks, color = as.factor(PCA), size = 0.7), show.legend = TRUE) +
  labs(x = "Trial Number", y = "Number of novel clicks", color = "CATI", fill = "CATI") +
  theme_classic() + scale_color_manual(values = c("deepskyblue", "grey", "yellow", "orange", "darkorange")) + scale_fill_manual(values=c("deepskyblue", "grey", "yellow", "orange", "darkorange")) 
ggsave("Nov(trial)PCA.png", device = "png", height = 5, width = 6)

#without controlling
modelfull <- glmer(new_click ~ (PCA + trial_nr)^2 + (1+trial_nr|subjectID), data = data_Wfilter, family = binomial, control = glmerControl(optimizer = "bobyqa"))
summary(modelfull)
z <- as.data.frame(effect("PCA:trial_nr", modelfull))
plot_dfW$PCA <- unique(as.factor(z$PCA))[plot_dfW$PCAbin]
ggplot() +
  geom_ribbon(data = z, aes(x = trial_nr, ymin = lower, ymax = upper, group = PCA, fill = as.factor(PCA)), alpha = 0.3, show.legend = FALSE) +
  geom_line(data = z, aes(x = trial_nr, y = fit, group = PCA, color = as.factor(PCA)), size = 1, show.legend = TRUE) +
  geom_point(data = plot_dfW, aes(x = trial_nr, y = Novclicks, color = as.factor(PCA), size = 0.7), show.legend = TRUE) +
  labs(x = "Trial Number", y = "Number of novel clicks", color = "CATI", fill = "CATI") +
  theme_classic() + scale_color_manual(values = c("deepskyblue", "grey", "yellow", "orange", "darkorange")) + scale_fill_manual(values=c("deepskyblue", "grey", "yellow", "orange", "darkorange")) 
ggsave("Nov(trial)PCAnocontrols.png", device = "png", height = 5, width = 6)

###
#High value clicks
###
#within group
modelfull <- glmer(HV_click ~ (PCA + trial_nr + SDS + SRS + PAQ)^2 + gender + age + (1+trial_nr|subjectID), data = data_Wfilter, family = binomial, control = glmerControl(optimizer = "bobyqa"))
#modelfull <- glmer(HV_click ~ (PCA + trial_nr + SDS + SRS + PAQ)^2 + (1+trial_nr|subjectID), data = data_Wfilter, family = binomial, control = glmerControl(optimizer = "bobyqa"))
summary(modelfull)
z <- as.data.frame(effect("PCA:trial_nr", modelfull))
ggplot() +
  geom_ribbon(data = z, aes(x = trial_nr, ymin = lower, ymax = upper, group = PCA, fill = as.factor(PCA)), alpha = 0.3, show.legend = FALSE) +
  geom_point(data = plot_dfW, aes(x = trial_nr, y = HVclicks, color = as.factor(PCA), size = 0.7), show.legend = TRUE) +
  geom_line(data = z, aes(x = trial_nr, y = fit, group = PCA, color = as.factor(PCA)), size = 1, show.legend = TRUE) +
  labs(x = "Trial Number", y = "Number of high value clicks", color = "PCA", fill = "PCA") +
  theme_classic() + scale_color_manual(values = c("deepskyblue", "grey", "yellow", "orange", "darkorange")) + scale_fill_manual(values=c("deepskyblue", "grey", "yellow", "orange", "darkorange")) 
ggsave("HV(trial)PCA.png", device = "png", height = 5, width = 6)

#without controlling
modelfull <- glmer(HV_click ~ (PCA + trial_nr)^2 + (1+trial_nr|subjectID), data = data_Wfilter, family = binomial, control = glmerControl(optimizer = "bobyqa"))
summary(modelfull)
z <- as.data.frame(effect("PCA:trial_nr", modelfull))
ggplot() +
  geom_ribbon(data = z, aes(x = trial_nr, ymin = lower, ymax = upper, group = PCA, fill = as.factor(PCA)), alpha = 0.3, show.legend = FALSE) +
  geom_point(data = plot_dfW, aes(x = trial_nr, y = HVclicks, color = as.factor(PCA), size = 0.7), show.legend = TRUE) +
  geom_line(data = z, aes(x = trial_nr, y = fit, group = PCA, color = as.factor(PCA)), size = 1, show.legend = TRUE) +
  labs(x = "Trial Number", y = "Number of high value clicks", color = "PCA", fill = "PCA") +
  theme_classic() + scale_color_manual(values = c("deepskyblue", "grey", "yellow", "orange", "darkorange")) + scale_fill_manual(values=c("deepskyblue", "grey", "yellow", "orange", "darkorange")) 
ggsave("HV(trial)PCAnocontrol.png", device = "png", height = 5, width = 6)

###
###
#Section 3: Distance measures for exploration
###
###
###
#distance from most nearby high value cell
###

#within group
m_d <- lmer(logdistance ~ (PCA + logtrial + block_nr + SDS + SRS + PAQ)^2 + gender + age + (1+logtrial*block_nr|subjectID), data = data_Wfilter, control = lmerControl(optimizer = "bobyqa"))
#modelfull <- lmer(logdistance ~ (PCA + logtrial + block_nr + SDS + SRS + PAQ)^2 + (1+logtrial*block_nr|subjectID), data = data_Wfilter, control = lmerControl(optimizer = "bobyqa"))
summary(m_d)
z <- as.data.frame(effect("PCA:logtrial", m_d))
ggplot() +
  geom_line(data = z, aes(x = logtrial, y = fit, group = PCA, color = as.factor(PCA)), size = 1, show.legend = FALSE) +
  geom_ribbon(data = z, aes(x = logtrial, ymin = lower, ymax = upper, group = PCA, fill = as.factor(PCA)), alpha = 0.3, show.legend = FALSE) +
  labs(x = "Log trial Number", y = "Distance from high value cell", color = "PCA", fill = "PCA") +
  theme_classic() + ylim(-5,2) + scale_color_manual(values = c("deepskyblue", "grey", "yellow", "orange", "darkorange")) + scale_fill_manual(values=c("deepskyblue", "grey", "yellow", "orange", "darkorange")) ###
ggsave("d(trial)PCA.png", device = "png", height = 5, width = 6)

#without controlling
m_d_nc <- lmer(logdistance ~ (PCA + logtrial + block_nr)^2 + (1+logtrial*block_nr|subjectID), data = data_Wfilter, control = lmerControl(optimizer = "bobyqa"))
summary(m_d_nc)
z <- as.data.frame(effect("PCA:logtrial", m_d_nc))
ggplot() +
  geom_line(data = z, aes(x = logtrial, y = fit, group = PCA, color = as.factor(PCA)), size = 1, show.legend = FALSE) +
  geom_ribbon(data = z, aes(x = logtrial, ymin = lower, ymax = upper, group = PCA, fill = as.factor(PCA)), alpha = 0.3, show.legend = FALSE) +
  labs(x = "Log trial Number", y = "Distance from high value cell", color = "PCA", fill = "PCA") +
  theme_classic() + ylim(-5,2) + scale_color_manual(values = c("deepskyblue", "grey", "yellow", "orange", "darkorange")) + scale_fill_manual(values=c("deepskyblue", "grey", "yellow", "orange", "darkorange")) ###
ggsave("d(trial)PCAnocontrol.png", device = "png", height = 5, width = 6)

###
#distance from previous click
###
#within group
#modelfull <- lmer(logdistance_prev ~ (PCA + logtrial + block_nr + SDS + SRS + PAQ)^2 + (1+logtrial*block_nr|subjectID), data = data_Wfilter, control = lmerControl(optimizer = "bobyqa"))
m_dprev <- lmer(logdistance_prev ~ (PCA + logtrial + block_nr + SDS + SRS + PAQ)^2 + gender + age + (1+logtrial*block_nr|subjectID), data = data_Wfilter, control = lmerControl(optimizer = "bobyqa"))
summary(m_dprev)
z <- as.data.frame(effect("PCA:logtrial", m_dprev))
ggplot() +
  geom_line(data = z, aes(x = logtrial, y = fit, group = PCA, color = as.factor(PCA)), size = 1, show.legend = FALSE) +
  geom_ribbon(data = z, aes(x = logtrial, ymin = lower, ymax = upper, group = PCA, fill = as.factor(PCA)), alpha = 0.3, show.legend = FALSE) +
  labs(x = "Log trial Number", y = "Distance from high value cell", color = "PCA", fill = "PCA") +
  theme_classic() + ylim(-3,2) + scale_color_manual(values = c("deepskyblue", "grey", "yellow", "orange", "darkorange")) + scale_fill_manual(values=c("deepskyblue", "grey", "yellow", "orange", "darkorange")) 
ggsave("dprev(trial)PCA.png", device = "png", height = 5, width = 6)

#without controlling
m_dprev_nc <- lmer(logdistance_prev ~ (PCA + logtrial + block_nr)^2 + (1+logtrial*block_nr|subjectID), data = data_Wfilter, control = lmerControl(optimizer = "bobyqa"))
summary(m_dprev_nc)
z <- as.data.frame(effect("PCA:logtrial", m_dprev_nc))
ggplot() +
  geom_line(data = z, aes(x = logtrial, y = fit, group = PCA, color = as.factor(PCA)), size = 1, show.legend = FALSE) +
  geom_ribbon(data = z, aes(x = logtrial, ymin = lower, ymax = upper, group = PCA, fill = as.factor(PCA)), alpha = 0.3, show.legend = FALSE) +
  labs(x = "Log trial Number", y = "Distance from high value cell", color = "PCA", fill = "PCA") +
  theme_classic() + ylim(-3,2) + scale_color_manual(values = c("deepskyblue", "grey", "yellow", "orange", "darkorange")) + scale_fill_manual(values=c("deepskyblue", "grey", "yellow", "orange", "darkorange")) 
ggsave("dprev(trial)PCAnocontrol.png", device = "png", height = 5, width = 6)


###
###
#Section 4: modelling results
###
###

infoS <- info

infoS["l"] <- infoS$l_fit_se
infoS["b"] <- infoS$beta_se
infoS["t"] <- infoS$tau_se
infoS["NLL"] <- infoS$NLL.x

info_W <- infoS[infoS$subjectID %in% info_Wfilter$subjectID,]
info_W$gender <- ifelse(info_W$Sex == "Other" | info_W$Sex == "Prefer not to say", "Female", info_W$Sex)
info_W["PCA"] <- scale(info_W$CATI)
info_W["gender"] <- as.factor(info_W$gender)
info_W["age"] <- scale(info_W$Age)
info_W["SDS"] <- scale(info_W$SDS)
info_W["SRS"] <- scale(info_W$ASRS)
info_W["PAQ"] <- scale(info_W$PAQ)
info_W["logl"] <- scale(log(info_W$l, 10))
info_W["logb"] <- scale(log(info_W$b, 10))
info_W["logt"] <- scale(log(info_W$t, 10))
info_W["NLL"] <- scale(info_W$NLL)

###
#for within group:
###
m <- glm(formula = PCA ~  (SDS + SRS + PAQ)^2 + logl + logb + logt + gender + age, family = gaussian, data = info_W)
summary(m)
plot(effect("logb", m),  ci.style="bands")

#without controlling
m_nc <- glm(formula = PCA ~ logl + logb + logt, family = gaussian, data = info_W)
summary(m_nc)
plot(effect("logt", m_nc), ci.style="bands")




####################################
#new analyses: cross trait diagnostics
###################################
#since only 3 pp of PNTS, omit them for the indiv differences study
info <- info[info$Sex == "Female" | info$Sex == "Male",]
info$logl_sc <- scale(log(info$l_fit_se, 10))
info$logb_sc <- scale(log(info$beta_se, 10))
info$logt_sc <- scale(log(info$tau_se, 10))

options(contrasts = c("contr.sum", "contr.poly"))
info$Sex <- factor(info$Sex, levels = c("Male", "Female"))
info$Age_sc <- scale(info$Age)
info$SDS_sc <- scale(info$SDS)
info$ASRS_sc <- scale(info$ASRS)
info$PAQ_sc <- scale(info$PAQ)
info$CATI_sc <- scale(info$CATI)

#score
ms <- glm(formula = score_se ~ (Sex + Age_sc + CATI_sc + SDS_sc + ASRS_sc + PAQ_sc)^2, family = gaussian, data = info)
summary(ms)
plot(effect("Sex", ms), ci.style="bands")
#learning
ml <- glm(formula = slope_score_se ~ (Sex + Age_sc + CATI_sc  + SDS_sc + ASRS_sc + PAQ_sc)^2, family = gaussian, data = info)
summary(ml)

#novel
mn <- glm(formula = Novclicks_se ~ (Sex + Age_sc + CATI_sc + SDS_sc + ASRS_sc + PAQ_sc)^2, family = gaussian, data = info)
summary(mn)
#distance
info$av_distance_se <- ifelse(info$av_distance_se > 100, 6, info$av_distance_se)
md <- glm(formula = av_distance_se ~ (Sex + Age_sc + CATI_sc + SDS_sc + ASRS_sc + PAQ_sc)^2, family = gaussian, data = info)
summary(md)
#distance_prev
mdp <- glm(formula = av_distance_prev_se ~ (Sex + Age_sc + CATI_sc + SDS_sc + ASRS_sc + PAQ_sc)^2, family = gaussian, data = info)
summary(mdp)
plot(effect("Sex", mdp), ci.style="bands")

#novel trend
msn <- glm(formula = slope_novel_se ~ (Sex + Age_sc + CATI_sc + SDS_sc + ASRS_sc + PAQ_sc)^2, family = gaussian, data = info)
summary(msn)
#distance trend
msd <- glm(formula = slope_dhv_se ~ (Sex + Age_sc + CATI_sc + SDS_sc + ASRS_sc + PAQ_sc)^2, family = gaussian, data = info)
summary(msd)
#distance_prev trend
msdp <- glm(formula = slope_dprev_se ~ (Sex + Age_sc + CATI_sc + SDS_sc + ASRS_sc + PAQ_sc)^2, family = gaussian, data = info)
summary(msdp)

#adaptivitiy
ma <- glm(formula = slope_dprew_se ~ (Sex + Age_sc + CATI_sc + SDS_sc + ASRS_sc + PAQ_sc)^2, family = gaussian, data = info)
summary(ma)

#generalization
mlf <- glm(formula = l_fit_se ~ (Sex + Age_sc + CATI_sc + SDS_sc + ASRS_sc + PAQ_sc)^2, family = gaussian, data = info)
summary(mlf)
plot(effect("Sex", mlf), ci.style="bands")
#UG exploration
mug <- glm(formula = beta_se ~ (Sex + Age_sc + CATI_sc + SDS_sc + ASRS_sc + PAQ_sc)^2, family = gaussian, data = info)
summary(mug)
#random exploration
mr <- glm(formula = tau_se ~ (Sex + Age_sc + CATI_sc + SDS_sc + ASRS_sc + PAQ_sc)^2, family = gaussian, data = info)
summary(mr)
#frequency bias
mf <- glm(formula = phi ~ (Sex + Age_sc + CATI_sc + SDS_sc + ASRS_sc + PAQ_sc)^2, family = gaussian, data = info)
summary(mf)

#log generalization
info$logl <- log(info$l_fit_se, 10)
mlf <- glm(formula = logl ~ (Sex + Age_sc + CATI_sc + SDS_sc + ASRS_sc + PAQ_sc)^2, family = gaussian, data = info)
summary(mlf)
#log UG exploration
info$logb <- log(info$beta_se, 10)
mug <- glm(formula = logb ~ (Sex + Age_sc + CATI_sc + SDS_sc + ASRS_sc + PAQ_sc)^2, family = gaussian, data = info)
summary(mug)
plot(effect("CATI_sc", mug), ci.style="bands")
#log random exploration
info$logt <- log(info$tau_se, 10)
mr <- glm(formula = logt ~ (Sex + Age_sc + CATI_sc + SDS_sc + ASRS_sc + PAQ_sc)^2, family = gaussian, data = info)
summary(mr)
#log frequency bias
info$logphi <- log(info$phi, 10)
mf <- glm(formula = logphi ~ (Sex + Age_sc + CATI_sc + SDS_sc + ASRS_sc + PAQ_sc)^2, family = gaussian, data = info)
summary(mf)

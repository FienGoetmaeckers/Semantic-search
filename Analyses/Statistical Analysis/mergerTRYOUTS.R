library(dplyr)
library(misty)
library(tidyverse)
setwd("C:/Users/fgoetmae/OneDrive - UGent/Documents/Projects/Semantic/data/SpatialSemantic/Full")

#read in all relevant files
data_se <- read.csv("data2.csv")
data_sp <- read.csv("data_spat.csv")
info_se <- read.csv("info2.csv")
info_sp <- read.csv("info_spat.csv")
info_q  <- read.csv("q_info.csv")

#rename columns such that there is no overlap between info frames
info_se <- rename(info_se, score_se = score)
info_se <- rename(info_se, bonus_se = bonus)
info_se <- rename(info_se, av_distance_se = av_distance)
info_se <- rename(info_se, av_distance_prev_se = av_distance_prev)
info_se <- rename(info_se, reclicks_se = reclicks)
info_se <- rename(info_se, HVclicks_se = HVclicks)
info_se <- rename(info_se, Novclicks_se = Novclicks)
info_se <- rename(info_se, trials_se = trials)
info_se <- rename(info_se, slope_score_se = slope_score)
info_se <- rename(info_se, slope_novel_se = slope_novel)
info_se <- rename(info_se, slope_dprev_se = slope_dprev)
info_se <- rename(info_se, slope_dhv_se = slope_dhv)
info_se <- rename(info_se, slope_dprew_se = slope_dprew)

info_sp <- rename(info_sp, score_sp = performance)
info_sp <- rename(info_sp, bonus_sp = bonus)
info_sp <- rename(info_sp, av_distance_sp = av_distance)
info_sp <- rename(info_sp, av_distance_prev_sp = av_distance_prev)
info_sp <- rename(info_sp, reclicks_sp = reclicks)
info_sp <- rename(info_sp, HVclicks_sp = HVclicks)
info_sp <- rename(info_sp, Novclicks_sp = Novclicks)
info_sp <- rename(info_sp, trials_sp = trials)
info_sp <- rename(info_sp, slope_score_sp = slope_score)
info_sp <- rename(info_sp, slope_novel_sp = slope_novel)
info_sp <- rename(info_sp, slope_dprev_sp = slope_dprev)
info_sp <- rename(info_sp, slope_dhv_sp = slope_dhv)
info_sp <- rename(info_sp, slope_dprew_sp = slope_dprew)

#read in parameter estimates
#setwd("../../../analysis/Experiment2")
est_se <- read.csv("est_090626.csv") #frequency bias and localized, full space
est_sp <- read.csv("1env_090626.csv") #local bias

est_se <- rename(est_se, l_fit_se = l_fit)
est_se <- rename(est_se, beta_se = beta)
est_se <- rename(est_se, tau_se = tau)

est_sp <- rename(est_sp, l_fit_sp = l_fit)
est_sp <- rename(est_sp, beta_sp = beta)
est_sp <- rename(est_sp, tau_sp = tau)

#demographic info:
demo <- read.csv("prolific_demographic.csv")
demo <- demo[demo$Participant.id %in% info_se$prolificID | demo$Participant.id %in% info_sp$prolificID, c("Participant.id", "Age", "Sex")]
demo <- rename(demo, prolificID = Participant.id)
demo$Age <- as.numeric(demo$Age)
info_q <- list(info_q, demo) %>% reduce(full_join, by="prolificID")

#bring all of them together in one dataframe
info <- list(info_se, info_sp, info_q, est_se, est_sp) %>% reduce(full_join, by="subjectID")
#info <- list(info_se, info_sp, info_q) %>% reduce(full_join, by="subjectID")
#clean duplicate and meaningless columns
info$prolificID <- ifelse(!is.na(info$prolificID.x), info$prolificID.x, info$prolificID.y)
info <- subset(info, select = - c(X.x, X.y, X, prolificID.x, prolificID.y))
info$tot_bonus <- ifelse(is.na(info$bonus_sp), info$bonus_se + mean(info$bonus_sp, na.rm=TRUE), info$bonus_se + info$bonus_sp)
#remove full NA rows
non_na_rows <- info[rowSums(is.na(info)) < ncol(info), ]

#setwd("../../data/SpatialSemantic")
write.csv(info, "info_all.csv")

setwd("C:/Users/fgoetmae/OneDrive - UGent/Documents/Projects/Semantic/analysis/SpatialSemantic")
##############
#study overlap
##############
#assess group size:
nrow(info)


overlap <- function(data, var_name, name){
  var_se <- paste0(var_name, "_se")
  var_sp <- paste0(var_name, "_sp")
  #correlations
  print(cor.test(data[[var_se]], data[[var_sp]], method = "pearson"))
  cor <- cor.test(data[[var_se]], data[[var_sp]], method="spearman", exact = FALSE)
  p <- round(cor$p.value, 3)
  if (p < 0.001){
    ptext <- "p < .001"
  }else{
    ptext <- paste0("p = ", p)
  }
  r <- round(cor$estimate, 2)
  #plotting
  #position of text
  #first check what maximum values are:
  max_x <- sort(unique(data[[var_se]]))[length(unique(data[[var_se]]))-2] #second highest value for x-axis
  if (r > 0){
    #then bottom right
    xpos <- min(na.omit(data[[var_se]])) + 0.8*(max_x - min(na.omit(data[[var_se]])))
  }else{
    #then bottom left
    xpos <- min(na.omit(data[[var_se]])) + 0.3*(max_x - min(na.omit(data[[var_se]])))
  }
  ypos <- min(na.omit(data[[var_sp]])) + 0.05*(max(na.omit(data[[var_sp]])) - min(na.omit(data[[var_sp]])))
  max <- max(max(na.omit(data[[var_se]])), max(na.omit(data[[var_sp]])))
  min <- min(min(na.omit(data[[var_se]])), min(na.omit(data[[var_sp]])))
  #plot itself
  p <- ggplot(data, aes(x = .data[[var_se]], y = .data[[var_sp]])) + 
    geom_point() + 
    geom_smooth(method = 'lm', color = "indianred4", fill = "indianred2") +
    theme_classic() +
    labs(x = paste("Semantic", name), y = paste("Spatial", name)) +
    #geom_label(aes(x = xpos, y = ypos, fontface = 40, label = paste0("r = ", r, ", ", ptext))) + 
    annotate("text", x = xpos, y = ypos, size = 8, label = paste0("r = ", r, ", ", ptext)) + 
    theme(text = element_text(size = 30))
  print(p)
  ggsave(paste0(var_sp, "(", var_se, ").png"), plot = p, device = "png", height = 5/0.8, width = 6/0.8)
}

####score####
overlap(info, "score", "score")
overlap(info, "slope_score", "learning")

#novelclicks
overlap(info, "Novclicks", "novel clicks")
overlap(info, "slope_novel", "trend novel clicks")

#Dprev
overlap(info, "av_distance_prev", "distance from previous choice")
overlap(info, "slope_dprev", "trend distance from previous choice")

#in function of reward
overlap(info, "slope_dprew", "adaptiveness")

#DHV
overlap(info, "av_distance", "distance from high value choice")
overlap(info, "slope_dhv", "trend distance from high value choice")


####
#correlations between comp measures
####
#l_fit
cor.test(info$l_fit_se,info$l_fit_sp , method="pearson")
cor <- cor.test(info$l_fit_se,info$l_fit_sp, method="spearman", exact = FALSE)
p <- round(cor$p.value, 3)
if (p < 0.001){
  ptext <- "p < .001"
}else{
  ptext <- paste0("p = ", p)
}
r <- round(cor$estimate, 2)
ggplot(info, aes(x = l_fit_se, y = l_fit_sp)) + geom_point() + geom_smooth(method = 'lm', color = "indianred4", fill = "indianred2") +
  theme_classic() + 
  labs(x = "Semantic generalization", y = "Spatial generalization") + 
  scale_x_log10() + scale_y_log10() + 
  annotate("text", x = 0.8, y = 0.3, size = 8, label = paste0("r = ", r, ", ", ptext)) + 
  theme(text = element_text(size = 30))
ggsave("l_fit_sp(l_fit_se).png", device = "png", height = 5/0.8, width = 6/0.8)

#beta
cor.test(info$beta_se,info$beta_sp , method="pearson")
cor <- cor.test(info$beta_se,info$beta_sp, method="spearman", exact = FALSE)
p <- round(cor$p.value, 3)
if (p < 0.001){
  ptext <- "p < .001"
}else{
  ptext <- paste0("p = ", p)
}
r <- round(cor$estimate, 2)
ggplot(info, aes(x = beta_se, y = beta_sp)) + geom_point(position = "jitter") + geom_smooth(method = 'lm', color = "indianred4", fill = "indianred2") +
  theme_classic() + 
  labs(x = "Semantic UG exploration", y = "Spatial UG exploration") + 
  scale_x_log10() + scale_y_log10() + 
  annotate("text", x = 0.3, y = 0.01, size = 8, label = paste0("r = ", r, ", ", ptext)) + 
  theme(text = element_text(size = 30))
ggsave("beta_sp(beta_se).png", device = "png", height = 5/0.8, width = 6/0.8)

#beta but without all the lower bounds clippers
info$beta_se <- as.numeric(round(info$beta_se, 7))
info$beta_sp <- as.numeric(round(info$beta_sp, 7))
clipper <- sort(unique(info$beta_se))[1] #lower bound
info_filter <- info[info$beta_se > clipper & info$beta_sp > clipper,]
cor.test(info_filter$beta_se,info_filter$beta_sp , method="pearson")
cor <- cor.test(info_filter$beta_se,info_filter$beta_sp, method="spearman", exact = FALSE)
p <- round(cor$p.value, 3)
if (p < 0.001){
  ptext <- "p < .001"
}else{
  ptext <- paste0("p = ", p)
}
r <- round(cor$estimate, 2)
ggplot(info_filter, aes(x = beta_se, y = beta_sp)) + geom_point() + geom_smooth(method = 'lm', color = "indianred4", fill = "indianred2") +
  theme_classic() + 
  labs(x = "Semantic UG exploration", y = "Spatial UG exploration") + theme(text = element_text(size = 20)) + 
  scale_x_log10() + scale_y_log10() + 
  geom_label(aes(x = 0.7, y = 0.01, label = paste0("r = ", r, ", ", ptext)))
ggsave("filtered_beta_sp(beta_se).png", device = "png", height = 5/0.8, width = 6/0.8)

#tau
cor.test(info$tau_se,info$tau_sp , method="pearson")
cor <- cor.test(info$tau_se,info$tau_sp, method="spearman", exact = FALSE)
p <- round(cor$p.value, 3)
if (p < 0.001){
  ptext <- "p < .001"
}else{
  ptext <- paste0("p = ", p)
}
r <- round(cor$estimate, 2)
ggplot(info, aes(x = tau_se, y = tau_sp)) + geom_point() + geom_smooth(method = 'lm', color = "indianred4", fill = "indianred2") +
  theme_classic() + scale_x_log10() + scale_y_log10() + 
  labs(x = "Semantic random exploration", y = "Spatial random exploration") +
  annotate("text", x = 0.2, y = 0.03, size = 8, label = paste0("r = ", r, ", ", ptext)) + 
  theme(text = element_text(size = 30))
ggsave("tau_sp(tau_se).png", device = "png", height = 5/0.8, width = 6/0.8)



####
#Q: does tau show overlap because tau is related to score??
####
cor.test(info$tau_se,info$score_se , method="pearson")
cor <- cor.test(info$tau_se,info$score_se, method="spearman", exact = FALSE)
p <- round(cor$p.value, 3)
if (p < 0.001){
  ptext <- "p < .001"
}else{
  ptext <- paste0("p = ", p)
}
r <- round(cor$estimate, 2)
ggplot(info, aes(x = tau_se, y = score_se)) + geom_point() + geom_smooth(method = 'lm', color = "indianred4", fill = "indianred2") +
  theme_classic() + scale_x_log10()  + 
  labs(x = "Semantic random exploration", y = "Semantic score") + theme(text = element_text(size = 20)) + 
  geom_label(aes(x = 0.03, y = 30, label = paste0("r = ", r, ", ", ptext)))
ggsave("Score(tau_se).png", device = "png", height = 5/0.8, width = 6/0.8)

cor.test(info$tau_sp,info$score_sp , method="pearson")
cor <- cor.test(info$tau_sp,info$score_sp, method="spearman", exact = FALSE)
p <- round(cor$p.value, 3)
if (p < 0.001){
  ptext <- "p < .001"
}else{
  ptext <- paste0("p = ", p)
}
r <- round(cor$estimate, 2)
ggplot(info, aes(x = tau_sp, y = score_sp)) + geom_point() + geom_smooth(method = 'lm', color = "indianred4", fill = "indianred2") +
  theme_classic() + scale_x_log10()  + 
  labs(x = "Spatial random exploration", y = "Spatial score") + theme(text = element_text(size = 20)) + 
  geom_label(aes(x = 0.04, y = 30, label = paste0("r = ", r, ", ", ptext)))
ggsave("Score(tau_sp).png", device = "png", height = 5/0.8, width = 6/0.8)

#check: do the other model parameters correlate with score?
info$l_fit_se_sc <- scale(info$l_fit_se)
info$l_fit_sp_sc <- scale(info$l_fit_sp)
info$beta_se_sc <- scale(info$beta_se)
info$beta_sp_sc <- scale(info$beta_sp)
info$tau_se_sc <- scale(info$tau_se)
info$tau_sp_sc <- scale(info$tau_sp)

summary(glm(score_se ~ l_fit_se_sc * beta_se_sc * tau_se_sc, data = info))
summary(glm(score_sp ~ l_fit_sp_sc * beta_sp_sc * tau_sp_sc, data = info))

#for generalization
cor.test(info$l_fit_se,info$score_se , method="pearson")
cor <- cor.test(info$l_fit_se,info$score_se, method="spearman", exact = FALSE)
p <- round(cor$p.value, 3)
if (p < 0.001){
  ptext <- "p < .001"
}else{
  ptext <- paste0("p = ", p)
}
r <- round(cor$estimate, 2)
ggplot(info, aes(x = l_fit_se, y = score_se)) + geom_point() + geom_smooth(method = 'lm', color = "indianred4", fill = "indianred2") +
  theme_classic() + scale_x_log10()  + 
  labs(x = "Semantic generalization", y = "Semantic score") + theme(text = element_text(size = 20)) + 
  geom_label(aes(x = 1, y = 30, label = paste0("r = ", r, ", ", ptext)))
ggsave("Score(l_fit_se).png", device = "png", height = 5/0.8, width = 6/0.8)

cor.test(info$l_fit_sp,info$score_sp , method="pearson")
cor <- cor.test(info$l_fit_sp,info$score_sp, method="spearman", exact = FALSE)
p <- round(cor$p.value, 3)
if (p < 0.001){
  ptext <- "p < .001"
}else{
  ptext <- paste0("p = ", p)
}
r <- round(cor$estimate, 2)
ggplot(info, aes(x = l_fit_sp, y = score_sp)) + geom_point() + geom_smooth(method = 'lm', color = "indianred4", fill = "indianred2") +
  theme_classic() + scale_x_log10()  + 
  labs(x = "Spatial generalization", y = "Spatial score") + theme(text = element_text(size = 20)) + 
  geom_label(aes(x = 1, y = 30, label = paste0("r = ", r, ", ", ptext)))
ggsave("Score(l_fit_sp).png", device = "png", height = 5/0.8, width = 6/0.8)

#for UG exploration
cor.test(info$beta_se,info$score_se , method="pearson")
cor <- cor.test(info$beta_se,info$score_se, method="spearman", exact = FALSE)
p <- round(cor$p.value, 3)
if (p < 0.001){
  ptext <- "p < .001"
}else{
  ptext <- paste0("p = ", p)
}
r <- round(cor$estimate, 2)
ggplot(info, aes(x = beta_se, y = score_se)) + geom_point() + geom_smooth(method = 'lm', color = "indianred4", fill = "indianred2") +
  theme_classic() + scale_x_log10()  + 
  labs(x = "Semantic UG exploration", y = "Semantic score") + theme(text = element_text(size = 20)) + 
  geom_label(aes(x = 0.03, y = 30, label = paste0("r = ", r, ", ", ptext)))
ggsave("Score(beta_se).png", device = "png", height = 5/0.8, width = 6/0.8)

cor.test(info$beta_sp,info$score_sp , method="pearson")
cor <- cor.test(info$beta_sp,info$score_sp, method="spearman", exact = FALSE)
p <- round(cor$p.value, 3)
if (p < 0.001){
  ptext <- "p < .001"
}else{
  ptext <- paste0("p = ", p)
}
r <- round(cor$estimate, 2)
ggplot(info, aes(x = beta_sp, y = score_sp)) + geom_point() + geom_smooth(method = 'lm', color = color = "indianred4", fill = "indianred2") +
  theme_classic() + scale_x_log10()  + 
  labs(x = "Spatial UG exploration", y = "Spatial score") + theme(text = element_text(size = 20)) + 
  geom_label(aes(x = 0.04, y = 30, label = paste0("r = ", r, ", ", ptext)))
ggsave("Score(beta_sp).png", device = "png", height = 5/0.8, width = 6/0.8)


#for UG exploration when we filter out everyone at the lower bound
cor.test(info_filter$beta_se,info_filter$score_se , method="pearson")
cor <- cor.test(info_filter$beta_se,info_filter$score_se, method="spearman", exact = FALSE)
p <- round(cor$p.value, 3)
if (p < 0.001){
  ptext <- "p < .001"
}else{
  ptext <- paste0("p = ", p)
}
r <- round(cor$estimate, 2)
ggplot(info_filter, aes(x = beta_se, y = score_se)) + geom_point() + geom_smooth(method = 'lm', color = color = "indianred4", fill = "indianred2") +
  theme_classic() + scale_x_log10()  + 
  labs(x = "Semantic UG exploration", y = "Semantic score") + theme(text = element_text(size = 20)) + 
  geom_label(aes(x = 0.03, y = 40, label = paste0("r = ", r, ", ", ptext)))
ggsave("FILTEREDScore(beta_se).png", device = "png", height = 5/0.8, width = 6/0.8)

cor.test(info_filter$beta_sp,info_filter$score_sp , method="pearson")
cor <- cor.test(info_filter$beta_sp,info_filter$score_sp, method="spearman", exact = FALSE)
p <- round(cor$p.value, 3)
if (p < 0.001){
  ptext <- "p < .001"
}else{
  ptext <- paste0("p = ", p)
}
r <- round(cor$estimate, 2)
ggplot(info_filter, aes(x = beta_sp, y = score_sp)) + geom_point() + geom_smooth(method = 'lm', color = color = "indianred4", fill = "indianred2") +
  theme_classic() + scale_x_log10()  + 
  labs(x = "Spatial UG exploration", y = "Spatial score") + theme(text = element_text(size = 20)) + 
  geom_label(aes(x = 0.04, y = 40, label = paste0("r = ", r, ", ", ptext)))
ggsave("FILTEREDScore(beta_sp).png", device = "png", height = 5/0.8, width = 6/0.8)


#what can we learn about generalization and random exploration in that filtered sample?
#generalization first
cor.test(info_filter$l_fit_se,info_filter$l_fit_sp , method="pearson")
cor <- cor.test(info_filter$l_fit_se,info_filter$l_fit_sp, method="spearman", exact = FALSE)
p <- round(cor$p.value, 3)
if (p < 0.001){
  ptext <- "p < .001"
}else{
  ptext <- paste0("p = ", p)
}
r <- round(cor$estimate, 2)
ggplot(info_filter, aes(x = l_fit_se, y = l_fit_sp)) + geom_point() + geom_smooth(method = 'lm', color = color = "indianred4", fill = "indianred2") +
  theme_classic() + 
  labs(x = "Semantic generalization", y = "Spatial generalization") + theme(text = element_text(size = 20)) + 
  scale_x_log10() + scale_y_log10() + 
  geom_label(aes(x = 0.7, y = 0.1, label = paste0("r = ", r, ", ", ptext)))
ggsave("filtered_l_fit_sp(l_fit_se).png", device = "png", height = 5/0.8, width = 6/0.8)

#random exploratin then
cor.test(info_filter$tau_se,info_filter$tau_sp , method="pearson")
cor <- cor.test(info_filter$tau_se,info_filter$tau_sp, method="spearman", exact = FALSE)
p <- round(cor$p.value, 3)
if (p < 0.001){
  ptext <- "p < .001"
}else{
  ptext <- paste0("p = ", p)
}
r <- round(cor$estimate, 2)
ggplot(info_filter, aes(x = tau_se, y = tau_sp)) + geom_point() + geom_smooth(method = 'lm', color = color = "indianred4", fill = "indianred2") +
  theme_classic() + 
  labs(x = "Semantic random exploration", y = "Spatial random exploration") + theme(text = element_text(size = 20)) + 
  scale_x_log10() + scale_y_log10() + 
  geom_label(aes(x = 0.3, y = 0.03, label = paste0("r = ", r, ", ", ptext)))
ggsave("filtered_tau_sp(tau_se).png", device = "png", height = 5/0.8, width = 6/0.8)


##################################################################################################
#dive deeper, holistic analysis: which model parameters from se predict sp (and vice versa)?
#canonical correlation analysis
library(candisc)
info <- info[!is.na(info$score_se) & !is.na(info$score_sp),]
###
#A
###
info$nov_sc_se <- scale(info$Novclicks_se)
info$av_distance_se <- ifelse(info$av_distance_se > 100, 12, info$av_distance_se)
info$d_sc_se <- scale(info$av_distance_se)
info$dprev_sc_se <- scale(info$av_distance_prev_se)
info$nov_sc_sp <- scale(info$Novclicks_sp)
info$d_sc_sp <- scale(info$av_distance_sp)
info$dprev_sc_sp <- scale(info$av_distance_prev_sp)

X <-  as.matrix(info[,c("nov_sc_se", "d_sc_se", "dprev_sc_se")])
Y <- as.matrix(info[,c("nov_sc_sp", "d_sc_sp", "dprev_sc_sp")])
cc <- cancor(X, Y, set.names=c("semantic", "spatial"), na.omit=TRUE)
summary(cc)
zapsmall(cor(scores(cc, type="x"), scores(cc, type="y")))
#variance explained per canonical dimension:
cc$cancor^2
cc$structure
#here we see:
round(cc$structure$X.xscores, 3) #first canonical relationship is driven by large contributions of all 3 exploration measures on the semantic side
round(cc$structure$Y.yscores, 3) #and large contributions of novel clic and distance from hv clicks on the spatial side, with moderate distr of distance previous
#the canonical correlation of the first variate is .33

#A canonical correlation analysis revealed one meaningful canonical function linking semantic and spatial parameters, 
#rc=.33. The first semantic canonical variate was associated with all exploration paraameters 

###
#B
###
#let's add all beh measures in one!
info$score_sc_se <- scale(info$score_se)
info$slope_score_sc_se <- scale(info$slope_score_se)
info$slope_nov_sc_se <- scale(info$slope_novel_se)
info$slope_d_sc_se <- scale(info$slope_dhv_se)
info$slope_dprev_sc_se <- scale(info$slope_dprev_se)
info$slope_dprew_sc_se <- scale(info$slope_dprew_se)
info$score_sc_sp <- scale(info$score_sp)
info$slope_score_sc_sp <- scale(info$slope_score_sp)
info$slope_nov_sc_sp <- scale(info$slope_novel_sp)
info$slope_d_sc_sp <- scale(info$slope_dhv_sp)
info$slope_dprev_sc_sp <- scale(info$slope_dprev_sp)
info$slope_dprew_sc_sp <- scale(info$slope_dprew_sp)

X <-  as.matrix(info[,c("nov_sc_se", "d_sc_se", "dprev_sc_se", "score_sc_se", "slope_score_sc_se", "slope_nov_sc_se", "slope_d_sc_se", "slope_dprev_sc_se", "slope_dprew_sc_se")])
Y <-  as.matrix(info[,c("nov_sc_sp", "d_sc_sp", "dprev_sc_sp", 
                        "score_sc_sp", "slope_score_sc_sp", 
                        "slope_nov_sc_sp", "slope_d_sc_sp", "slope_dprev_sc_sp", 
                        "slope_dprew_sc_sp")])
cc <- cancor(X, Y, set.names=c("semantic", "spatial"), na.omit=TRUE)
summary(cc)
zapsmall(cor(scores(cc, type="x"), scores(cc, type="y")))
#variance explained per canonical dimension:
cc$cancor^2
cc$structure
#here we see:
round(cc$structure$X.xscores, 3) 
round(cc$structure$Y.yscores, 3) 


###
#C
###
#computational measures
info["logl_se"] <- scale(log(info$l_fit_se, 10))
info["logb_se"] <- scale(log(info$beta_se, 10))
info["logt_se"] <- scale(log(info$tau_se, 10))
info["logphi_se"] <- scale(log(info$phi, 10))
info["logl_sp"] <- scale(log(info$l_fit_sp, 10))
info["logb_sp"] <- scale(log(info$beta_sp, 10))
info["logt_sp"] <- scale(log(info$tau_sp, 10))

X <-  as.matrix(info[,c("logl_se", "logb_se", "logt_se", "logphi_se")])
Y <- as.matrix(info[,c("logl_sp", "logb_sp", "logt_sp")])
cc <- cancor(X, Y, set.names=c("semantic", "spatial"))
summary(cc)
zapsmall(cor(scores(cc, type="x"), scores(cc, type="y")))
#variance explained per canonical dimension:
cc$cancor^2
cc$structure
#here we see:
round(cc$structure$X.xscores, 3) #first canonical relationship is driven by tau_se on the semantic side, moderate contribution of l_fit_se
round(cc$structure$Y.yscores, 3) #and tau_sp on the spatial side, moderate contribution of beta_sp
# with weaker contr from the other variables
#the canonical correlation of the first variate is .420
#individuals who have high tau_se also tend to have high tau_sp (latent composites correlating at approx .42, shared variance .177)

#A canonical correlation analysis revealed one meaningful canonical function linking semantic and spatial parameters, 
#rc=.42. The first semantic canonical variate was primarily associated with tau_se (loading = .97), 
#while the corresponding spatial canonical variate was primarily associated with tau_sp (loading = .92). 
#This suggests that individual differences in semantic and spatial tau parameters are positively related.

###
#D
###
#ALL MEASURES!
X <-  as.matrix(info[,c("nov_sc_se", "d_sc_se", "dprev_sc_se", 
                        "score_sc_se", "slope_score_sc_se", 
                        "slope_nov_sc_se", "slope_d_sc_se", "slope_dprev_sc_se", 
                        "slope_dprew_sc_se",
                        "logl_se", "logb_se", "logt_se", "logphi_se")])
Y <-  as.matrix(info[,c("nov_sc_sp", "d_sc_sp", "dprev_sc_sp", 
                        "score_sc_sp", "slope_score_sc_sp", 
                        "slope_nov_sc_sp", "slope_d_sc_sp", "slope_dprev_sc_sp", 
                        "slope_dprew_sc_sp",
                        "logl_sp", "logb_sp", "logt_sp")])
cc <- cancor(X, Y, set.names=c("semantic", "spatial"), na.omit=TRUE)
summary(cc)
zapsmall(cor(scores(cc, type="x"), scores(cc, type="y")))
#variance explained per canonical dimension:
cc$cancor^2
cc$structure
#here we see:
round(cc$structure$X.xscores, 3) 
round(cc$structure$Y.yscores, 3) 

#####
#correlation plot
#####
library(corrplot)
library(Hmisc)
info_filter <- info[c("nov_sc_se", "d_sc_se", "dprev_sc_se", 
                      "score_sc_se", "slope_score_sc_se", 
                      "slope_nov_sc_se", "slope_d_sc_se", "slope_dprev_sc_se", 
                      "slope_dprew_sc_se",
                      "logl_se", "logb_se", "logt_se", "logphi_se",
                      "nov_sc_sp", "d_sc_sp", "dprev_sc_sp", 
                      "score_sc_sp", "slope_score_sc_sp", 
                      "slope_nov_sc_sp", "slope_d_sc_sp", "slope_dprev_sc_sp", 
                      "slope_dprew_sc_sp",
                      "logl_sp", "logb_sp", "logt_sp")]
info_filter <- na.omit(info_filter)
c <- cor(info_filter, method = c("spearman"))
res <- rcorr(as.matrix(info_filter), type = c("spearman"))
corrplot(res$r, type="upper", p.mat = res$P, sig.level = 0.05, insig = "blank")
round(rcorr(as.matrix(info_filter))$P, 3)

#####
#Factor analysis
#####
library(factoextra)
library(tidyverse)
PCA <- prcomp(info_filter, scale = TRUE)
fviz_eig(PCA)

fviz_pca_var(PCA,
             col.var = "contrib", # Color by contributions to the PC
             gradient.cols = c("#00AFBB", "#E7B800", "#FC4E07"),
             repel = TRUE     # Avoid text overlapping
)
rot <- as.data.frame(PCA$rotation)
loadings_long <- rot[,c("PC1", "PC2", "PC3", "PC4", "PC5")] %>%
  rownames_to_column("variable") %>%
  pivot_longer(
    cols = starts_with("PC"),
    names_to = "component",
    values_to = "loading"
  )
variable_order <- colnames(info_filter)

loadings_long$variable <- factor(
  loadings_long$variable,
  levels = variable_order
)
#visualize
ggplot(loadings_long, 
       aes(x = loading, 
           y = variable,
           fill = component)) +
  
  geom_col(width = 0.7) +
  
  geom_vline(xintercept = 0, 
             linetype = "dashed") +
  
  facet_wrap(~component, nrow = 1) +
  
  theme_minimal() +
  
  theme(
    legend.position = "none",
    strip.text = element_text(size = 12)
  ) + 
  scale_y_discrete(limits = rev(variable_order))


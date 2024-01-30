# Import Libraries
library(readxl) #to read excel files
library(survey)
library(MASS)

###############################################################################
# Simple Random Sampling -  ENGLISH

# Select 45 sample
set.seed(1111)
SRS = sample(1:590,45,replace=FALSE)
print(SRS)

# Read in data
srs_data = read_excel("part_B_SRS.xlsx")

# Calculations
srs_design <- svydesign(ids = ~page, fps=1, data = srs_data)
#total words in dictionary?
srs_dict_mean = svymean(~x,srs_design) #for one page
srs_dict_total = srs_dict_mean * 590 #590 pages of words in dictionary
srs_dict_total
#total words i know?
srs_know_mean = svymean(~y,srs_design)
srs_know_total = srs_know_mean * 590
srs_know_total
#What percentage of words do i know? 
srs_percent = (srs_know_total / srs_dict_total) * 100
srs_percent
confint(srs_percent) #CI
#############################################
# Simple Random Sampling -  SPANISH
# Select 45 sample
set.seed(01)
sp_SRS = sample(1:111,45,replace=FALSE)
print(sp_SRS)

# Read in data
sp_srs_data = read_excel("spanish_SRS.xlsx")

# Calculations
sp_srs_design <- svydesign(ids = ~page, fps=1, data = sp_srs_data)
#total words in dictionary?
sp_srs_dict_mean = svymean(~x,sp_srs_design) #for one page
sp_srs_dict_total = sp_srs_dict_mean * 111 #111 pages of words in dictionary
sp_srs_dict_total
#total words i know?
sp_srs_know_mean = svymean(~y,sp_srs_design)
sp_srs_know_total = sp_srs_know_mean * 111
sp_srs_know_total
#What percentage of words do i know? 
sp_srs_percent = (sp_srs_know_total / sp_srs_dict_total) * 100
sp_srs_percent
confint(sp_srs_percent) #CI
##############################################################################
# Stratified Random Sampling - ENGLISH

# Take samples from stratum
set.seed(111)
Aa = sample(1:34,3,replace=FALSE)
set.seed(222)
Bb = sample(35:67,3,replace=FALSE)
set.seed(333)
Cc = sample(68:123,4,replace=FALSE)
set.seed(444)
Dd = sample(124:155,2,replace=FALSE)
set.seed(555)
Ee = sample(156:176,2,replace=FALSE) 
set.seed(666)
Ff = sample(177:204,2,replace=FALSE)
set.seed(777)
Gg = sample(205:222,1,replace=FALSE)
set.seed(888)
Hh = sample(223:243,2,replace=FALSE)
set.seed(999)
Ii = sample(244:267,2,replace=FALSE)
set.seed(100)
Jj = sample(268:273,1,replace=FALSE) 
# K -> 0 samples
set.seed(120)
Ll = sample(278:295,1,replace=FALSE)
set.seed(130)
Mm = sample(296:330,3,replace=FALSE)
set.seed(140)
Nn = sample(331:344,1,replace=FALSE)
set.seed(150)
Oo = sample(345:358,1,replace=FALSE) 
set.seed(160)
Pp = sample(359:407,4,replace=FALSE)
# Q -> 0 samples
set.seed(180)
Rr = sample(411:441,2,replace=FALSE)
set.seed(190)
Ss = sample(442:503,5,replace=FALSE)
set.seed(200)
Tt = sample(504:536,2,replace=FALSE) 
set.seed(210)
Uu = sample(537:550,1,replace=FALSE)
set.seed(220)
Vv = sample(551:563,1,replace=FALSE) 
set.seed(230)
Ww = sample(564:585,2,replace=FALSE)
# X -> 0 samples
# Y -> 0 samples
# Z -> 0 samples

# Read in data
strat_data = read_excel("part_B_Strat.xlsx")

# Calculations
strat_design <- svydesign(id=~1,strata=~letter,fpc=~fpc,data=strat_data)
#total words in dictionary?
st_dict_mean = svymean(~x,strat_design) #for one page
st_dict_total =  st_dict_mean * 590 #590 pages of words in dictionary
st_dict_total
#total words i know?
st_know_mean = svymean(~y,strat_design)
st_know_total = st_know_mean * 590
st_know_total
#What percentage of words do i know? 
st_percent = (st_know_total / st_dict_total) * 100
st_percent
confint(st_percent) #CI
###########################################
# Stratified Random Sampling - SPANISH

# Take samples from stratum 
set.seed(9999)
a = sample(3:15,5,replace=FALSE)
set.seed(998)
b = sample(16:18,1,replace=FALSE)
set.seed(997)
c = sample(19:30,5,replace=FALSE)
set.seed(996)
d = sample(31:35,2,replace=FALSE)
set.seed(995)
e = sample(36:41,2,replace=FALSE) 
set.seed(994)
f = sample(42:44,1,replace=FALSE)
set.seed(993)
g = sample(45:47,1,replace=FALSE)
set.seed(992)
h = sample(48:49,1,replace=FALSE)
set.seed(991)
i = sample(50:53,2,replace=FALSE)
set.seed(989)
j = sample(54:54,1,replace=FALSE) 
set.seed(988)
l = sample(55:57,1,replace=FALSE)
set.seed(987)
m = sample(58:64,3,replace=FALSE)
set.seed(986)
n = sample(65:66,1,replace=FALSE)
set.seed(985)
o = sample(67:69,1,replace=FALSE) 
set.seed(984)
p = sample(70:79,4,replace=FALSE)
set.seed(983)
r = sample(80:88,4,replace=FALSE)
set.seed(982)
s = sample(89:97,4,replace=FALSE)
set.seed(981)
t = sample(98:104,3,replace=FALSE) 
u = 105
set.seed(979)
v = sample(105:109,2,replace=FALSE) 

# Read in data
sp_strat_data = read_excel("spanish_Strat.xlsx")

# Calculations
sp_strat_design <- svydesign(id=~1,strata=~letter,fpc=~fpc,data=sp_strat_data)
#total words in dictionary?
sp_st_dict_mean = svymean(~x,sp_strat_design) #for one page
sp_st_dict_total =  sp_st_dict_mean * 111 #111 pages of words in dictionary
sp_st_dict_total
#total words i know?
sp_st_know_mean = svymean(~y,sp_strat_design)
sp_st_know_total = sp_st_know_mean * 111
sp_st_know_total
#What percentage of words do i know? 
sp_st_percent = (sp_st_know_total / sp_st_dict_total) * 100
sp_st_percent
confint(sp_st_percent) #CI
##############################################################################
# Cluster Sampling - ENGLISH (cluster by letter of the alphabet)

#desired sample = 45
#average number of pages per cluster = 23
#clusters to sample = 45/23 = 2

# Selecting Clusters
clusters <- unique(clus_data$letter)
selected_clusters <- sample(clusters, 2) #J and B

# Read in data
clus_data = read_excel("part_B_Clus.xlsx")

# Calculations
clus_design <- svydesign(id=~1,data=clus_data,fpc=~fpc)
#total words in dictionary?
clus_dict_mean = svymean(~x,clus_design) #for one page
clus_dict_total =  clus_dict_mean * 590 #590 pages of words in dictionary
clus_dict_total
#total words i know?
clus_know_mean = svymean(~y,clus_design)
clus_know_total = clus_know_mean * 590
clus_know_total
#What percentage of words do i know? 
clus_percent = (clus_know_total / clus_dict_total) * 100
clus_percent
confint(clus_percent) #CI
##############################################################################
# Cluster Sampling - Spanish (cluster by letter of the alphabet)

#desired sample = 45
#average number of pages per cluster = 4
#clusters to sample = 45/4 = 11

# Selecting Clusters
clusters = list('A','B','C','D','E','F','G','H','I','J','K','L','M','N','O','P','Q','R','S','T','U','V','W','X','Y','Z')
set.seed(101101)
selected_clusters <- sample(clusters, 11) #A,C,D,E,J,L,N,O,P,S,U

# Read in data
sp_clus_data = read_excel("spanish_Clus.xlsx")

# Calculations
sp_clus_design <- svydesign(id=~1,data=sp_clus_data,fpc=~fpc)
#total words in dictionary?
sp_clus_dict_mean = svymean(~x,sp_clus_design) #for one page
sp_clus_dict_total =  sp_clus_dict_mean * 590 #590 pages of words in dictionary
sp_clus_dict_total
#total words i know?
sp_clus_know_mean = svymean(~y,sp_clus_design)
sp_clus_know_total = sp_clus_know_mean * 590
sp_clus_know_total
#What percentage of words do i know? 
sp_clus_percent = (sp_clus_know_total / sp_clus_dict_total) * 100
sp_clus_percent
confint(sp_clus_percent) #CI
##############################################################################
# Systematic Sampling - ENGLISH

# calculate k (k=N/n) and take sample
k = 560 / 45
random_starting_point <- sample(1:k, 1)
systematic_sample <- seq(from = random_starting_point, to = 590, by = k)
#round pages to nearest whole number
systematic_sample = round(systematic_sample)

# Read in data
sys_data = read_excel("part_B_Sys.xlsx")

# Calculations
sys_design <- svydesign(id=~1, data=sys_data, weights=~weight)
#total words in dictionary?
sys_dict_mean = svymean(~x,sys_design) #for one page
sys_dict_total =  sys_dict_mean * 590 #590 pages of words in dictionary
sys_dict_total
#total words i know?
sys_know_mean = svymean(~y,sys_design)
sys_know_total = sys_know_mean * 590
sys_know_total
#What percentage of words do i know? 
sys_percent = (sys_know_total / sys_dict_total) * 100
sys_percent
confint(sys_percent) #CI
##########################################
# Systematic Sampling - SPANISH

# calculate k and take sample
k = 111 / 45
k = round(k)
random_starting_point <- sample(1:k, 1)
systematic_sample <- seq(from = random_starting_point, to = 111, by = k)

# Read in data
sp_sys_data = read_excel("Spanish_Sys.xlsx")

# Calculations
sp_sys_design <- svydesign(id=~1, data=sp_sys_data, weights=~weight)
#total words in dictionary?
sp_sys_dict_mean = svymean(~x,sp_sys_design) #for one page
sp_sys_dict_total =  sp_sys_dict_mean * 111 #111 pages of words in dictionary
sp_sys_dict_total
#total words i know?
sp_sys_know_mean = svymean(~y,sp_sys_design)
sp_sys_know_total = sp_sys_know_mean * 111
sp_sys_know_total
#What percentage of words do i know? 
sp_sys_percent = (sp_sys_know_total / sp_sys_dict_total) * 100
sp_sys_percent
confint(sp_sys_percent) #CI
##############################################################################
# ENGLISH RESULTS GRAPH
library(ggplot2)

#create a dataframe to hold English results
english_results <- data.frame(
  method = c("SRS", "Stratified", "Cluster","Systematic"),
  percentage = c(82.108, 83.242, 79.896, 81.343),
  se_percentage = c(53.728, 0.194, 0.2909, 1.1354)
)

#plotting
ggplot(english_results, aes(x = method, y = percentage, fill = method)) +
  geom_bar(stat = "identity", position = position_dodge(width = 0.8), color = "black", width = 0.7) +
  geom_errorbar(aes(ymin = percentage - se_percentage, ymax = percentage + se_percentage),
                position = position_dodge(width = 0.9), width = 0.25) +
  geom_text(aes(label = sprintf("%.1f", percentage)), vjust = -0.5, position = position_dodge(width = 0.8))+
  labs(title = "Percentage of Words Known by Sampling Method - English",
       x = "Sampling Method",
       y = "Percentage",
       fill = "Sampling Method") +
  ylim(0, 100) +  # Limit percentage to display up to 100% only
  theme_minimal()

##############################################################################
# SPANISH RESULTS GRAPH

#create a dataframe to hold spanish results
english_results <- data.frame(
  method = c("SRS", "Stratified", "Cluster","Systematic"),
  percentage = c(82.015, 83.971, 84.403, 83.793),
  se_percentage = c(1.2619, 0.3636, 0.5046, 1.5465)
)

#plotting
ggplot(english_results, aes(x = method, y = percentage, fill = method)) +
  geom_bar(stat = "identity", position = position_dodge(width = 0.8), color = "black", width = 0.7) +
  geom_errorbar(aes(ymin = percentage - se_percentage, ymax = percentage + se_percentage),
                position = position_dodge(width = 0.9), width = 0.25) +
  geom_text(aes(label = sprintf("%.1f", percentage)), vjust = -0.5, position = position_dodge(width = 0.8))+
  labs(title = "Percentage of Words Known by Sampling Method - Spanish",
       x = "Sampling Method",
       y = "Percentage",
       fill = "Sampling Method") +
  ylim(0, 100) +  # Limit percentage to display up to 100% only
  theme_minimal()

##############################################################################
# Difference between English and Spanish knowledge

#SRS
srs_diff = srs_percent - sp_srs_percent
srs_diff
confint(srs_diff)
#Stratified
st_diff = st_percent - sp_st_percent
st_diff
confint(st_diff)

#Cluster
clus_diff = clus_percent - sp_clus_percent
clus_diff
confint(clus_diff)

#Systematic
sys_diff = sys_percent - sp_sys_percent
sys_diff
confint(sys_diff)


# Observed movements from Lisboa

scaled_matrix <- readRDS("~/mobility_africa/mobility_africa_models/draft/mobility_figures/20211123-155031-ccebac33/scaled_matrix.rds")

lisb_mov <- scaled_matrix$portugal["LISBOA",]
summary(lisb_mov)
# Min.  1st Qu.   Median     Mean  3rd Qu.     Max. 
# 0.0     55.8    131.5   2101.0    334.8 343845.5

sum(lisb_mov > 0)
# movement to 274 out of 278 units

sort(lisb_mov)
# the four patches where no movement from Lisbon occurs
# BOTICAS                   MONCHIQUE          OLIVEIRA_DE_FRADES 
# RIBEIRA_DE_PENA


mdd_mov <- scaled_matrix$portugal["MIRANDA_DO_DOURO",]
summary(mdd_mov)
# Min.  1st Qu.   Median     Mean  3rd Qu.     Max. 
# 0.000    0.000    0.000   27.899    5.552 3086.855 

sum(mdd_mov > 0)
# movement to 88 out of 278 units

sum(mdd_mov)
# [1] 7756
mdd_mov["MIRANDA_DO_DOURO"]
# MIRANDA_DO_DOURO 
# 3086.855

1 - (mdd_mov["MIRANDA_DO_DOURO"] / sum(mdd_mov))
# 0.6020043 = proportion of people moving out of MdD
# this gets reduced to ~0.34
# and spread from



prs_mov <- scaled_matrix$france["PARIS",]
summary(prs_mov)
# Min.   1st Qu.    Median      Mean   3rd Qu.      Max. 
# 0.0     144.3     320.6    6727.5     939.7 1448150.8 

sum(prs_mov > 0)
# movement to 323 out of 324 units

sort(prs_mov)


bre_mov <- scaled_matrix$france["BREST",]
summary(bre_mov)
# Min.   1st Qu.    Median      Mean   3rd Qu.      Max. 
# 0.0     144.3     320.6    6727.5     939.7 1448150.8 

sum(bre_mov > 0)
# movement to 239 out of 324 units

sort(bre_mov)


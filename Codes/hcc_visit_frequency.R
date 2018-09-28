# load library
library(ggplot2)

# load data
hcc_igg_igm <- read.csv("hcc_igg_igm.csv", head=T)

# convert interview date to date class 
hcc_igg_igm$int_date <- as.Date(hcc_igg_igm$int_date, "%Y-%m-%d")

# subset data for kids who were dengue positive upon first visit
denv_pos_init <- subset(hcc_igg_igm, redcap_event_name == "visit_a_arm_1" & result_igg_denv_stfd == 1)

# subset data for kids who were dengue negative on first visit
hcc_neg_init <- subset(hcc_igg_igm, redcap_event_name == "visit_a_arm_1" & result_igg_denv_stfd == 0)

# plot visit frequency
ggplot() + geom_point(data = hcc_neg_init, aes(x = int_date, y = person_id)) +
  theme(axis.text.x = element_text(angle = 90, hjust = 1)) + labs("HCC visit frequency") +
  xlab("") + ylab("")

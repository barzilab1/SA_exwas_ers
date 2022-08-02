
source("config.R")
source("utility_fun.R")

ksad_y = load_instrument("abcd_ksad501",abcd_files_path)

#555 and 888 will be NA
ksad_y[ksad_y == "888" | ksad_y == "555"] = NA



##calculate suicidality
#if one of the items is 1, the result will be 1
#if one of the items is NA and the rest is 0, the result will be NA

#attempt
ksad_y$SA_current_y <- apply(ksad_y[,which(grepl("ksads_23_(952|953|954)", colnames(ksad_y)))],1 ,function(x) {any(x == 1)*1})
ksad_y$SA_past_y <- apply(ksad_y[,which(grepl("ksads_23_(963|964|965)", colnames(ksad_y)))],1 ,function(x) {any(x == 1)*1})
ksad_y$SA_y <-(ksad_y$SA_current_y == 1 | ksad_y$SA_past_y == 1)*1

#ideation: current or past = 1 & SA_y != 1 --> 1
ksad_y$SI_current_y <- apply(ksad_y[,which(grepl("ksads_23_(946|947|948|949|950|951)", colnames(ksad_y)))],1 ,function(x) {any(x == 1)*1})
ksad_y$SI_past_y <- apply(ksad_y[,which(grepl("ksads_23_(957|958|959|960|961|962)", colnames(ksad_y)))],1 ,function(x) {any(x == 1)*1})
ksad_y$SI_y <- (ksad_y$SI_current_y == 1 | ksad_y$SI_past_y == 1)*1
## Those with both SA_y == 1 & SI_y == 1 will get SI_y == 0
ksad_y$SI_y[ksad_y$SA_y == 1] <- 0

#combine ideation and attempt
ksad_y$suicidality_current_y <- (ksad_y$SI_current_y == 1 | ksad_y$SA_current_y == 1)*1
ksad_y$suicidality_past_y <- (ksad_y$SI_past_y == 1 | ksad_y$SA_past_y == 1)*1
ksad_y$suicidality_y <- (ksad_y$SI_y == 1 | ksad_y$SA_y == 1)*1

#nssi current or past = 1 SA_y != 1 & SI_y != 1 --> 1
ksad_y$nssi_current_y <- apply(ksad_y[,which(grepl("ksads_23_(945|955)", colnames(ksad_y)))], 1, function(x) {any(x == 1)*1})
ksad_y$nssi_past_y <- apply(ksad_y[,which(grepl("ksads_23_(956|966)", colnames(ksad_y)))], 1, function(x) {any(x == 1)*1})
ksad_y$nssi_y <- (ksad_y$nssi_current_y == 1 | ksad_y$nssi_past_y == 1)*1
## Those with nssi_y == 1 but SA_y == 1 will get 0
ksad_y$nssi_y[ksad_y$SA_y == 1] <- 0

#control: SA, SI, NSSI = 0
ksad_y$sui_control <- apply(ksad_y[,which(grepl("SA_y|SI_y|nssi_y", colnames(ksad_y)))], 1, function(x) {all(x == 0)*1})

# Long format
suicide_set <- ksad_y[,grepl("src|inter|event|sex|SI|SA|sui|nssi", colnames(ksad_y))]

#  new variable to use in reshape from long to wide format
suicide_set$timepoint = sub("_year.*", "", suicide_set$eventname)
suicide_set[,c("sex","interview_date","interview_age","eventname")] = NULL
suicide_set_wide = reshape(suicide_set, direction = "wide", idvar = "src_subject_id", timevar = "timepoint", sep = "_")



write.csv(file = "outputs/suicide_long.csv", x = suicide_set, row.names = F, na = "")
write.csv(file = "outputs/suicide_wide.csv", x = suicide_set_wide, row.names = F, na = "")




##########################################
### for each past/current pair, create one feature
### for youth only

ksad_y$symptom_self_injurious_behavior = apply(ksad_y[,which(grepl("ksads_23_(143|144)", colnames(ksad_y)))],1 ,function(x) {any(x == 1)*1})
ksad_y$symptom_wishes_better_off_dead = apply(ksad_y[,which(grepl("ksads_23_(145|146)", colnames(ksad_y)))],1 ,function(x) {any(x == 1)*1})
ksad_y$symptom_SI = apply(ksad_y[,which(grepl("ksads_23_(147|148)", colnames(ksad_y)))],1 ,function(x) {any(x == 1)*1})
ksad_y$symptom_SA = apply(ksad_y[,which(grepl("ksads_23_(149|150)", colnames(ksad_y)))],1 ,function(x) {any(x == 1)*1})


ksad_y$diag_SelfInjuriousBehaviorwithoutsuicidalintent = apply(ksad_y[,which(grepl("ksads_23_(945|956)", colnames(ksad_y)))],1 ,function(x) {any(x == 1)*1})
ksad_y$diag_SuicidalideationPassive = apply(ksad_y[,which(grepl("ksads_23_(946|957)", colnames(ksad_y)))],1 ,function(x) {any(x == 1)*1})
ksad_y$diag_SuicidalideationActivenonspecific = apply(ksad_y[,which(grepl("ksads_23_(947|958)", colnames(ksad_y)))],1 ,function(x) {any(x == 1)*1})
ksad_y$diag_SuicidalideationActivemethod = apply(ksad_y[,which(grepl("ksads_23_(948|959)", colnames(ksad_y)))],1 ,function(x) {any(x == 1)*1})
ksad_y$diag_SuicidalideationActiveintent = apply(ksad_y[,which(grepl("ksads_23_(949|960)", colnames(ksad_y)))],1 ,function(x) {any(x == 1)*1})
ksad_y$diag_SuicidalideationActiveplan = apply(ksad_y[,which(grepl("ksads_23_(950|961)", colnames(ksad_y)))],1 ,function(x) {any(x == 1)*1})
ksad_y$diag_PreparatoryActionstowardimminentSuicidalbehavior = apply(ksad_y[,which(grepl("ksads_23_(951|962)", colnames(ksad_y)))],1 ,function(x) {any(x == 1)*1})
ksad_y$diag_InterruptedAttempt = apply(ksad_y[,which(grepl("ksads_23_(952|963)", colnames(ksad_y)))],1 ,function(x) {any(x == 1)*1})
ksad_y$diag_AbortedAttempt = apply(ksad_y[,which(grepl("ksads_23_(953|964)", colnames(ksad_y)))],1 ,function(x) {any(x == 1)*1})
ksad_y$diag_SuicideAttempt = apply(ksad_y[,which(grepl("ksads_23_(954|965)", colnames(ksad_y)))],1 ,function(x) {any(x == 1)*1})
ksad_y$diag_Nosuicidalideationorbehavior = apply(ksad_y[,which(grepl("ksads_23_(955|966)", colnames(ksad_y)))],1 ,function(x) {any(x == 1)*1})



##########################################




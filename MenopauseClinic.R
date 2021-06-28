# BMS Survey Analyses
# Author: Hugo Pedder
# Date: 2021-04-29

library(readxl)
library(tidyverse)
library(reshape2)
library(ggplot2)
library(RColorBrewer)

# Load patient data

pat.df <- read_excel("BMS Patient Survey (Responses).xlsx")

names(pat.df) <- c("time", "clintype", "clinic", "apptype2", "lastappt", "pract", "delay",
                   "apptype", "disctime_yn", "involve_yn", "convenient_yn", "language_yn", "hearing_yn",
                   "techdiff_yn", "satis", "comparef2f", "choice", "age", "white", "mixed", "asian", "black",
                   "other", "work", "deaf", "english", "sexpref", "comment")
pat.df$id <- 1:nrow(pat.df)


# Add new categories for age
pat.df$age2 <- NA
pat.df$age2[pat.df$age %in% c("25 to 34", "35 to 44")] <- "25-44"
pat.df$age2[pat.df$age %in% c("45 to 54")] <- "45-54"
pat.df$age2[pat.df$age %in% c("55 to 64")] <- "55-64"
pat.df$age2[pat.df$age %in% c("65 to 74", "75 to 84")] <- "65-84"



# Drop rows with no answers
pat.df <- pat.df[!apply(pat.df, MARGIN=1, FUN=function(x) {all(is.na(x[3:length(x)]))}),]
nrow(pat.df)


# Patient demographics
table(pat.df$age)

table(pat.df$white) # 144 or 155
table(pat.df$mixed) # 2 or 5
table(pat.df$asian) # 11 or 16
table(pat.df$black) # 8
table(pat.df$other) # 9

table(pat.df$deaf)



# Clinic details
table(pat.df$clintype)
table(pat.df$clinic)
table(pat.df$apptype)
table(pat.df$lastappt)
table(pat.df$pract)


# Appt satisfaction
table(pat.df$disctime_yn)
table(pat.df$involve_yn)
table(pat.df$convenient_yn)
table(pat.df$language_yn)
table(pat.df$hearing_yn)
table(pat.df$techdiff_yn)




getbars <- function(pat.df) {

  bin.df <- pat.df %>% select(id, disctime_yn, involve_yn, convenient_yn, language_yn, hearing_yn, techdiff_yn)


  sum.df <- data.frame(question=names(bin.df)[2:ncol(bin.df)],
                       yes=apply(bin.df[,2:ncol(bin.df)], MARGIN=2, FUN=function(x) {
                         y <- table(x)
                         return(y["Yes"])
                       }),
                       no=apply(bin.df[,2:ncol(bin.df)], MARGIN=2, FUN=function(x) {
                         y <- table(x)
                         return(y["No"])
                       })
  )

  sum.df$n <- sum.df$yes+sum.df$no
  sum.df$pyes <- sum.df$yes / sum.df$n
  sum.df$pno <- sum.df$no / sum.df$n
  sum.df$pyes_l95

  l95 <- vector()
  u95 <- vector()
  for (i in seq_along(sum.df$yes)) {
    y <- binom.test(sum.df$yes[i], sum.df$n[i])
    l95 <- append(l95, y$conf.int[1])
    u95 <- append(u95, y$conf.int[2])
  }
  sum.df$l95 <- l95
  sum.df$u95 <- u95

  sum.df <- sum.df %>% select(question, pyes, pno, l95, u95)
  sum.df <- melt(sum.df, id.vars = c("question", "l95", "u95"))
  sum.df$variable <- factor(sum.df$variable, labels=c("No", "Yes"), levels = c("pno", "pyes"))

  return(sum.df)
}


plotbars <- function(pat.df, var="apptype", cats=NULL) {

  sum.df <- getbars(subset(pat.df, pat.df[[var]]==cats[1]))
  sum.df$apptype <- cats[1]
  for (i in 2:length(cats)) {
    sum.df2 <- getbars(subset(pat.df, pat.df[[var]]==cats[i]))
    sum.df2$apptype <- cats[i]
    sum.df <- rbind(sum.df, sum.df2)
  }

  cols <- brewer.pal(n=3, "Set1")[1:2]

  sum.df$Response <- sum.df$variable
  sum.df$question <- factor(sum.df$question, labels=c(
    "Convenient?",
    "Time for discussion?",
    "Hearing difficulties?",
    "Involved in decisions?",
    "Language problems?",
    "Technical difficulties?"
  ))

  ggplot(data=sum.df, aes(fill=Response, y=value, x=question)) +
    geom_bar(position="stack", stat="identity") +
    geom_errorbar(aes(x=question, ymin=l95, ymax=u95), width=0.2) +
    facet_wrap(~apptype) +
    ylab("Proportion of responders") +
    theme_bw() +
    theme(axis.title.x=element_blank(),
          axis.text.x = element_text(angle = 90, vjust = 0.5)) +
    scale_fill_manual(values=cols, name="Response")
}




# Overall

cols <- brewer.pal(n=3, "Set1")[1:2]

sum.df <- getbars(pat.df)
sum.df$Response <- sum.df$variable
sum.df$question <- factor(sum.df$question, labels=c(
  "Was the appointment\nconvenient",
  "Did you have enough\ntime for discussion?",
  "Did you have any\ndifficulties hearing?",
  "Did you feel involvedas much\nas you wanted in decisions?",
  "Did you have any\nlanguage problems?",
  "Did you experience any\ntechnical difficulties?"
))

ggplot(data=sum.df, aes(fill=Response, y=value, x=question)) +
  geom_bar(position="stack", stat="identity") +
  geom_errorbar(aes(x=question, ymin=l95, ymax=u95), width=0.2) +
  ylab("Proportion of responders") +
  theme_bw() +
  theme(axis.title.x=element_blank(),
        axis.text.x = element_text(angle = 30, vjust = 0.5)) +
  scale_fill_manual(values=cols, name="Response")





# By clinic type
plotbars(pat.df, var="clintype", cats=c("GP",
                                           "NHS hospital clinic",
                                           "Private clinic"))


# By appointment type
plotbars(pat.df, var="apptype", cats=c("Over the telephone",
                                          "Via a video-call",
                                          "Face-to-face in a clinic"))


# Before/after lockdown
plotbars(pat.df, var="lastappt", cats=c("Before the UK Covd-19 lockdown started (23rd March 2020)",
                                           "After the UK Covd-19 lockdown started (23rd March 2020)"))







########### Satisfaction #############

satplot <- function(pat.df) {
  sum.df <- data.frame(Satisfaction=names(table(pat.df$satis)), value=table(pat.df$satis))
  sum.df$value <- sum.df$value.Freq/sum(sum.df$value.Freq)

  l95 <- vector()
  u95 <- vector()
  for (i in seq_along(sum.df$Satisfaction)) {
    r <- sum.df$value.Freq[i]
    n <- sum(sum.df$value.Freq)
    bin <- binom.test(r,n)
    l95 <- append(l95, bin$conf.int[1])
    u95 <- append(u95, bin$conf.int[2])
  }

  sum.df$l95 <- l95
  sum.df$u95 <- u95

  return(sum.df)
}




satplot.cat <- function(pat.df, var="apptype", cats=NULL) {

  sum.df <- satplot(subset(pat.df, pat.df[[var]]==cats[1]))
  sum.df$apptype <- cats[1]
  for (i in 2:length(cats)) {
    sum.df2 <- satplot(subset(pat.df, pat.df[[var]]==cats[i]))
    sum.df2$apptype <- cats[i]
    sum.df <- rbind(sum.df, sum.df2)
  }

  sum.df$Satisfaction <- factor(sum.df$Satisfaction, levels=c("Very poor", "Fairly poor",
                                                              "Neither good nor poor",
                                                              "Fairly good", "Very good"))

  cols <- brewer.pal(11, "RdYlGn")
  cols <- brewer.pal(5, "RdYlGn")

  ggplot(data=sum.df, aes(x=Satisfaction, y=value, fill=Satisfaction)) +
    geom_bar(stat="identity") +
    geom_errorbar(aes(ymin=l95, ymax=u95), width=0.2) +
    facet_wrap(~apptype) +
    ylab("Proportion of responders") +
    scale_fill_manual(values=cols, name="Satisfaction") +
    theme_bw() +
    theme(axis.title.x=element_blank(),
          axis.text.x=element_blank(),
          axis.ticks.x=element_blank())
}


# Overall

sum.df <- satplot(pat.df)
sum.df$Satisfaction <- factor(sum.df$Satisfaction, levels=c("Very poor", "Fairly poor",
                                                            "Neither good nor poor",
                                                            "Fairly good", "Very good"))

cols <- brewer.pal(5, "RdYlGn")
ggplot(data=sum.df, aes(x=Satisfaction, y=value, fill=Satisfaction)) +
  geom_bar(stat="identity") +
  geom_errorbar(aes(ymin=l95, ymax=u95), width=0.2) +
  ylab("Proportion of responders") +
  scale_fill_manual(values=cols, name="Satisfaction") +
  theme_bw() +
  theme(axis.title.x=element_blank(),
        axis.text.x=element_blank(),
        axis.ticks.x=element_blank())





# By clinic type
satplot.cat(pat.df, var="clintype", cats=c("GP",
                                          "NHS hospital clinic",
                                          "Private clinic"))


# By appointment type
satplot.cat(pat.df, var="apptype", cats=c("Over the telephone",
                                          "Via a video-call",
                                          "Face-to-face in a clinic"))


# Before/after lockdown
satplot.cat(pat.df, var="lastappt", cats=c("Before the UK Covd-19 lockdown started (23rd March 2020)",
                           "After the UK Covd-19 lockdown started (23rd March 2020)"))



# Women by age
satplot.cat(pat.df, var="age2", cats=names(table(pat.df$age2)))









####################### Compare to f2f ##################

f2f <- function(pat.df) {
  sum.df <- data.frame(Compare=names(table(pat.df$comparef2f)), value=table(pat.df$comparef2f))
  sum.df$value <- sum.df$value.Freq/sum(sum.df$value.Freq)

  l95 <- vector()
  u95 <- vector()
  for (i in seq_along(sum.df$Compare)) {
    r <- sum.df$value.Freq[i]
    n <- sum(sum.df$value.Freq)
    bin <- binom.test(r,n)
    l95 <- append(l95, bin$conf.int[1])
    u95 <- append(u95, bin$conf.int[2])
  }

  sum.df$l95 <- l95
  sum.df$u95 <- u95

  return(sum.df)
}


f2fplot <- function(pat.df, var="apptype", cats=NULL) {

  sum.df <- f2f(subset(pat.df, pat.df[[var]]==cats[1]))
  sum.df$apptype <- cats[1]
  for (i in 2:length(cats)) {
    sum.df2 <- f2f(subset(pat.df, pat.df[[var]]==cats[i]))
    sum.df2$apptype <- cats[i]
    sum.df <- rbind(sum.df, sum.df2)
  }

  sum.df$Compare <- factor(sum.df$Compare, levels=c("This was my first menopause clinic appointment",
                                                    "Not as good as face-to-face",
                                                    "No difference",
                                                    "Better than face-to-face"),
                           labels = c("First clinic", "Not as good", "No difference", "Better"))

  if (n_distinct(sum.df$Compare)==4) {
    col1 <- brewer.pal(3, "Set1")[2]
    cols <- brewer.pal(3, "RdYlGn")
    cols <- append(col1, cols)
  } else {
    cols <- brewer.pal(3, "RdYlGn")
  }


  ggplot(data=sum.df, aes(x=Compare, y=value, fill=Compare)) +
    geom_bar(stat="identity") +
    geom_errorbar(aes(ymin=l95, ymax=u95), width=0.2) +
    facet_wrap(~apptype) +
    ylab("Proportion of responders") +
    scale_fill_manual(values=cols, name="Compare") +
    theme_bw() +
    theme(axis.title.x=element_blank(),
          axis.text.x=element_blank(),
          axis.ticks.x=element_blank())
}



# Overall

f2fplot(sub, var="clintype", cats=c("GP",
                                    "NHS hospital clinic",
                                    "Private clinic"))






# By clinic type
sub <- pat.df
sub <- subset(pat.df, comparef2f!="This was my first menopause clinic appointment")

f2fplot(sub, var="clintype", cats=c("GP",
                                       "NHS hospital clinic",
                                       "Private clinic"))









###############################################################################################
###############################################################################################

######## CLINICIAN SURVEY #########

###############################################################################################
###############################################################################################



# Load patient data

pat.df <- read_excel("BMS Clinician Survey (Responses).xlsx")

names(pat.df) <- c("time", "clintype", "role", "profs", "apptype", "moref2f", "tele.int",
                   "video.int", "flex", "reschedule", "reason", "dna", "improve",
                   "future")
pat.df$id <- 1:nrow(pat.df)


# Demographics
table(pat.df$clintype)
table(pat.df$role)
table(pat.df$profs)







############## Interaction from telephone vs virtual ###########

intdat <- function(var) {
  sum.df <- data.frame(Satisfaction=names(table(var)), value=table(var))
  sum.df$value <- sum.df$value.Freq/sum(sum.df$value.Freq)

  l95 <- vector()
  u95 <- vector()
  for (i in seq_along(sum.df$Satisfaction)) {
    r <- sum.df$value.Freq[i]
    n <- sum(sum.df$value.Freq)
    bin <- binom.test(r,n)
    l95 <- append(l95, bin$conf.int[1])
    u95 <- append(u95, bin$conf.int[2])
  }

  sum.df$l95 <- l95
  sum.df$u95 <- u95

  return(sum.df)
}


plot.df <- intdat(pat.df$tele.int)
plot.df$type <- "Telephone"
plot.df2 <- intdat(pat.df$video.int)
plot.df2$type <- "Video"
plot.df <- rbind(plot.df, plot.df2)


plot.df$Satisfaction <- factor(plot.df$Satisfaction, levels=c("Much worse than face to face",
                                                  "A bit worse than face to face",
                                                  "No difference to face to face",
                                                  "A bit better than face to face",
                                                  "Much better than face to face"),
                         labels = c("Much worse", "A bit worse", "No difference", "A bit better", "Much better"))

cols <- brewer.pal(5, "RdYlGn")


ggplot(data=plot.df, aes(x=Satisfaction, y=value, fill=Satisfaction)) +
  geom_bar(stat="identity") +
  geom_errorbar(aes(ymin=l95, ymax=u95), width=0.2) +
  facet_wrap(~type) +
  ylab("Proportion of responders") +
  scale_fill_manual(values=cols, name="Clinician-patient interaction") +
  theme_bw() +
  theme(axis.title.x=element_blank(),
        axis.text.x=element_blank(),
        axis.ticks.x=element_blank())







############### Flexibility ################

plot.df <- intdat(pat.df$flex)

plot.df$Response <- factor(plot.df$Satisfaction, levels=c("A bit less flexible than face to face",
                                                              "No difference to face to face",
                                                              "A bit more flexible than face to face",
                                                              "Much more flexible than face to face"),
                               labels = c("A bit less", "No difference", "A bit more", "Much more"))

cols <- brewer.pal(5, "RdYlGn")[-1]


g1 <- ggplot(data=plot.df, aes(x=Response, y=value, fill=Response)) +
  geom_bar(stat="identity") +
  geom_errorbar(aes(ymin=l95, ymax=u95), width=0.2) +
  ylab("Proportion of responders") +
  scale_fill_manual(values=cols, name="Flexibility") +
  theme_bw() +
  theme(axis.title.x=element_blank(),
        axis.text.x=element_blank(),
        axis.ticks.x=element_blank())





############### DNA rate ################

plot.df <- intdat(pat.df$dna)

plot.df$Response <- factor(plot.df$Satisfaction, levels=c("DNA rate has increased a bit for virtual clinics",
                                                          "DNA rate is the same",
                                                          "DNA rate has decreased a bit for virtual clinics",
                                                          "DNA rate has decreased a lot for virtual clinics"),
                           labels = c("A bit higher", "No difference", "A bit lower", "Much lower"))

cols <- brewer.pal(5, "RdYlGn")[-1]


g2 <- ggplot(data=plot.df, aes(x=Response, y=value, fill=Response)) +
  geom_bar(stat="identity") +
  geom_errorbar(aes(ymin=l95, ymax=u95), width=0.2) +
  ylab("Proportion of responders") +
  scale_fill_manual(values=cols, name="DNA rate") +
  theme_bw() +
  theme(axis.title.x=element_blank(),
        axis.text.x=element_blank(),
        axis.ticks.x=element_blank())






############### Service improved ################

plot.df <- intdat(pat.df$improve)

plot.df$Response <- factor(plot.df$Satisfaction, levels=c("Virtual consultations have slightly worsened my service",
                                                          "Virtual consultations have made no difference to my service",
                                                          "Virtual consultations have slightly improved my service",
                                                          "Virtual consultations have greatly improved my service"),
                           labels = c("A bit worse", "No difference", "A bit better", "Much better"))

cols <- brewer.pal(5, "RdYlGn")[-1]


g3 <- ggplot(data=plot.df, aes(x=Response, y=value, fill=Response)) +
  geom_bar(stat="identity") +
  geom_errorbar(aes(ymin=l95, ymax=u95), width=0.2) +
  ylab("Proportion of responders") +
  scale_fill_manual(values=cols, name="Impact of virtual consultations\non service") +
  theme_bw() +
  theme(axis.title.x=element_blank(),
        axis.text.x=element_blank(),
        axis.ticks.x=element_blank())





############### Going forward ################

plot.df <- intdat(pat.df$future)

plot.df$Response <- factor(plot.df$Satisfaction)

cols <- brewer.pal(4, "Set1")


plot.df$Response <- factor(plot.df$Satisfaction, labels=c("All face-to-face",
                                                          "All remote",
                                                          "Clinician's choice",
                                                          "Patient's choice"))

g4 <- ggplot(data=plot.df, aes(x=Response, y=value, fill=Response)) +
  geom_bar(stat="identity") +
  geom_errorbar(aes(ymin=l95, ymax=u95), width=0.2) +
  ylab("Proportion of responders") +
  scale_fill_manual(values=cols, name="How would you run the\nservice in the future?") +
  theme_bw() +
  theme(axis.title.x=element_blank(),
        axis.text.x=element_blank(),
        axis.ticks.x=element_blank())





############## Combine plots ##############

library(ggpubr)

ggarrange(g1, g2,g3,g4)

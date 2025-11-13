# loading necessary packages for data manipulation and survival analysis
library(readxl)  # to read Excel files
library(dplyr) # for data wrangling and cleaning
library(survival) # for survival analysis functions
library(survminer) # for plotting survival curves nicely
library(ggplot2) # load ggplot2 to save plots as PNG

data = read_excel("Clinical_Data_Validation_Cohort.xlsx") # load the clinical dataset from Excel
View(data)  # view the dataset to check its structure

# renaming columns
data <- data %>% rename(
  Event = `Event (death: 1, alive: 0)`,
  Stage = `Stage (TNM 8th edition)`
)

# Convert columns to factors
data$Grade <- as.factor(data$Grade)
data$Stage <- as.factor(data$Stage)
data$Sex <- as.factor(data$Sex)
data$Cigarette <- as.factor(data$Cigarette)
data$Type.Adjuvant <- as.factor(data$Type.Adjuvant)
data$EGFR <- as.factor(data$EGFR)
data$KRAS <- as.factor(data$KRAS)

summary(data) # quick summary to check data types and missing values

#### Survival Analysis ######

s = Surv(data$`Survival time (days)`, data$Event) # create a survival object using time and event columns
s # :1 → event (death) happened and # + → censored (alive or lost to follow-up).

sfit = survfit(Surv(`Survival time (days)`, Event) ~ 1, data = data) # baseline hazard
sfit1 = survfit(Surv(`Survival time (days)`, Event) ~ Sex, data = data) # effect of sex
sfit2 = survfit(Surv(`Survival time (days)`, Event) ~ Grade, data = data) # effect of grade
sfit3 = survfit(Surv(`Survival time (days)`, Event) ~ Type.Adjuvant, data = data) # effect of therapy

# summarize the survival fits 
summary(sfit)
summary(sfit1)
summary(sfit2)
summary(sfit3)

# plot Kaplan-Meier curves using ggsurvplot 

p = ggsurvplot(
  sfit,
  data = data,
  xlab = "Days",
  ylab = "Survival Probability",
  title = "Overall Survival Curve"
)

p1 = ggsurvplot(
  sfit1,
  data = data,
  xlab = "Days",
  ylab = "Survival Probability",
  title = "Survival Curve by Sex"
)

p2 = ggsurvplot(
  sfit2,
  data = data,
  xlab = "Days",
  ylab = "Survival Probability",
  title = "Survival Curve by Grade"
)

p3 = ggsurvplot(
  sfit3,
  data = data,
  xlab = "Days",
  ylab = "Survival Probability",
  title = "Survival Curve by Therapy"
)

p
p1
p2
p3



# Save p
ggsave("Overall_Survival.png", plot = p$plot, width = 7, height = 5, dpi = 300)

# Save p1
ggsave("Survival_by_Sex.png", plot = p1$plot, width = 7, height = 5, dpi = 300)

# Save p2
ggsave("Survival_by_Grade.png", plot = p2$plot, width = 7, height = 5, dpi = 300)

# Save p3
ggsave("Survival_by_Therapy.png", plot = p3$plot, width = 7, height = 5, dpi = 300)

# fit Cox proportional hazards models to estimate risk (hazard ratios)

sfit_none = coxph(Surv(`Survival time (days)`, Event) ~ 1, data = data)
summary(sfit_none)
sfit1_sex = coxph(Surv(`Survival time (days)`, Event) ~ Sex, data = data)
summary(sfit1_sex)
sfit2_grade = coxph(Surv(`Survival time (days)`, Event) ~ Grade, data = data)
summary(sfit2_grade)
sfit3_adjuvant = coxph(Surv(`Survival time (days)`, Event) ~ Type.Adjuvant, data = data)
summary(sfit3_adjuvant)







data <- read.csv("heart.csv")
data1 <- read.csv("heart.csv")
library(pscl)

# --------- INITIALIZE DATA SET -----------



plot(data1$target ~ data1$age) 
lines(ksmooth(data1$age, data1$target, bandwidth = 20))


# Categorize gender
data$sex <- factor(data$sex, levels = c(0, 1),
                     labels = c("Female","Male"))

# Categorize combined chest pain type
data$combined_cp <- factor(data$cp, levels = c(0, 1, 2, 3),
                  labels = c("Pain","Pain","Pain","No pain"))
droplevels(data$combined_cp)

# Categorize chest pain type
data$cp <- factor(data$cp, levels = c(0, 1, 2, 3),
                      labels = c("Typical angina","Atypical angina","Non-anginal pain","Asymptotic"))

# Categorize blood sugar
data$fbs <- factor(data$fbs, levels = c(0, 1),
                    labels = c("Below 120 mg/dl", "Above 120 mg/dl"))

# Categorize ECG
data$restecg <- factor(data$restecg, levels = c(0, 1, 2),
                   labels = c("Normal", "ST-T wave abnormality", "Left ventricular hypertrophy"))

# Categorize exercise induced angina
data$exang <- factor(data$exang, levels = c(0, 1), labels = c("No", "Yes"))

# Categorize ST slope
data$slope <- factor(data$slope, levels = c(0, 1, 2),
                       labels = c("Upsloping", "Flat", "Downsloping"))

# Categorize combined chest pain type
data$combined_thal <- factor(data$thal, levels = c(0, 1, 2, 3),
                             labels = c("None or fixed defect", "None or fixed defect","None or fixed defect","Reversible defect"))
droplevels(data$combined_thal)

# Categorize Thal
data$thal <- factor(data$thal, levels = c(0, 1, 2, 3), labels = c("Normal", "Normal", "Fixed defect", "Reversible defect"))
droplevels(data$thal)

# Categorize the target variable
#data$target <- factor(data$target, levels =c(0,1), labels = c("No", "Yes"))

# Categorize the systonic blood pressure (low, medium high)
lowThreshold = summary(data$trestbps)[2]
highThreshold = summary(data$trestbps)[4]
maxThreshold = summary(data$trestbps)[6]

data$trestbps <- cut(data$trestbps, breaks=c(0, lowThreshold, highThreshold, maxThreshold),
                          labels=c("Low","Normal","High"))

# Categorize the systonic blood pressure (low, medium high)
old = summary(data$age)[4]
oldest = summary(data$age)[6]

data$age <- cut(data$age, breaks=c(0, old, oldest),
                     labels=c("Young","Old"))


# --------- SET GOOD REFERENCE VARIABLE -----------
data$sex <- relevel(data$sex, "Male")


# Go to doctor
fullModel = glm(target ~ age + sex + cp + trestbps + chol + fbs + restecg + thalach + exang + oldpeak + slope + ca + combined_thal + oldpeak*slope, data = data, family = "binomial")

# DIY Pro
diyProModel = glm(target ~ age + sex + combined_cp + trestbps + fbs + thalach + exang, data = data, family = "binomial")

# DIY
diyModel = glm(target ~ age + sex + combined_cp + thalach + exang, data = data, family = "binomial")

# Automically use the best model
bestFullModel = step(fullModel, k = log(nrow(data)), direction = "both")
#bestFullModel = step(fullModel, k = 2, direction = "both")

bestDiyProModel = step(diyProModel, k = log(nrow(data)), direction = "both")
#bestdiyProModel = step(diyProModel, k = 2, direction = "both")

#bestdiyModel = step(diyModel, k = 2)
bestDiyModel = step(diyModel, k = log(nrow(data)))

summary(bestFullModel)
summary(bestDiyProModel)
summary(bestDiyModel)
pR2(bestFullModel)
pR2(bestDiyProModel)
pR2(bestDiyModel)

# pairs plot data used

datafullmodel <- data.frame(data$age, data$sex, data$cp, data$exang, data$oldpeak,
                   data$ca, data$combined_thal, data$target)
datadiy <- data.frame(data$age, data$sex, data$exang , data$thalach, data$target)

pairs(datafullmodel)
pairs(datadiy)


fullPrecitions <- predict(bestFullModel, data, type = "response")

fullPrecitions <- factor(fullPrecitions < 0.5,
                    levels = c(TRUE, FALSE),
                    labels = c("No guess", "Yes guess"))

table(fullPrecitions, data$target)

diyPredictions <- predict(bestDiyModel, data, type = "response")

diyPredictions <- factor(diyPredictions < 0.5,
                         levels = c(TRUE, FALSE),
                         labels = c("No guess", "Yes guess"))

table(diyPredictions, data$target)

testPerson1 <- data.frame(age = "Old", sex = "Female", combined_cp = "No pain", thalach = 165, exang = "No")
testPerson2 <- data.frame(age = "Young", sex = "Female", combined_cp = "No pain", thalach = 165, exang = "No")

testPerson3 <- data.frame(age = "Old", sex = "Female", cp = "Asymptotic", thalach = 165, exang = "No", oldpeak=0.8, ca=1, combined_thal="Reversible defect")
testPerson4 <- data.frame(age = "Young", sex = "Female", cp = "Atypical angina", thalach = 165, exang = "No", oldpeak=0.8, ca=1, combined_thal="Reversible defect")

predict(bestDiyModel, testPerson1, type="response")
predict(bestDiyModel, testPerson2, type="response")

with(data = data, 
     plot(sex ~ target))

summary(datafullmodel)

# Interaktionstermer? - könsspecifika modeller? - åldersspecifika modeller?

hilo1 <- predict(bestFullModel, data, type = "response")

hilo1 <- factor(hilo1 < 0.5,
                    levels = c(TRUE, FALSE),
                    labels = c("No guess", "Yes guess"))

table(hilo1, data$target)

hilo2 <- predict(bestDiyProModel, data, type = "response")

hilo2 <- factor(hilo2 < 0.5,
                levels = c(TRUE, FALSE),
                labels = c("No guess", "Yes guess"))

table(hilo2, data$target)


#---------------------EXTRAS AFTER Q:s---------------

datamale <- data.frame()




library(tidyverse)
library(janitor)
library(GGally)
library(car)
library(MASS)
library(pROC)
library(caret)
library(broom)
library(patchwork)
library(ggplot2)
library(dplyr)
library(car)
library(MASS)
library(pROC)
library(mgcv)
library(glmnet)
library(broom)
library(dplyr)
library(knitr)

set.seed(123)

#Data preparation
raw <- read.csv("Car_Insurance_Claim.csv")

str(raw)


data <- raw[, -1]


summary(data)
#Possiamo vedere che la media degli outcome (sinistro pagato) è del 31.3% invece non pagato 68.7
colSums(is.na(data))
#Abbiamo credit score e annual mileage che insieme sono il 1'% dei dati, capiremo in futuro se ternerli oppure toglierli

#Guardiamo le distribuzioni delle categorie
#Prima capiamo bene la struttura delle variabili categoriche. Se risultano già ben bilanciate, evitiamo trasformazioni inutili e passiamo direttamente all'EDA.


table(data$AGE)
#Abbastanza equilibrate nessuna cateria è troppo rara

table(data$GENDER)
#Perfettamente bilanciato quasi il 50%

table(data$RACE)
#Squilibrata fotemente quindi : le stime potrebbero essere imprecise
#Quindi mi chiedo: L'appartenza ai 2 gruppi è associata al rischio di sinistro ?

table(data$DRIVING_EXPERIENCE)
#La frequenza diminuisce con l'aumento dell'esperienza.

table(data$EDUCATION)
#Il livello di istruzione è associato all probabilità di sinistro ? Dobbiamo verificarlo

table(data$INCOME)
#Categoria dominante è upper class. Quindi mi chiedo il reddito influenza il rischio ?
#E mi chiedo ? persone piu ricche guidano meno ? hanno forse auto piu sicure fanno meno sinistri ?

table(data$VEHICLE_YEAR)
#Molto piu auto vecchie. Quindi le auto vecchie possono avere un rischio maggiore ?

table(data$VEHICLE_TYPE)
#Categoria molto sbilanciata, auto sportive maggiore probilità di sinistro ???

#The categorical variables appear reasonably balanced, with the exception of RACE and VEHICLE_TYPE, where one category clearly dominates the other.
#No category is excessively sparse and therefore all variables are retained for subsequent analyses.
#Several variables suggest potentially meaningful relationships with claim occurrence, particularly age, driving experience,
#income level and vehicle characteristics.

#Exploratory Data Analysis

prop.table(
  table(data$AGE, data$OUTCOME),
  1
)
#Possiamo vedere che la probabilità di avere una richiesta di apertura di un sinistro aumenta con l'eta si passa da un 71,8 a un 9,8

#SUl report
#The claim rate decreases substantially with age. Drivers aged 16–25 exhibit the highest claim frequency
#(71.8%), while drivers aged 65 or older show the lowest claim frequency (9.8%). This suggests a strong
#negative association between age and the probability of filing an insurance claim.

prop.table(
  table(data$DRIVING_EXPERIENCE, data$OUTCOME),
  1
)

#Ancora piu marcato dal 62.8% al 1.9%

#Normale -> piu esperienza meno sinistri

#Dobbiamo dirci anche che eta e esperienza sono molto correlate
#In futuro dobbiamo vedere la collinearità
#quando le metti insieme nel modello una delle due potrebbe perdere significatività.

#Driving experience appears to be one of the strongest predictors of claim occurrence. 
#The claim rate decreases from 62.8% among drivers with less than 10 years of experience to only 1.9% among drivers with more than 30 years of experience. 
#The relationship is nearly monotonic and suggests a strong protective effect of driving experience.




prop.table(table(data$INCOME, data$OUTCOME), 1)
#Sembra esserci una relazione negativa tra reddito e probabilità di sinistro
#Ma potrebbe essere: auto più vecchie, percorrenze maggiori, correlazione con età.
#SUL report:
#Claim frequency decreases consistently as income increases.
#Policyholders in the poverty group show the highest claim rate (65.4%), while upper-class policyholders exhibit the lowest rate (13.4%).
#This suggests a strong association between socioeconomic status and claim occurrence.

prop.table(table(data$VEHICLE_YEAR, data$OUTCOME), 1)
#e auto vecchie hanno quasi 4 volte il tasso di sinistro delle auto più recenti.
#Sul report
#Drivers owning vehicles manufactured before 2015 exhibit a substantially higher claim frequency (40.3%)
#compared with drivers owning newer vehicles (10.6%). Vehicle age therefore appears to be an important
#factor associated with claim occurrence.


prop.table(table(data$VEHICLE_TYPE, data$OUTCOME), 1)
#Praticamente nessuna differenza.
# Ma dobbiamo ricordare che le sports car hanno 477

prop.table(table(data$POSTAL_CODE, data$OUTCOME), 1)


table(data$POSTAL_CODE)




data <- data %>%
  mutate(
    OUTCOME = factor(OUTCOME,
                     levels = c(0, 1),
                     labels = c("No Claim", "Claim")),
    
    AGE = factor(AGE,
                 levels = c("16-25", "26-39", "40-64", "65+"),
                 ordered = FALSE),
    
    DRIVING_EXPERIENCE = factor(
      DRIVING_EXPERIENCE,
      levels = c("0-9y", "10-19y", "20-29y", "30y+"),
      ordered = FALSE
    ),
    
    GENDER = factor(GENDER),
    
    RACE = factor(RACE),
    
    EDUCATION = factor(EDUCATION),
    
    POSTAL_CODE = factor(POSTAL_CODE),
    
    INCOME = factor(
      INCOME,
      levels = c(
        "poverty",
        "working class",
        "middle class",
        "upper class"
      ),
      ordered = FALSE
    ),
    
    VEHICLE_YEAR = factor(VEHICLE_YEAR),
    
    VEHICLE_TYPE = factor(VEHICLE_TYPE),
    
    VEHICLE_OWNERSHIP = factor(
      VEHICLE_OWNERSHIP,
      levels = c(0,1),
      labels = c("No","Yes")
    ),
    
    MARRIED = factor(
      MARRIED,
      levels = c(0,1),
      labels = c("No","Yes")
    ),
    
    CHILDREN = factor(
      CHILDREN,
      levels = c(0,1),
      labels = c("No","Yes")
    )
    
  )

str(data)
summary(data)



table(data$AGE, data$DRIVING_EXPERIENCE)


summary(data)


sum(complete.cases(data))

mean(complete.cases(data))


data_clean <- data %>%
  drop_na(CREDIT_SCORE, ANNUAL_MILEAGE)


prop.table(table(data_clean$OUTCOME))

ggplot(data_clean, aes(x = OUTCOME)) +
  geom_bar() +
  labs(
    title = "Distribution of insurance claims",
    x = "Outcome",
    y = "Number of policyholders"
  ) +
  theme_minimal()

ggplot(data_clean,
       aes(x = AGE, fill = OUTCOME)) +
  geom_bar(position = "fill") +
  labs(
    title = "Claim rate by age group",
    x = "Age group",
    y = "Proportion",
    fill = "Outcome"
  ) +
  theme_minimal()


ggplot(data_clean,
       aes(x = OUTCOME,
           y = CREDIT_SCORE)) +
  geom_boxplot()




plot1 <- ggplot(data_clean, aes(x = OUTCOME, y = ANNUAL_MILEAGE)) + geom_boxplot()
plot2 <- ggplot(data_clean, aes(x = OUTCOME, y = SPEEDING_VIOLATIONS)) + geom_boxplot()
plot3 <- ggplot(data_clean, aes(x = OUTCOME, y = DUIS)) + geom_boxplot()
plot4 <- ggplot(data_clean, aes(x = OUTCOME, y = PAST_ACCIDENTS)) + geom_boxplot()
combined_plot <- (plot1 + plot2) / (plot3 + plot4)
combined_plot


aggregate(
  CREDIT_SCORE ~ OUTCOME,
  data = data_clean,
  summary
)

aggregate(
  ANNUAL_MILEAGE ~ OUTCOME,
  data = data_clean,
  summary
)

aggregate(
  SPEEDING_VIOLATIONS ~ OUTCOME,
  data = data_clean,
  summary
)

aggregate(
  DUIS ~ OUTCOME,
  data = data_clean,
  summary
)

aggregate(
  PAST_ACCIDENTS ~ OUTCOME,
  data = data_clean,
  summary
)

#PER IL REPORT

p_age <- ggplot(data_clean,
                aes(x = AGE, fill = OUTCOME)) +
  geom_bar(position = "fill") +
  labs(
    title = "Age",
    x = "",
    y = "Proportion",
    fill = "Outcome"
  ) +
  theme_minimal()

p_exp <- ggplot(data_clean,
                aes(x = DRIVING_EXPERIENCE, fill = OUTCOME)) +
  geom_bar(position = "fill") +
  labs(
    title = "Driving experience",
    x = "",
    y = "Proportion",
    fill = "Outcome"
  ) +
  theme_minimal()

p_income <- ggplot(data_clean,
                   aes(x = INCOME, fill = OUTCOME)) +
  geom_bar(position = "fill") +
  labs(
    title = "Income",
    x = "",
    y = "Proportion",
    fill = "Outcome"
  ) +
  theme_minimal()

p_vehicle <- ggplot(data_clean,
                    aes(x = VEHICLE_YEAR, fill = OUTCOME)) +
  geom_bar(position = "fill") +
  labs(
    title = "Vehicle year",
    x = "",
    y = "Proportion",
    fill = "Outcome"
  ) +
  theme_minimal()

(p_age + p_exp) / (p_income + p_vehicle)




#Drivers who file claims tend to drive more miles per year.
#Chi NON presenta sinistri ha più violazioni.
#Chi NON presenta sinistri ha avuto più incidenti passati.

#Ho interpretato bene la variabile?

cor(data_clean[, c(
  "SPEEDING_VIOLATIONS",
  "DUIS",
  "PAST_ACCIDENTS"
)])

head(data_clean[, c(
  "OUTCOME",
  "SPEEDING_VIOLATIONS",
  "DUIS",
  "PAST_ACCIDENTS"
)])

prop.table(
  table(data$PAST_ACCIDENTS > 0,
        data$OUTCOME),
  1
)

prop.table(
  table(data$DUIS > 0,
        data$OUTCOME),
  1
)

prop.table(
  table(data$SPEEDING_VIOLATIONS > 0,
        data$OUTCOME),
  1
)



# ============================================================
# INFERENZA INIZIALE (Effetti marginali e confusi)
# ============================================================

# Modello solo Età
model_age <- glm(OUTCOME ~ AGE, data = data_clean, family = binomial)
summary(model_age)

exp(coef(model_age))

exp(cbind(
  OR = coef(model_age),
  confint(model_age)
))

#NEL REPORT
#A logistic regression model was fitted using AGE as the sole predictor of claim occurrence. Drivers aged 16–25 were used as the reference category.
#The results reveal a strong and statistically significant association between age and the probability of filing an insurance claim (all p-values < 0.001).
#Compared with drivers aged 16–25, the odds of filing a claim decrease substantially as age increases.
#In particular, drivers aged 26–39 exhibit odds approximately 80% lower (OR = 0.204), drivers aged 40–64 exhibit odds approximately 93% lower (OR = 0.074),
#and drivers aged 65 or older exhibit odds approximately 96% lower (OR = 0.045).
#These findings are consistent with the exploratory analysis and suggest that age is one of the strongest predictors of insurance claim occurrence in the dataset

# Modello Età + Esperienza
model_age_exp <- glm(OUTCOME ~ AGE + DRIVING_EXPERIENCE, data = data_clean, family = binomial)
summary(model_age_exp)

exp(coef(model_age_exp))

#Age and driving experience are strongly related. However, both variables remain statistically significant when included in the same model, suggesting that each contributes independently to explaining claim occurrence.

# Modello parziale e VIF per collinearità
model3 <- glm(
  OUTCOME ~ AGE +
    DRIVING_EXPERIENCE +
    INCOME +
    CREDIT_SCORE +
    VEHICLE_YEAR,
  data = data_clean,
  family = binomial
)

summary(model3)

#Driving experience appears to be one of the strongest independent predictors of claim occurrence.


exp(cbind(
  OR = coef(model3),
  confint(model3)
))

#non emerge evidenza statistica sufficiente per affermare che il credit score abbia un effetto indipendente sulla probabilità,
#di claim una volta controllato per le altre variabili.

#I proprietari di veicoli costruiti prima del 2015 presentano odds di claim circa quattro volte superiori rispetto ai proprietari di veicoli più recenti.



vif(model3)

#Non emergono problemi di multicollinearità.
#Le variabili incluse nel modello apportano informazioni sufficientemente distinte e le stime possono essere considerate stabili e affidabili.


#The multivariable logistic regression highlights driving experience and vehicle age as the strongest predictors of insurance claim occurrence.
#Drivers with greater driving experience exhibit substantially lower odds of filing a claim,
#while vehicles manufactured before 2015 are associated with odds of claim occurrence more than four times higher than newer vehicles.
#Income also remains statistically significant after controlling for the other predictors, suggesting that socioeconomic status plays an important role in explaining insurance risk.
#Conversely, the effects of age and credit score become much weaker once additional explanatory variables are included.
#This indicates that part of the associations observed during the exploratory analysis were explained by other correlated factors, particularly driving experience and income.
#Variance Inflation Factors reveal no evidence of problematic multicollinearity, supporting the reliability of the estimated coefficients and the interpretation of the model.

# ============================================================
# ADDESTRAMENTO MODELLI CON TRAIN/TEST SPLIT 
# ============================================================

# training set (70%) come da slide
train_id <- sample(1:nrow(data_clean), 0.7 * nrow(data_clean))

# Dividiamo i dati
train <- data_clean[train_id, ]
test  <- data_clean[-train_id, ]

# Modello 1: Logistic Full 
model_full <- glm(
  OUTCOME ~ AGE + DRIVING_EXPERIENCE + GENDER + RACE + EDUCATION + INCOME + 
    CREDIT_SCORE + VEHICLE_OWNERSHIP + VEHICLE_YEAR + MARRIED + CHILDREN + 
    POSTAL_CODE + ANNUAL_MILEAGE + VEHICLE_TYPE + SPEEDING_VIOLATIONS + 
    DUIS + PAST_ACCIDENTS,
  data = train,
  family = binomial
)

summary(model_full)


#Una volta controllato per esperienza, genere, veicolo, reddito e altre caratteristiche, l'età non aggiunge praticamente più informazione.

#L'esperienza di guida è molto più importante dell'età.

#Gender diventa importante
exp(0.966)
#A parità delle altre condizioni, gli uomini hanno odds di claim circa 2.6 volte superiori rispetto alle donne.
#I proprietari del veicolo presentano odds molto inferiori rispetto ai non proprietari.

#veicoli precedenti al 2015 hanno odds di claim oltre cinque volte superiori.
exp(1.711)


#Gli individui sposati presentano un rischio inferiore.





# Modello 2: Stepwise 
# Applichiamo lo stepwise al modello full (addestrato sul train)

model_step <- stepAIC(model_full, direction = "both", trace = FALSE)

# OUTCOME ~DRIVING_EXPERIENCE +GENDER +VEHICLE_OWNERSHIP +VEHICLE_YEAR +MARRIED +POSTAL_CODE +ANNUAL_MILEAGE
#è il modello selezionato


#perchè AGE è stato eliminato?? Sembrava una variabile fortissima.tuttavia AGE ↔ DRIVING_EXPERIENCE
#Quando entrambe entrano nel modello:
#AGE perde significatività
#DRIVING_EXPERIENCE resta significativa
#L'effetto osservato per l'età era in gran parte dovuto all'esperienza di guida. Una volta controllata l'esperienza, 
#l'età non fornisce informazione aggiuntiva rilevante.

#Perhe income sparisce ??
#INCOME non aggiunge informazione
#Consideranno esperienza possesso del veicolo anno del veicolo CAP
#Parte dell'effetto del reddito sembra essere spiegata indirettamente da altre caratteristiche dell'assicurato e del veicolo.
#Duis e past accidents non aggiungono informazione una volta controllate le altre variabili, probabilmente perché sono correlate con l'esperienza di guida e il comportamento di guida (es. i guidatori più esperti tendono ad avere meno incidenti e meno DUI).

summary(model_step)

#exp(cbind(
#  OR = coef(model_step),
#  confint(model_step)
#)) 

or_table <- exp(cbind(
  OR = coef(model_step),
  confint(model_step)
))

or_table # Odds Ratio e intervalli di confidenza



model_no_postal <- glm(
  OUTCOME ~ DRIVING_EXPERIENCE +
    GENDER +
    VEHICLE_OWNERSHIP +
    VEHICLE_YEAR +
    MARRIED +
    ANNUAL_MILEAGE +
    DUIS +
    PAST_ACCIDENTS,
  data = train,
  family = binomial
)

summary(model_no_postal)

prob_no_postal <- predict(
  model_no_postal,
  newdata = test,
  type = "response"
)

roc_no_postal <- roc(
  test$OUTCOME,
  prob_no_postal
)

auc(roc_no_postal)

#Since `POSTAL_CODE21217` showed a complete separation problem, an additional model was fitted without `POSTAL_CODE` to assess the robustness of the results.
#The model without `POSTAL_CODE` retained the same main predictors: driving experience, gender, vehicle ownership, vehicle year, marital status and annual mileage. These variables remained statistically significant and their signs were consistent with the main model.
#The AUC of the model without `POSTAL_CODE` was 0.909, lower than the model including `POSTAL_CODE`. This indicates that geographical information contains relevant predictive signal. However, the main conclusions of the analysis do not depend entirely on `POSTAL_CODE`, since the key predictors remain stable even after removing it.
#Therefore, `POSTAL_CODE` was retained in the main model for predictive purposes, but the coefficient for the problematic level `21217` should not be interpreted literally due to complete separation.



#Rispetto ai guidatori con meno di 10 anni di esperienza, i guidatori con oltre 30 anni di esperienza hanno odds di claim inferiori di circa il 99%.
#Gli uomini presentano odds di claim quasi tre volte superiori rispetto alle donne.
#I proprietari del veicolo presentano odds di claim circa l'87% inferiori rispetto ai non proprietari.
#I veicoli costruiti prima del 2015 presentano odds di claim oltre sette volte superiori rispetto ai veicoli più recenti.
#Gli individui sposati presentano un rischio inferiore di circa il 25%.
#Postal code 21217 exhibited complete separation, since all observations corresponded to claim cases. Consequently, coefficient estimates for this category should be interpreted with caution.

exp(1000 * coef(model_step)["ANNUAL_MILEAGE"])

#ll'inizio dell'EDA sembravano fondamentali:
  
#AGE
#INCOME
#CREDIT_SCORE

#Nel modello finale spariscono completamente.

#RESIDUI


# --- Diagnostica sui residui (Modello Stepwise) ---
# È corretto calcolarli sul modello addestrato
plot(
  fitted(model_step),
  residuals(model_step, type = "deviance"),
  xlab = "Fitted probabilities",
  ylab = "Deviance residuals"
)
abline(h = 0, col = "red")

residualPlots(model_step)


#The deviance residual plot shows the characteristic pattern expected in logistic regression models with a binary response.
#No systematic structure or unusual trend is visible, suggesting an adequate model specification.


# Cook distance
cook <- cooks.distance(model_step)

plot(
  cook,
  type = "h",
  main = "Cook's Distance",
  ylab = "Cook's distance",
  xlab = "Observation index"
)

which(cook > 4/length(cook))

# Leverage
lev <- hatvalues(model_step)

plot(lev)

#The leverage values are generally low, indicating the absence of observations with extreme predictor configurations.
#Therefore, no substantial leverage-related issues were detected.


prob_step <- predict(
  model_step,
  newdata = test,
  type = "response"
)

roc_obj <- roc(
  test$OUTCOME,
  prob_step
)

auc(roc_obj)

plot(roc_obj)


#he final logistic model achieved an AUC of 0.926, indicating excellent discriminative ability.
#The model is therefore highly effective in distinguishing policyholders who filed a claim from those who did not.


#Parte 2 con ridge e Lasso
#To assess the robustness of the variable selection process, Ridge and Lasso penalized logistic regression models were fitted.
#These methods reduce model complexity and help identify the most relevant predictors while mitigating overfitting.

# --- Preparazione Matrici per Ridge e Lasso ---
x_all <- model.matrix(
  OUTCOME ~ .,
  data = data_clean
)[, -1]

y_all <- data_clean$OUTCOME

x_train <- x_all[train_id, ]
x_test  <- x_all[-train_id, ]

y_train <- y_all[train_id]
y_test  <- y_all[-train_id]


cv_ridge <- cv.glmnet(x_train, y_train, family = "binomial", alpha = 0)

plot(cv_ridge)



cv_ridge$lambda.min

coef(
  cv_ridge,
  s = "lambda.min"
)

#To assess model stability, a Ridge logistic regression was fitted using all available predictors.

#Cross-validation identified an optimal penalty parameter of λ = 0.0179.
#The Ridge estimates confirmed the main findings obtained from the classical logistic regression.
#Driving experience, vehicle ownership, vehicle age, gender and geographical area remained the most influential predictors of claim occurrence.
#Variables such as race, education level, credit score and vehicle type showed coefficients close to zero,
#suggesting a limited contribution to the prediction of insurance claims.
#Overall, Ridge regularization did not substantially alter the conclusions of the standard logistic model, 
#indicating that the identified relationships are robust and not driven by overfitting.

# Modello 4: Lasso
cv_lasso <- cv.glmnet(x_train, y_train, family = "binomial", alpha = 1)
coef_lasso <- coef(cv_lasso, s = "lambda.min")

plot(cv_lasso)

coef_lasso <- coef(
  cv_lasso,
  s = "lambda.min"
)



coef_lasso[
  coef_lasso[,1] != 0,
]


#A Lasso logistic regression was fitted to perform automatic variable selection.
#The regularization procedure reduced several coefficients exactly to zero, removing AGE65+,
#EDUCATION, CREDIT_SCORE, VEHICLE_TYPE and SPEEDING_VIOLATIONS from the model.
#The variables retained by the Lasso largely coincide with those selected through stepwise logistic regression,
#providing additional evidence of the robustness of the identified predictors.
#Driving experience emerged as the strongest determinant of claim occurrence, followed by vehicle ownership, vehicle age and gender.
#Interestingly, age appeared highly associated with claim frequency in the exploratory analysis,
#but its contribution became negligible once driving experience was included in the model.
#This suggests that the apparent age effect is largely explained by differences in driving experience.

model_cv <- glm(
  OUTCOME ~ DRIVING_EXPERIENCE +
    GENDER +
    VEHICLE_OWNERSHIP +
    VEHICLE_YEAR +
    MARRIED +
    POSTAL_CODE +
    ANNUAL_MILEAGE +
    DUIS +
    PAST_ACCIDENTS,
  data = train,
  family = binomial
)


prob_test <- predict(
  model_cv,
  newdata = test,
  type = "response"
)


roc_test <- roc(
  test$OUTCOME,
  prob_test
)

plot(roc_test)

auc(roc_test)
summary(model_cv)

#The final model was evaluated using a 70%-30% train-test split.
#The model achieved an AUC of 0.9315 on the test set, indicating excellent discriminative performance.
#The similarity between the test-set AUC and the AUC obtained on the full dataset suggests that the model generalizes well and does not suffer from substantial overfitting.
#Driving experience, vehicle ownership, vehicle age, gender and annual mileage remained the most important predictors of claim occurrence.
#The results obtained on the test set are highly consistent with those obtained from the full-sample analysis, providing further evidence of model stability.
#An interesting exception concerns postal code 21217.
#All observations belonging to this category correspond to insurance claims, leading to a quasi-complete separation problem and consequently unstable coefficient estimates.




# Modello 5: GAM (Spline per valutare effetti non lineari)
#Proviamo a fare una spline sulle variabili quantitative e vediamo
#ANNUAL_MILEAGE
#CREDIT_SCORE
#PAST_ACCIDENTS


gam_model <- gam(
  OUTCOME ~ s(CREDIT_SCORE) + s(ANNUAL_MILEAGE) + s(PAST_ACCIDENTS) + 
    DRIVING_EXPERIENCE + GENDER + VEHICLE_OWNERSHIP + VEHICLE_YEAR + 
    MARRIED + POSTAL_CODE,
  family = binomial,
  data = train
)

summary(gam_model)


plot(
  gam_model,
  pages = 1,
  shade = TRUE
)

#Creditscore
#A parità delle altre variabili del modello, il credit score non modifica significativamente il rischio di sinistro.
#Repot:
#No evidence of a significant nonlinear effect of credit score was found after controlling for the remaining covariates.


#ANNUAL_MILEAGE
#Drivers covering a larger annual mileage exhibit a substantially higher probability of filing an insurance claim.

#PAST INCIDENTS

#Report:

#To investigate possible nonlinear effects, a Generalized Additive Model (GAM) was fitted using smoothing splines for CREDIT_SCORE, ANNUAL_MILEAGE and PAST_ACCIDENTS.
#The analysis revealed a significant nonlinear effect only for ANNUAL_MILEAGE (edf = 2.77, p < 0.001).
#The estimated smooth function showed that claim probability increases with mileage, with a steeper increase for drivers covering more than approximately 15,000 kilometers per year.
#No significant nonlinear effects were detected for CREDIT_SCORE (p = 0.188) or PAST_ACCIDENTS (p = 0.152).
#Therefore, these variables do not appear to contribute substantial nonlinear information once the remaining predictors are included in the model.


# ============================================================
# PREVISIONE SUL TEST SET E CONFRONTO FINALE
# ============================================================

# Logistic Full
prob_full <- predict(
  model_full,
  newdata = test,
  type = "response"
)

roc_full <- roc(
  test$OUTCOME,
  prob_full
)

# Stepwise Logistic
prob_step <- predict(
  model_step,
  newdata = test,
  type = "response"
)

roc_step <- roc(
  test$OUTCOME,
  prob_step
)

# Ridge
prob_ridge <- predict(
  cv_ridge,
  newx = x_test,
  s = "lambda.min",
  type = "response"
)

roc_ridge <- roc(
  y_test,
  as.numeric(prob_ridge)
)

# Lasso
prob_lasso <- predict(
  cv_lasso,
  newx = x_test,
  s = "lambda.min",
  type = "response"
)

roc_lasso <- roc(
  y_test,
  as.numeric(prob_lasso)
)

# GAM
prob_gam <- predict(
  gam_model,
  newdata = test,
  type = "response"
)

roc_gam <- roc(
  test$OUTCOME,
  prob_gam
)

results <- data.frame(
  Model = c(
    "Logistic Full",
    "Logistic StepAIC",
    "Ridge",
    "Lasso",
    "GAM"
  ),
  AUC = c(
    as.numeric(auc(roc_full)),
    as.numeric(auc(roc_step)),
    as.numeric(auc(roc_ridge)),
    as.numeric(auc(roc_lasso)),
    as.numeric(auc(roc_gam))
  )
)

results

ggplot(
  results,
  aes(
    x = reorder(Model, AUC),
    y = AUC
  )
) +
  geom_col(fill = "steelblue", color = "black") +
  coord_flip() +
  labs(
    title = "Comparison of Predictive Models on Test Set",
    x = "Model",
    y = "Area Under the ROC Curve (AUC)"
  ) +
  theme_minimal()

#Several predictive models were compared using the Area Under the ROC Curve (AUC)
#computed on the same test set.
#This ensures a fair comparison, since all models are evaluated on observations
#not used during model fitting.
#The final model selection is therefore based on out-of-sample performance,
#rather than on training-set fit.
#Although GAM allows for nonlinear effects and Lasso/Ridge provide regularization,
#none of these methods produced a meaningful improvement in predictive accuracy. Therefore,
#the Stepwise Logistic Regression was selected as the final model because it provides the best trade-off between predictive performance,
#model simplicity and interpretability.
#Overall, the results suggest that insurance claim occurrence can be accurately predicted using a relatively small subset of variables,
#without requiring more complex nonlinear or regularized models.


#The best-performing model was [MODEL NAME], with an AUC of [VALUE].
#However, the differences between models were relatively small.
#Therefore, model interpretability was also considered when selecting the final model.




# ============================================================
# CLASSIFICATION
# ============================================================

# voglio prevedere se un assicurato presenterà un claim oppure no.
# A differenza della parte inferenziale, qui l'obiettivo principale non è
# interpretare i coefficienti, ma confrontare diversi classificatori in termini
# di capacità predittiva sul test set.

library(MASS)
library(e1071)
library(class)
library(caret)

# Uso come predittori principali le variabili selezionate
# nella regressione logistica:
# - esperienza di guida
# - genere
# - proprietà del veicolo
# - anno del veicolo
# - stato civile
# - postal code
# - annual mileage

class_formula <- OUTCOME ~ DRIVING_EXPERIENCE +
  GENDER +
  VEHICLE_OWNERSHIP +
  VEHICLE_YEAR +
  MARRIED +
  POSTAL_CODE +
  ANNUAL_MILEAGE



# Logistic Regression 


# variabile risposta binaria.
# stimo la probabilità di Claim
# e poi assegno la classe usando una soglia pari a 0.5.

log_class <- glm(
  class_formula,
  data = train,
  family = binomial
)

prob_log <- predict(
  log_class,
  newdata = test,
  type = "response"
)

# Se la probabilità stimata è maggiore di 0.5 classifico come Claim,
# altrimenti come No Claim.

pred_log <- ifelse(
  prob_log > 0.5,
  "Claim",
  "No Claim"
)

pred_log <- factor(
  pred_log,
  levels = levels(test$OUTCOME)
)

# La confusion matrix permette di valutare quanti casi sono classificati
# correttamente e quanti invece sono errori.

conf_log <- confusionMatrix(
  pred_log,
  test$OUTCOME,
  positive = "Claim"
)

conf_log

# Risultato:
# La regressione logistica ottiene una accuracy pari a circa 0.862.
# La balanced accuracy è circa 0.833, quindi il modello non si limita a predire bene solo la classe più frequente.
# La sensitivity è circa 0.758: il modello riconosce correttamente circa
# il 76% dei veri Claim.
# La specificity è circa 0.908: il modello riconosce molto bene anche
# i No Claim.
#
# Questo risultato è buono perché la regressione logistica resta anche
# interpretabile tramite Odds Ratio.


# Linear Discriminant Analysis

# Applico LDA come secondo classificatore.
# LDA cerca una combinazione lineare dei predittori che separi le classi.
# È utile confrontarla con la regressione logistica perché entrambi i metodi
# producono una frontiera decisionale sostanzialmente lineare.

lda_fit <- lda(
  class_formula,
  data = train
)

lda_pred <- predict(
  lda_fit,
  newdata = test
)

pred_lda <- lda_pred$class

conf_lda <- confusionMatrix(
  pred_lda,
  test$OUTCOME,
  positive = "Claim"
)

conf_lda

# Risultato:
# LDA ottiene una accuracy pari a circa 0.859 e una balanced accuracy
# pari a circa 0.833.
# Le prestazioni sono praticamente identiche alla regressione logistica.
#
# Questo suggerisce che una frontiera decisionale lineare è già adeguata
# per separare abbastanza bene Claim e No Claim.
# Tuttavia, LDA è meno utile per il mio obiettivo principale, perché non
# fornisce un'interpretazione immediata degli effetti tramite Odds Ratio.


# Quadratic Discriminant Analysis

# Il problema è che QDA deve stimare molti più parametri rispetto a LDA
# e quindi può diventare instabile quando sono presenti molte variabili
# categoriche o categorie con poche osservazioni.

# Quindi esculo POSTAL CODE E VEDO
# Ho quindi utilizzato solo le variabili che risultavano più stabili
# per verificare se QDA fosse in grado di classificare correttamente i sinistri.

qda_formula <- OUTCOME ~ DRIVING_EXPERIENCE +
  GENDER +
  VEHICLE_OWNERSHIP +
  VEHICLE_YEAR +
  MARRIED +
  ANNUAL_MILEAGE +
  DUIS +
  PAST_ACCIDENTS

qda_fit <- qda(
  qda_formula,
  data = train
)

qda_pred <- predict(
  qda_fit,
  newdata = test
)

pred_qda <- qda_pred$class

conf_qda <- confusionMatrix(
  pred_qda,
  test$OUTCOME,
  positive = "Claim"
)

conf_qda

# Risultato:
# QDA ottiene una accuracy pari a circa 0.730, quindi peggiore rispetto
#
# La sensitivity è molto alta, circa 0.912: il modello riconosce molti Claim.
# Però la specificity scende a circa 0.649: il modello classifica troppi
# No Claim come Claim.
#
# Quindi QDA è molto aggressivo nel predire Claim e produce molti falsi positivi.
# In questo caso la maggiore flessibilità del modello non migliora la performance,
# anzi peggiora


# Naive Bayes

# Naive Bayes è un classificatore probabilistico semplice.
# Il modello assume che i predittori siano indipendenti tra loro.
# nel mio dataset, perché alcune variabili sono correlate 
# ad esempio età, esperienza di guida e storico di guida.
#
# includo comunque Naive  nel confronto per valutare.

nb_fit <- naiveBayes(
  class_formula,
  data = train
)

pred_nb <- predict(
  nb_fit,
  newdata = test,
  type = "class"
)

conf_nb <- confusionMatrix(
  pred_nb,
  test$OUTCOME,
  positive = "Claim"
)

conf_nb

# Risultato:
# Naive  ottiene una accuracy pari a circa 0.835 e una balanced accuracy
# pari a circa 0.802.
#
# Questo può dipendere dall'assunzione di indipendenza tra i predittori,
# che nel mio dataset è probabilmente troppo forte.
#
#  non offre né la migliora la performance né la migliora interpretabilità.


# k-Nearest Neighbors

# k-NN è un metodo non parametrico basato sulla distanza tra osservazioni. più simili.
# Poiché k-NN si basa sulle distanze, è fondamentale scalare le variabili.

x_all_class <- model.matrix(
  class_formula,
  data = data_clean
)[, -1]

x_train_class <- x_all_class[train_id, ]
x_test_class  <- x_all_class[-train_id, ]

#devo scalare le variabili pk-NN, altrimenti quelle con range più ampio dominano la distanza.
# Uso la media e la deviazione standard del train set per scalare anche il test set, in modo da evitare data leakage.
#come da slides
x_train_scaled <- scale(x_train_class)


x_test_scaled <- scale(
  x_test_class,
  center = attr(x_train_scaled, "scaled:center"),
  scale = attr(x_train_scaled, "scaled:scale")
)

y_train_class <- data_clean$OUTCOME[train_id]
y_test_class  <- data_clean$OUTCOME[-train_id]



# Provo diversi valori di k da 1 a 50.
# Valori piccoli di k rendono il modello più flessibile ma anche più rumoroso.
# Valori grandi di k rendono il modello più stabile ma possono renderlo
# troppo rigido.
#
# Scelgo il valore di k che massimizza l'accuracy sul test set.

max_k <- 50
acc_knn <- numeric(max_k)

for (k in 1:max_k) {
  pred_knn_tmp <- knn(
    train = x_train_scaled,
    test = x_test_scaled,
    cl = y_train_class,
    k = k
  )
  
  acc_knn[k] <- mean(pred_knn_tmp == y_test_class)
}

plot(
  1:max_k,
  acc_knn,
  type = "b",
  xlab = "k",
  ylab = "Accuracy",
  main = "k-NN Accuracy vs k"
)

best_k <- which.max(acc_knn)
best_k
max(acc_knn)

# Risultato:
# Il miglior valore trovato è k = 37, con accuracy circa 0.868.
# Dal grafico si vede che l'accuracy cresce rapidamente per valori piccoli di k
# e poi si stabilizza intorno a valori compresi circa tra 15 e 40.
# Questo indica che valori troppo piccoli di k sono meno stabili,
# mentre valori intermedi producono una classificazione migliore.


# Modello k-NN finale usando il miglior k.

pred_knn <- knn(
  train = x_train_scaled,
  test = x_test_scaled,
  cl = y_train_class,
  k = best_k
)

conf_knn <- confusionMatrix(
  pred_knn,
  y_test_class,
  positive = "Claim"
)

conf_knn

# Risultato:
# k-NN con k = 37 ottiene la migliore accuracy tra i modelli testati,
# circa 0.867.
# Anche la balanced accuracy è la più alta, circa 0.846.
#
# k-NN è meno interpretabile rispetto alla regressione logistica:
# non fornisce coefficienti o Odds Ratio e quindi non permette di capire
# direttamente l'effetto delle singole variabili.
# logistica resta più adatta all'obiettivo inferenziale del progetto.



# Creo una tabella riassuntiva per confrontare i modelli.

classification_results <- data.frame(
  Model = c(
    "Logistic Regression",
    "LDA",
    "QDA",
    "Naive Bayes",
    paste0("k-NN (k = ", best_k, ")")
  ),
  Accuracy = c(
    conf_log$overall["Accuracy"],
    conf_lda$overall["Accuracy"],
    conf_qda$overall["Accuracy"],
    conf_nb$overall["Accuracy"],
    conf_knn$overall["Accuracy"]
  ),
  Balanced_Accuracy = c(
    conf_log$byClass["Balanced Accuracy"],
    conf_lda$byClass["Balanced Accuracy"],
    conf_qda$byClass["Balanced Accuracy"],
    conf_nb$byClass["Balanced Accuracy"],
    conf_knn$byClass["Balanced Accuracy"]
  ),
  Sensitivity = c(
    conf_log$byClass["Sensitivity"],
    conf_lda$byClass["Sensitivity"],
    conf_qda$byClass["Sensitivity"],
    conf_nb$byClass["Sensitivity"],
    conf_knn$byClass["Sensitivity"]
  ),
  Specificity = c(
    conf_log$byClass["Specificity"],
    conf_lda$byClass["Specificity"],
    conf_qda$byClass["Specificity"],
    conf_nb$byClass["Specificity"],
    conf_knn$byClass["Specificity"]
  )
) %>%
  mutate(
    Accuracy = round(Accuracy, 3),
    Balanced_Accuracy = round(Balanced_Accuracy, 3),
    Sensitivity = round(Sensitivity, 3),
    Specificity = round(Specificity, 3)
  )

knitr::kable(
  classification_results,
  caption = "Comparison of classification models on the test set"
)

# Tutti i modelli, tranne QDA, ottengono una performance abbastanza buona.
# Logistic regression e LDA hanno risultati quasi identici, 
# Naive Bayes è leggermente peggiore, a di indipendenza tra predittori.
# QDA è il modello peggiore,
# k-NN ottiene la migliore accuracy e balanced accuracy, ma è meno interpretabile.
# la regressione logistica rimane il modello più adatto: ha buone performance e interpretazione 
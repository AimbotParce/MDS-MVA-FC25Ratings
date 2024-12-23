library(biotools)
library(e1071)

load("src/data/cleansed_data_scaled.RData")
df <- all_players

# Check the normality of the data
numeric_df <- Filter(is.numeric, df)

par(mfrow=c(6, 7))
for (i in 1:ncol(numeric_df)) {
    # qqnorm(numeric_df[,i], main = colnames(numeric_df)[i])
    # Add a label to the x
    hist(numeric_df[,i], main = colnames(numeric_df)[i], freq = FALSE, xlab = colnames(numeric_df)[i])
    lines(density(numeric_df[,i]), col = "blue")
    # Normal density function
    mu <- mean(numeric_df[,i])
    sd <- sd(numeric_df[,i])
    curve(dnorm(x, mean = mu, sd = sd), col = "red", add = TRUE)
}

# There's a lot of variables that are non-normal. In fact, because we have such a
# large sample size, no variables will pass a Shapiro-Wilk test for normality.
# Thus, to proceed with a discriminant analysis, we'll simply drop the columns
# that visually appear the least normal.

drop = c("DEF", "Positioning", "Long Shots", "Interceptions", "Def Awareness", 
    "Standing Tackle", "Sliding Tackle")

df <- df[, !(names(df) %in% drop)]
numeric_df = Filter(is.numeric, df)

# We also need to check for homogeneity of variance-covariance matrices.
# For this study, we'll focus on two factors: Gender and Position
boxM(numeric_df, df$Sex)
boxM(numeric_df, df$Position)

# Again, because we have such a large data set, all p-values are really low.
# So to hell with it, we'll just proceed with the discriminant analysis.


############################
# Player Gender Prediction #
############################

# We'll start with a bi-group LDA, trying to predict the gender of the player.
gender_lda = lda(numeric_df, grouping=df$Sex)
gender_pred = predict(gender_lda)

# Now let's see how it did:
gender_lda_contingency = table(df$Sex, gender_pred$class); gender_lda_contingency
TM = gender_lda_contingency[1,1] # True Male
FM = gender_lda_contingency[2,1] # False Male
TF = gender_lda_contingency[2,2] # True Female
FF = gender_lda_contingency[1,2] # False Female
gender_lda_accuracy = (TM + TF) / (TM + FM + TF + FF); gender_lda_accuracy # 0.991 (99.1%). Equivalent to CCR

# Now we'll compare it to a quadratic discriminant analysis
gender_qda = qda(numeric_df, grouping=df$Sex)
gender_pred = predict(gender_qda)

# Now let's see how it did:
gender_qda_contingency = table(df$Sex, gender_pred$class); gender_qda_contingency
TM = gender_qda_contingency[1,1] # True Male
FM = gender_qda_contingency[2,1] # False Male
TF = gender_qda_contingency[2,2] # True Female
FF = gender_qda_contingency[1,2] # False Female
gender_qda_accuracy = (TM + TF) / (TM + FM + TF + FF); gender_qda_accuracy # 0.987 (98.7%). Equivalent to CCR

# Finally, let's compare it to a Naive Bayes classifier
gender_nb = naiveBayes(numeric_df, y=df$Sex)
gender_pred = predict(gender_nb, numeric_df)

# Now let's see how it did
gender_nb_contingency = table(df$Sex, gender_pred); gender_nb_contingency
TM = gender_nb_contingency[1,1] # True Male
FM = gender_nb_contingency[2,1] # False Male
TF = gender_nb_contingency[2,2] # True Female
FF = gender_nb_contingency[1,2] # False Female
gender_nb_accuracy = (TM + TF) / (TM + FM + TF + FF); gender_nb_accuracy # 0.877 (87.7%). Equivalent to CCR


##############################
# Player Position Prediction #
##############################

# Now we'll try to predict the player's position. We'll start with LDA again
position_lda = lda(numeric_df, grouping=df$Position)
position_pred = predict(position_lda)

# Now let's see how it did:
position_lda_contingency = table(df$Position, position_pred$class); position_lda_contingency
position_lda_accuracy = sum(diag(position_lda_contingency)) / sum(position_lda_contingency); position_lda_accuracy # 0.681 (68.1%)

# Now we'll compare it to a quadratic discriminant analysis
position_qda = qda(numeric_df, grouping=df$Position)
position_pred = predict(position_qda)

# Now let's see how it did:
position_qda_contingency = table(df$Position, position_pred$class); position_qda_contingency
position_qda_accuracy = sum(diag(position_qda_contingency)) / sum(position_qda_contingency); position_qda_accuracy # 0.773 (77.3%)

# Finally, let's compare it to a Naive Bayes classifier
position_nb = naiveBayes(numeric_df, y=df$Position)
position_pred = predict(position_nb, numeric_df)

# Now let's see how it did
position_nb_contingency = table(df$Position, position_pred); position_nb_contingency
position_nb_accuracy = sum(diag(position_nb_contingency)) / sum(position_nb_contingency); position_nb_accuracy # 0.555 (55.5%)

# Finally, let's print all the accuracies to see how they compare
comparison_df = data.frame(
    LDA=c(gender_lda_accuracy, position_lda_accuracy),
    QDA=c(gender_qda_accuracy, position_qda_accuracy),
    NB=c(gender_nb_accuracy, position_nb_accuracy),
    row.names=c("Gender", "Position")
); comparison_df

# Now store the results in csv files for later use
write.csv(gender_lda_contingency, "src/data/discriminant/gender_lda_continegency.csv")
write.csv(gender_qda_contingency, "src/data/discriminant/gender_qda_continegency.csv")
write.csv(gender_nb_contingency, "src/data/discriminant/gender_nb_continegency.csv")

write.csv(position_lda_contingency, "src/data/discriminant/position_lda_continegency.csv")
write.csv(position_qda_contingency, "src/data/discriminant/position_qda_continegency.csv")
write.csv(position_nb_contingency, "src/data/discriminant/position_nb_continegency.csv")

write.csv(comparison_df, "src/data/discriminant/comparison.csv")
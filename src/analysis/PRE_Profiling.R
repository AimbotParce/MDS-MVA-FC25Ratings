library(corrplot)

load("~/Desktop/MVA PROJECT/MDS-MVA-FC25Ratings/src/data/cleansed_data.RData")

## CATEGORICAL VARIABLES (Target Overall Rating)

res.con <- condes(all_players[, c(3:45)], num.var=24)

## CORRELATION MATRIX

cor_matrix <- cor(all_players[, sapply(all_players, is.numeric)], use = "complete.obs")

cor_matrix[abs(cor_matrix) < 0.7] <- NA

print(cor_matrix)

corrplot(cor_matrix, method = "color", type = "upper", 
         order = "hclust",
         addgrid.col=TRUE)

# Boxplot to visualize `PAC` across different `Position` categories
library(ggplot2)
ggplot(all_players, aes(x = Position, y = PAC)) +
  geom_boxplot() +
  labs(title = "PAC Distribution by Position", x = "Position", y = "PAC") +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))

# Boxplot to visualize `Skill Moves` across `Weak Foot`
ggplot(all_players, aes(x = as.factor(Skill.moves), y = PAC, color = as.factor(Weak.foot))) +
  geom_boxplot() +
  labs(title = "PAC by Skill Moves and Weak Foot", x = "Skill Moves", y = "PAC") +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))


#
cat_df <- Filter(is.factor, all_players)
cat_df <- cat_df[,-c(6,7)]
res.cat <- catdes(cat_df, num.var=6)
res.cat













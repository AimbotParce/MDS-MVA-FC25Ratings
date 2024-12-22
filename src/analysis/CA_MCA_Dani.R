### CA ###

library(FactoMineR)
### Relation between Weak foot and Position

load("src/data/cleansed_data_scaled.RData")

#Check percentage of LW that prefer right foot
left_wingers <- subset(all_players, Position == "LW")
right_footed_lw_count <- sum(left_wingers["Preferred foot"] == "Right")
total_lw_count <- nrow(left_wingers)
percentage_right_footed_lw <- if (total_lw_count > 0) {
  (right_footed_lw_count / total_lw_count) * 100
} else {
  0
}
percentage_right_footed_lw

#Start CA
correspondance <- all_players[, c("Position", "Weak foot")]
contingency_table <- table(correspondance$`Position`, correspondance$`Weak foot`)
res.ca <- CA(contingency_table, col.sup=1)
#We took weak foot 1 as supplementary because players with a score of 1 were only outliers and had too much of an effect on CA
res.ca
res.ca$eig
res.ca$row
res.ca$col

# Low weak foot skill (Score 2): Strongly associated with defensive positions like GK and possibly CB.
# Intermediate skill (Score 3): Found in positions with balanced requirements, such as CM or RM.
# High weak foot skill (Scores 4 and 5): Associated with attacking positions like LW, RW, and CAM, where ambidexterity and weak foot proficiency are critical.

### MCA ###

library(FactoMineR)
load("src/data/cleansed_data_scaled.RData")
multipleca <- all_players[, c("OVR", "Position", "Sex","Preferred foot", "Weak foot")]
res.mca <-MCA(multipleca, quanti.sup=1)

summary(res.mca)
# K=12+2+2+5=21, J=4
# no of dimension: K-J=17
# average inertia per dimension: 1/(K-J)=1/17
# Choose the dimensions with eigenvalues > 1/21
#We choose the first 5 dimensions
summary(res.mca, ncp=5)

dimdesc(res.mca)

plot(res.mca,choix="var",title="Cloud of variables")
plot(res.mca,invisible=c("ind"),title="Graph of the active categories")
plot(res.mca,invisible=c("ind"),axes=c(2,3),title="Graph of the active categories")
plot(res.mca,invisible=c("ind"),axes=c(3,4),title="Graph of the active categories")




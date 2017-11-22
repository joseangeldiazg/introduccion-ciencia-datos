#Examle from
# STHDA
#Statistical tools for high-throughput data analysi

# ggplot
library(ggplot2)
library(ggpubr)
df <- ToothGrowth
df$dose <- as.factor(df$dose)
head(df)
p <- ggdensity(df, x = "len", fill = "dose",
palette = "jco",
ggtheme = theme_light(), legend = "top")
p
#Divide by the levels of the supp variable in
#the horizontal direction
facet(p, facet.by = "supp")

# Divide with "supp" vertical, "dose" horizontal
facet(p, facet.by = c("supp", "dose"),
short.panel.labs = FALSE)

# Divide with "supp" vertical, "dose" horizontal
facet(p, facet.by = c("supp", "dose"),
panel.labs = list(
supp = c("Orange Juice", "Vitamin C"),
dose = c("D0.5", "D1", "D2")
),
panel.labs.background = list(color = "steelblue", fill = "steelblue", size = 0.5),
panel.labs.font = list(color = "white"),
panel.labs.font.x = list(angle = 45, color = "white")
)
# Lattice
# Install
#install.packages("lattice")
# Load
library("lattice")
# Box plot
bwplot(len ~ supp | dose,  data = ToothGrowth,
layout = c(3, 1),
xlab = "Dose", ylab = "Length")
# Violin plot
bwplot(len ~ supp | dose,  data = ToothGrowth,
layout = c(3, 1), panel = panel.violin,
xlab = "Dose", ylab = "Length")
# Dot plot
dotplot(len ~ supp | dose,  data = ToothGrowth,
layout = c(3, 1),
xlab = "Dose", ylab = "Length")
# Strip plot
stripplot(len ~ supp | dose,  data = ToothGrowth,
layout = c(3, 1), jitter.data = TRUE,
xlab = "Dose", ylab = "Length")

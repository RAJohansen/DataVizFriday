library(ggplot2)

H <- read.csv("C:/DataVizFriday/New Student Orientation/Heights.csv")
H$Meters <- as.numeric(H$Meters)
ggplot(H) + geom_col(aes(Objects,Meters))

ggplot(H, aes(Objects,Meters)) + geom_col(aes(x=reorder(Objects,-Meters), Meters, fill = Objects)) + 
  geom_text(aes(label=Meters), vjust=-0.25) +
  labs(title= "Object Heights\nCompared to Aggregate\nof UC's 2018 Incoming Class", x = "", y = "Height\n(meters)") +
  theme_classic() +
  scale_fill_manual(values=c("lightgrey","lightgrey","lightgrey","lightgrey",
                             "lightgrey","lightgrey","lightgrey","lightgrey",
                             "lightgrey","red"), guide=FALSE) +
  theme(axis.text.x = element_text(angle = 90, hjust = 1),
        axis.title.y = element_text(angle = 0, vjust = 0.5),
        plot.title = element_text(size = 20, face = "bold", hjust = 0.5))


W <- read.csv("C:/DataVizFriday/New Student Orientation/Weights.csv")
W$Weight <- as.numeric(W$Weight)

ggplot(W, aes(Objects, Weight)) + geom_col(aes(x=reorder(Objects,-Weight), Weight, fill = Objects)) + 
  geom_text(aes(label=Weight), vjust=-0.25) +
  labs(title= "Object Weights\nCompared to Aggregate\nof UC's 2018 Incoming Class", x = "", y = "Weight\n(pounds)") +
  theme_classic() +
  scale_fill_manual(values=c("lightgrey","lightgrey","lightgrey","lightgrey","red"), guide=FALSE) +
  theme(axis.text.x = element_text(angle = 90, hjust = 1),
        axis.title.y = element_text(angle = 0, vjust = 0.5),
        plot.title = element_text(size = 20, face = "bold", hjust = 0.5))



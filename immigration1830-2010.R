  library(ggplot2)
  library(ggjoy)

# Data downloaded from the Yearbook of Immigration Statistics
  immrates <- read.csv("immigration200years.csv")

# Aggregate origins to regions
  immrates$share <- as.numeric(sub("%", "", immrates$share))
  immrates$region <- NA
  immrates$region[which(immrates$key %in% levels(immrates$key)[c(8,10,13,20)]==TRUE)] <- "Northern/Western\nEurope"
  immrates$region[which(immrates$key %in% levels(immrates$key)[c(1,11,16,18)]==TRUE)] <- "Southern/Eastern\nEurope"
  immrates$region[which(immrates$key %in% levels(immrates$key)[c(5,12,19)]==TRUE)] <- "Central/South\nAmerica"
  immrates$region[which(immrates$key %in% levels(immrates$key)[c(9,15,17,6)]==TRUE)] <- "Asia"
  immrates$region[which(immrates$key %in% levels(immrates$key)[c(2,4)]==TRUE)] <- "Africa and Carribean"
  immrates$region[which(immrates$key %in% levels(immrates$key)[c(3,7,14)]==TRUE)] <- "Other"

  
  immrates2 <- aggregate(value~date+region, data=immrates, FUN=function(x) sum=sum(x))
  immrates2$region <- factor(immrates2$region, levels=c("Central/South\nAmerica", "Asia", "Africa and Carribean", "Other", "Southern/Eastern\nEurope", "Northern/Western\nEurope"))

# Plot using ggjoy
  ggplot(data=immrates2, aes(x=date, y=region, height=value/1000000, group=region, color=region, fill=region)) + 
    geom_joy(stat="identity") + theme_joy() + 
    ggtitle("Immigration to the United States, 1830-2010") +
    theme(legend.position='none', axis.text.x=element_text(angle = 0, vjust = .5), legend.text=element_text(), axis.title.y=element_blank(), axis.title.x=element_blank()) + 
    guides(fill = guide_legend(reverse=FALSE)) + 
    #scale_fill_manual(values=c("#8B0000", "#FF0000", "#FFA500", "#999999", "#56B4E9", "#0072B2")) + scale_colour_manual(values=c("#8B0000", "#FF0000", "#FFA500", "#999999", "#56B4E9", "#0072B2")) +
    scale_x_continuous(limits=c(1829,2010), expand = c(0, 0))

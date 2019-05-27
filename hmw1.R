#Load the appropriate libraries
library(purrr)
library(ggplot2)
library(cowplot)

#This is the graphing function
gguc <- function(x=1,y=1){
  ggplot(data = iris, aes_string(x = iris[[x]], y = iris[[y]], color = iris$Species))+
    geom_point()+
    geom_density_2d()+
    xlab(x)+
    ylab(y)+
    scale_color_manual(values=c("#CC6666", "#9999CC", "#66CC99"))+
    theme_classic()+
    theme(axis.text = element_text(size = 10), 
          axis.title = element_text(size = 14),
          legend.position = "bottom",
          legend.title = element_blank(),
          legend.text = element_text(size = 18))
}

#This creates names from the original dataframe that can be used to assign unique varible names
#If you want to use different variables, you can alter the index to set the names
dat1 <- names(iris)[1:4]
dat1 <- set_names(dat1)

#This iterates the graphing function throughout the dataframe based on the set names
all_plots2 = map(dat1,
                 ~map(dat1, gguc, y = .x))

#This arranges the graph and adds the labels
labs <- c("A", "B", "C", "D", "E", "F", "G", "H", "I", "J", "K", "L", "M", "N", "O", "P")
thing <- plot_grid(all_plots2[[1]][[1]]+theme(legend.position = "none"),
                   all_plots2[[1]][[2]]+theme(legend.position = "none"),
                   all_plots2[[1]][[3]]+theme(legend.position = "none"),
                   all_plots2[[1]][[4]]+theme(legend.position = "none"),
                   all_plots2[[2]][[1]]+theme(legend.position = "none"),
                   all_plots2[[2]][[2]]+theme(legend.position = "none"),
                   all_plots2[[2]][[3]]+theme(legend.position = "none"),
                   all_plots2[[2]][[4]]+theme(legend.position = "none"),
                   all_plots2[[3]][[1]]+theme(legend.position = "none"),
                   all_plots2[[3]][[2]]+theme(legend.position = "none"),
                   all_plots2[[3]][[3]]+theme(legend.position = "none"),
                   all_plots2[[3]][[4]]+theme(legend.position = "none"),
                   all_plots2[[4]][[1]]+theme(legend.position = "none"),
                   all_plots2[[4]][[2]]+theme(legend.position = "none"),
                   all_plots2[[4]][[3]]+theme(legend.position = "none"),
                   all_plots2[[4]][[4]]+theme(legend.position = "none"),
                   nrow = 4, ncol = 4,
                   labels = labs)

#This adds the common legend to the bottom of the graph
legnd <- get_legend(all_plots2[[1]][[1]])
things <- plot_grid(thing, legnd, ncol = 1, rel_heights = c(1, 0.05))

#This saves the image as a 'svg'
svg("hmw1.svg", height = 11, width = 8.5)
print(things)
dev.off()

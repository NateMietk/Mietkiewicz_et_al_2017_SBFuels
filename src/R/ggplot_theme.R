# GGPLOT Theme ------------------------------------------------------------
theme_pub <- function(base_size=11, base_family="Times New Roman") {
  library(grid)
  library(ggthemes)
  (theme_foundation(base_size=base_size, base_family=base_family)
    + theme(plot.title = element_text(hjust = 0.05, size = 13),
            
            panel.border = element_rect(colour = "black", size = 0.55),
            panel.background = element_rect(colour = NA),
            panel.grid.major = element_line(colour="#f0f0f0"),
            panel.grid.minor = element_blank(),
            plot.background = element_rect(colour = NA),
            
            axis.line = element_line(colour="black"),
            axis.ticks = element_line(),
            
            legend.title = element_text(size=11),
            legend.position = "right",
            legend.text = element_text(size=11),
            legend.direction = "vertical",
            legend.key = element_rect(colour = "transparent", fill = "white"),
            
            strip.background=element_rect(colour=NA),
            strip.text.x = element_text(size = 10),
            
            axis.title = element_text(size = 11),
            axis.text.x = element_text(size = 11, hjust = 1),
            axis.text.y = element_text(size = 11)))
}

# Create generic ggplot function as a wrapper
ggfunction <- function(d, i, j, ermax, a, text){
  p <- ggplot(d, aes_string(x = "Year", y = i, fill = "Age")) +
    geom_bar(colour="black", 
             stat = "identity", position = position_dodge()) +
    geom_errorbar(aes_string(ymax = ermax, ymin = ermax),
                  position = position_dodge(.9), width=0.35, size=0.3) +
    geom_linerange(aes_string(ymin = i, ymax = ermax), 
                   position = position_dodge(.9)) +
    theme_pub() + 
    theme(axis.text.x = element_blank(),
          legend.position="none") +
    scale_fill_manual(values=c("Old" = "gray90", "Young" = "gray23"))
  if(text == "TRUE"){
    y1 <- ggplot_build(p)$data[[1]]$ymax
    y2 <- ggplot_build(p)$data[[2]]$ymax
    y1[is.na(y1)] <- 0
    y2[is.na(y2)] <- 0
    ymax <- ifelse(max(y1) > max(y2), max(y1), max(y2))
    p <- p + geom_text(aes(x = 1, y = ymax, 
                           hjust = 0.95, vjust = 0.95, label=paste0("(", a, ")")), 
                       check_overlap = TRUE, stat = 'identity')}
  p
}

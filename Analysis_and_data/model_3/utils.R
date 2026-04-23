tabulateDescriptives <- function(data, rowNames){
  df <- data.frame(rows = rowNames,
                   Mean = c(mean(data)),
                   SD = c(sd(data)),
                   Max = c(max(data)),
                   Min = c(min(data)))
  knitr::kable(df)
}

data_summary <- function(data, varname, groupnames){

  summary_func <- function(x, col){
    c(mean = mean(x[[col]], na.rm=TRUE),
      sd = sd(x[[col]], na.rm=TRUE))
  }
  data_sum<-ddply(data, groupnames, .fun=summary_func,
                  varname)
  data_sum <- rename(data_sum, c("mean" = varname))
  return(data_sum)
}

errorbar <- function(data_summarised, x, y, title){
  plt <- ggplot(data = data_summarised,
                mapping = aes(x = x,
                              y = y))+
    geom_errorbar(aes(ymin=y-sd, ymax=y+sd),  # errorbar is +/- 1 SD
                  width=0.1, 
                  position=position_dodge(0.1)) +
    geom_line(position=position_dodge(0.1), group=1)+
    geom_point(position=position_dodge(0.1))+
    theme_classic() +
    theme(legend.position = "bottom",
          #legend.position.inside = c(0.5,1),
          #legend.justification.inside = c(1,1),
          legend.direction = "horizontal",
          #legend.position = "bottom",
          plot.title = element_text(margin = margin(b=15)),
          axis.text = element_text(size = 12),
          axis.title = element_text(size = 12, face = "bold"),
          axis.title.y = element_text(margin = margin(r=13)))+
    scale_x_discrete(expand = expansion(0.2, 0))
  
  return(plt)
}

minMaxScale <- function(
    dat
){
  return((dat - min(dat)) / (max(dat)-min(dat)))
}

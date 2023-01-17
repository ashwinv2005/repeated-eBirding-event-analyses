require(tidyverse)
require(stringr)
require(ggthemes)

a = read.csv("timelist.csv")
b = str_split_fixed(a$Checklists," ",2)
c = str_split_fixed(b[,1],":",2)
c1 = as.numeric(c[,1])
c2 = as.numeric(c[,2])
d = c1*60 + c2
e = data.frame(time = d)

is.extrafont.installed <- function(){
  if(is.element("extrafont", installed.packages()[,1])){
    library(extrafont)
    # probably need something here to run font_import()
    return(T)
  }else{
    warning("Library extrafont installed; using system sans/serif libraries as fallback fonts. 
    To enable full font support, run: 
      install.packages('extrafont') 
      font_import()")
    return(F)
  }
}

base_font_family_tufte <- function(){
  if(is.extrafont.installed()){
    library(extrafont)
    tuftefont <- choose_font(c("Gill Sans MT", "Gill Sans", "GillSans", "Verdana", "serif"), quiet = FALSE)  
  }else{
    tuftefont <- "serif"
  }
  return(tuftefont)
}

theme_tufte_revised <- function(base_size = 11, base_family = base_font_family_tufte(), ticks = TRUE) {
  
  ret <- theme_bw(base_family = base_family, base_size = base_size) + 
    theme(
      axis.line = element_line(color = 'black'),
      axis.title.x = element_text(vjust = -0.3), 
      axis.title.y = element_text(vjust = 0.8),
      legend.background = element_blank(), 
      legend.key = element_blank(), 
      legend.title = element_text(face="plain"),
      panel.background = element_blank(), 
      panel.border = element_blank(),
      panel.grid = element_blank(),
      plot.background = element_blank(),
      strip.background = element_blank()
    )
  
  if (!ticks) {
    ret <- ret + theme(axis.ticks = element_blank())
  }
  
  ret
} 

require(extrafont)

ggp = ggplot(data = e, aes(x = time)) +
  geom_histogram(binwidth = 15, fill = NA, col = "black") +
  xlab("Time of Day (AM)") +
  ylab("Number of Checklists")+
  theme_tufte_revised()

ggp3 = ggp +
  theme(axis.title.x = element_text(size = 16), axis.text.x = element_text(size = 12),
        axis.title.y = element_text(size = 16), axis.text.y = element_text(size = 14)) +
  theme(text=element_text(family="Gill Sans MT")) +
  scale_x_continuous(breaks = c(360,375,390,405,420,435,450,465,480,495,510,525,540,555,570,585),
                     labels = c("6:00","6:15","6:30","6:45","7:00","7:15","7:30",
                                "7:45","8:00","8:15","8:30","8:45","9:00","9:15","9:30",
                                "9:45"))

png('CBEatlas_time.png', units="in", width=10, height=7, res=1000)
ggp3
dev.off()

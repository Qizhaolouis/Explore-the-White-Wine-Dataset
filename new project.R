library(tidyverse)
library(data.table)
library(ggplot2)
library(RColorBrewer)
library(rbokeh)
wine <- read.csv("wineQualityWhites.csv", row.names = 1)
names <- colnames(wine)
dim(wine)

####Univariate Analysis
##summary
library(gridExtra)
library(grid)
#summary table
library(ggpubr)

###summary
stats <- c("min","median","mean","max","var")
summary <- list(variable = names)
for (i in 1:length(stats)){
  summary[[stats[i]]] <- apply(wine, 2, eval(stats[i])) %>% round(.,2)
}
table <- data.frame(summary) %>% 
         ggtexttable(rows = NULL, 
                     theme = ttheme("mBlue")) %>% print


###


########Histgrams
# Draw plot
histgrams=list()
histcolor=c("BuPu","GnBu","Greens","Oranges","OrRd","PuBu","PuBuGn","PuRd","RdPu","YlGn","YlGnBu","YlOrRd")
linecolor=c("blue","green","green2","orange","red","purple","skyblue","red","purple4","yellow","green","red")
for (i in 1:ncol(wine)){
  histgrams[[names[i]]] <- figure(title = names[i], width = 600, height = 400) %>%
            ly_hist(wine[,i], data = wine, 
                    color= brewer.pal(10,histcolor[i]), 
                    breaks = 40, freq = FALSE) %>%
            ly_density(wine[,i], data = wine,
                       color= linecolor[i])
}
#####observe
histgrams$fixed.acidity
histgrams$volatile.acidity
histgrams$citric.acid
histgrams$residual.sugar
histgrams$chlorides
histgrams$free.sulfur.dioxide
histgrams$total.sulfur.dioxide
histgrams$density
histgrams$pH
histgrams$sulphates
histgrams$alcohol
histgrams$quality

####outliers (except quality)
library(outliers)
wine1 <- wine[,-12]
subwine <- as.list(wine1) %>% 
  lapply(rm.outlier)
###count
lapply(subwine, length) %>% 
  unlist %>% 
  data.frame(name=names[1:11],count=.) %>% 
  ggtexttable(rows = NULL, 
              theme = ttheme("mBlue")) %>% print
######
ol<- as.list(wine1) %>% 
  lapply(outlier) %>% 
  lapply(as.numeric)
outliers <- list()
for (i in 1:11){
  outliers[[names[i]]] <- list(which(wine1[,i]==ol[i]))
}
outindex <- outliers %>% unlist %>% 
  as.numeric %>% .[!duplicated(.)] 
olpoints <- wine[outindex,]
olpoints
###new hist
subhist=list()
for (i in 1:12){
  points=unlist(subwine[1]) %>% as.numeric %>% c(.)
  subhist[[names[i]]] <- figure(title = names[i], width = 600, height = 400) %>%
    ly_hist(points,  
            color = brewer.pal(10,histcolor[i]), 
            breaks = 40, freq = FALSE) %>%
    ly_density(points,
               color= linecolor[i])
}
subhist$chlorides

#### What is the structure of your dataset?
#### What is/are the main feature(s) of interest in your dataset? 
#What other features in the dataset will help support your investigation into your feature(s) of interest?
#Did you create any new variables from existing variables in the dataset?
#Of the features you investigated, were there any unusual distributions? 
#Did you perform any operations on the data to tidy, adjust, or change the form of the data? 
#If so, why did you do this?
#I log-transformed the right skewed price and volume distributions. 


############remove outliers
wine_new <- wine[-outindex,]






###Bivariate Plots Section

##cor plot
library(ggcorrplot)
corr <- round(cor(wine_new), 1)
ggcorrplot(corr, hc.order = TRUE, 
           type = "lower", 
           lab = TRUE, 
           lab_col = "chocolate4",
           outline.color = "chocolate",
           colors = c("royalblue4", "white", "steelblue4"),
           lab_size = 3, 
           method="square", 
           show.legend = TRUE, legend.title = "correlation", 
           title="Correlarion Plot", 
           ggtheme=theme_bw)



#####free sulfur dioxide & total
x <- free.sulfur.dioxide
y <- total.sulfur.dioxide
idx <- split(1:nrow(wine_new), wine_new$quality)
figs <- lapply(idx, function(x) {
  figure(width = 250, height = 250, title = names(idx)) %>%
    ly_points(x, y, data = wine_new,
              hover = list(free.sulfur.dioxide, total.sulfur.dioxide))})
  
grid_plot(figs,nrow=3)


 
#####feature selection






#### pairs
wine$level <- as.factor(wine$quality)
library(GGally)
pair<-ggpairs(data=wine, # data.frame with variables
             columns=1:4, # columns to plot, default to all.
             title="White Wine", # title of the plot
             ggplot2::aes(colour=level),legend=c(1,1)) # aesthetics, ggplot2 style
print(pair)

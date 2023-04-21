#### libraries imported #### 
library(ggplot2)
library(vcd)
library(dplyr)
library(tidyr)
library(ggpie)
library(treemapify)
# bar chart 


# categorical visualization 

#### bar plot of the improvement rates  ####
ggplot(mtcars,aes(x=mtcars$cyl))+geom_bar()
data("Arthritis",package='vcd')
table(Arthritis$Improved)
Arthritis
ggplot(Arthritis,aes(x=Improved,color=Improved))+
              geom_bar()+ 
              labs(title = 'Arthritis data analysis',
                   subtitle = 'Improvement rates ',
                   x='Improvemnt',
                   y='Frequancy rates',
                   color='Improved')+
              coord_flip()+theme_bw()
# table(Arthritis$Improved,Arthritis$Treatment)
#### bar plot of the cross table of the treatment methods over the improvement rates####
ggplot(data=Arthritis, aes(x=Treatment, fill=Improved))+
                       geom_bar(position = 'stack')+# position can be stack,fill,dodge
                       coord_flip()+
                       labs(title='Rates analysis of Arthritis',
                             subtitle='imporvment rates across different treamtments ',
                             x='Frequancy rates',
                             y='Treatmnet'  )+
                      theme_bw()

#### plotting a bar plot of the mean of illiteracy in each region####
states<- data.frame(state.region, state.x77)
dim(states)
mean_illit<-states%>%
     group_by(state.region)%>%
     summarise(mean=mean(Illiteracy))
ggplot(mean_illit,aes(x=reorder(state.region, mean),y=,mean))+
  geom_bar(stat = 'identity')+
  labs(title = 'Mean of Illiteracy in each region',
       x='Regions',
       y='Mean of Illiteracy')

#### bar chart (plot) of the rates of illiteracy in the regions with standard error ####
data_plot<-group_by(states,state.region)%>%
           summarise(n=n(),mean=mean(Illiteracy),
                     se=(sd(Illiteracy))/sqrt(n))
ggplot(data_plot,aes(x=state.region,y=mean))+
                 geom_bar(stat = 'identity', fill='cornflowerblue',color='skyblue')+
                 geom_errorbar(aes(ymin=mean-se,ymax=mean+se),width=0.115,color='red')
states$Illiteracy                  
#### vector colors to indicate different variables ####
ggplot(Arthritis,aes(x=Treatment, fill= Improved), alpha=0.3)+ 
  geom_bar(position = 'fill')+ 
  scale_fill_manual(values = c('skyblue','cornflowerblue','navyblue'))+
  theme_get()+
  coord_flip()
#### dealing with labels overlap ####
ggplot(mpg,aes(x=model))+
  geom_bar()+
  labs(title = 'Car models in the mpg dataset')+
  theme(axis.text.x = element_text(angle = 30,hjust =0.9,size=6))
# or 
ggplot(mpg,aes(x=model))+
  geom_bar()+
  coord_flip()
#### simple pie chart  #####
# pie charts
ggpie(mpg,group_key =  'class', count_type = 'full')
ggpie(mpg, group_key='class',
      label='year',
      count_type = 'count',
      label_type = 'horizon', 
      label_info = 'ratio',
      label_pos = 'out',
      label_split = NULL,
      label_threshold = 5,title='car calss by year')
     





#### tree maps  ####
  #simple tree map
mpg_manufact<-mpg %>% count(manufacturer,drv)
mpg_manufact$drv<- factor(mpg_manufact$drv, levels = c('4','f','r'),labels= c('4-wheeled','front-wheeled','rare-wheeled'))
ggplot(mpg_manufact, aes(fill=manufacturer, area=n, label= manufacturer, subgroup=drv))+
  geom_treemap()+
  geom_treemap_subgroup_border()+
  theme(legend.position = 'none')+
  geom_treemap_subgroup_text(color='black',place = 'middle',grow=FALSE,alpha=0.4,size=12)+
  geom_treemap_text(color= ' white',place = 'centre',grow=FALSE,size=5)
  


c<- mpg%>%count(manufacturer,drv)
c$drv<- factor(c$drv,levels = c('4','r','f'), labels = c('4-Wheel','front','rare'))
                                          
ggplot(c,aes(fill=manufacturer, area=n,label=manufacturer, subgroup=drv))+
  geom_treemap()+
  geom_treemap_subgroup_border()+
  theme(legend.position = 'none')+
  geom_treemap_text(color='black',grow = FALSE)+
  geom_treemap_subgroup_text(color='navyblue',grow = FALSE,alpha=0.5,size=35,place = 'middle')








# continuous visualization 

#### histograms  ####
ggplot(mpg,aes(x=hwy))+geom_histogram()
car2008<-mpg[mpg$year==2008,]

ggplot(car2008,aes(x=cty))+
  geom_histogram(binwidth = 1.2)+
  labs(title = 'cities distrbution for year of 2008')

ggplot(car2008,aes(x=hwy))+
  geom_histogram(binwidth = 1.2,color='navyblue',fill='steelblue')+
  labs(title = 'city distrbution per gallon',
       x='gallons',
       y='frequancy')

ggplot(car2008, aes(x=hwy,y=after_stat(density)))+
  geom_histogram(bins=20, color='steelblue', fill='skyblue')+
  scale_y_continuous(labels = scales::percent)+
  geom_density(color='navyblue')+
  theme_minimal()
  










#### density functions  ####

bw.nrd0(car2008$cty) #is used to determine the suitable band width for a varaible

#simple density 
ggplot(car2008,aes(x=hwy, y = after_stat(density)))+
  geom_density(fill='black', alpha=0.4,bw=2.0305)+
  scale_fill_grey()

# to group by a vector the type of the vector must be factor not a continuous variable
car2008$cyl<-factor(car2008$cyl)

# a grouped density function
ggplot(car2008,aes(x=cty,fill=cyl, color=cyl))+
  geom_density(size=1,alpha=0.3)+
  theme(legend.position = 'None')+
  scale_fill_grey()+# both are used to print grey color that can be distinguished for printing
  scale_color_grey()




#### box plots####
 
ggplot(mtcars, aes(x='',y=mpg))+
  geom_boxplot()
boxplot.stats(mtcars$mpg)#5numbers summary of a box plot 

mpg$year<-factor(mpg$year)
mpg$cyl<-factor(mpg$cyl)

ggplot(mpg,aes(x=cyl,y=cty, fill=year))+
  geom_boxplot(notch = TRUE,varwidth=TRUE)+# varwidth width of box  sqrt of n of each variable
  theme_get()

#### violin plots ####
ggplot(mpg,aes(x=cyl,y=cty))+
  geom_boxplot(width=0.2,fill='yellow',color='brown')+
  geom_violin( alpha=0.3,fill='gold')+
  theme_get()



#### dot plots  ####
plyed08<-mpg%>%filter(year==2008)%>%group_by(model)%>%summarise(mean_hwy=mean(hwy))
plyed99<-mpg%>%filter(year==1999)%>%group_by(model)%>%summarise(mean_hwy=mean(hwy))
ggplot(plyed08, aes(x=mean_hwy,y=model))+
         geom_point(  )+
         theme(axis.text.y = element_text(angle=20,hjust =1,size=4))
  

plotdata <- mpg %>%
  filter(year == "2008") %>%
  group_by(model) %>%
  summarize(meanHwy=mean(hwy))
ggplot(plotdata, aes(x=meanHwy, y=model)) +
  geom_point() +
  labs(x="Miles Per Gallon",
       y="",
       title="Gas Mileage for Car Models")



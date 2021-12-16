##### Class: Text Analytics and Natural Language Processing 
##### Karley Webster
##### MBAN2 HULT 2021
##### Correlograms
##### Version 0.1

library(scales)
ggplot(frequency, aes(x=proportion, y=`United States`, #United States is the benchmark, we will compare the other countries to the other ones
                      color = abs(`United States`- proportion)))+
  geom_abline(color="red4", lty=2)+ #adding the division for the correlation, colors() provides the pallet of colors available. lyt: references to the linetype 0: blank, 1: solid, 2: dashed, 3: Doted, 4: dotdash, 5; longdash, 6: twodash
  geom_jitter(alpha=.1, size=2.5, width=0.3, height=0.3)+ #alpha gives the transparency, jitter gives the scatter plot with a bigger distribution than geom_point
  geom_text(aes(label=word), check_overlap = TRUE, vjust=1.5) + #adding the labels of the data
  scale_x_log10(labels = percent_format())+ #Transforming the x axis to a logarithmic scale of 10
  scale_y_log10(labels= percent_format())+ #Transforming the y axis to a logarithmic scale of 10
  scale_color_gradient(limits = c(0,0.001), low = "darkslategray4", high = "gray75")+ #adding the gradiant for the data, it needs a low and high value. If we want a mid value we should use scale_color_gradient2(Low="xx", high="xx", mid ="xx")
  facet_wrap(~author, ncol=2)+ #Creating a breakdown of charts by author. If we want each one to have there own scale: face_wrap(~variable, scales ="free y"), and if we want it to start at zero: expand_limits(y=0)
  theme(legend.position = "none")+ #creating a theme for the graph
  ggtitle("Words distribution per author (Country)") + #Creating the title of the graph
  labs(y= "United States", x=NULL) #adding the y label, x label is nul becaus it will depend on the country

#Business insights: 
#The words next to the line appear in both countries 
#The words that are away from the diagonal are only seen in that country
#

#09 let's plot the correlograms
library(scales)
ggplot(frequency, aes(x=proportion, y=`United States`, #United States is the benchmark, we will compare the other countries to the other ones
                      color = abs(`United States`- proportion)))+
  geom_smooth(formula = y ~ x, method = "lm", se = T, level = 0.95) #Creating the regression line: geom_smooth(method: (NULL/lm/glm/gam/loess), formula: (y ~ x / y ~ poly(x, 2) / y ~ log(x) / NULL), se: (T/F) interval confidence, level: 0.95 confidence level)

help("geom_smooth")


#10 Doing the cor.test()
#Test for association between paired samples, using one of Pearson's product moment correlation coefficient

cor.test(data=frequency[frequency$author == "Brazil",], #Comparing data from USA to Brazil
         ~proportion + `United States`) #cor 0.6594891 Moderate

cor.test(data=frequency[frequency$author == "India",], #Comparing data from USA to India
         ~proportion + `United States`) #cor 0.6593853 Moderate

#11 DTM

netflix_dtm <- netflix %>% #Creating a dtm
  unnest_tokens(word, text) %>% #tokenization 
  count(title, word) %>% #counting the times the word appears
  cast_dtm(title, word, n) #This turns a "tidy" one-term-per-document-per-row data frame into a DocumentTermMatrix 
netflix_dtm #sparsity 100%, the closest to 100% is the better




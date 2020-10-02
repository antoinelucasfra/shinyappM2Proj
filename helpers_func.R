
### MAP FUNCTIONS ###

# function to plot suicide by year

#country,age,sex,generations,total_suicide
iris %>% filter(Species == "virginica")

#function to plot for a specified sex

sex_plot <- function(df,sex){
  
  g <- df %>% filter(Sex == sex)
  ggplot(aes(x=date,y=suicide, color = Sex)) + geom_line() +
    ylab("Amount of suicide per year") + 
    theme_bw() 
  
  return(g)
}

#function to plot for a specified Age class

sex_plot <- function(df,age_class){
  
  g <- df %>% filter(Age == age_class)
  ggplot(aes(x=date,y=suicide, color = Age)) + geom_line() +
    ylab("Amount of suicide per year") + 
    theme_bw() 
  
  return(g)
}

#function to plot for a specified generation

sex_plot <- function(df,generation){
  
  g <- df %>% filter(Generation == generation)
  ggplot(aes(x=date,y=suicide, color = Generation)) + geom_line() +
    ylab("Amount of suicide per year") + 
    theme_bw() 
  
  return(g)
}

variable_plot <- function(suicide,input_pick = c("sex","age","generation")){
  
  if (input_pick == "sex"){
    
    g <- suicide %>% group_by(country,year,Capital.Major.City,Latitude,Longitude) %>% 
      summarise(total_suicide = sum(suicides_no), pop_sum = sum(population)) %>% 
      ggplot(aes(x=year,y=total_suicide,color=input$sex))
    
  }
  
  
}


# function to plot cumulative cases by region
country_cases_cumulative = function(cv_cases, input_start=c("sex", "age", "generation"), plot_start_date) {
  if (start_point=="sex") {
    g = ggplot(cv_cases, aes(x = date, y = outcome, colour = region, group = 1,
                             text = paste0(format(date, "%d %B %Y"), "\n", region, ": ",outcome))) +
      xlim(c(plot_start_date,(current_date+1))) + xlab("Date")
  }
  
  if (start_point=="Day of 100th confirmed case") {
    cv_cases = subset(cv_cases, days_since_case100>0)
    g = ggplot(cv_cases, aes(x = days_since_case100, y = outcome, colour = region, group = 1,
                             text = paste0("Day ", days_since_case100,"\n", region, ": ",outcome))) +
      xlab("Days since 100th confirmed case")
  }
  
  if (start_point=="Day of 10th death") {
    cv_cases = subset(cv_cases, days_since_death10>0)
    g = ggplot(cv_cases, aes(x = days_since_death10, y = outcome, colour = region, group = 1,
                             text = paste0("Day ", days_since_death10,"\n", region, ": ",outcome))) +
      xlab("Days since 10th death")
  }
  
  g1 = g + geom_line(alpha=0.8) + geom_point(size = 1, alpha = 0.8) +
    ylab("cumulative") + theme_bw() + 
    scale_colour_manual(values=country_cols) +
    theme(legend.title = element_blank(), legend.position = "", plot.title = element_text(size=10))
  ggplotly(g1, tooltip = c("text")) %>% layout(legend = list(font = list(size=11)))
}









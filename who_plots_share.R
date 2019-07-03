##########################################################
# Code to plot outputs for WHO-MR-ELIMINATION runs
# Author: Petra Klepac, LSHTM
##########################################################


# load necessary packages
library(ggplot2)
library(dplyr)
library(reshape2)
library(data.table)
library(ggforce)
library(viridis)

# set up folder names
rname = path.expand("~/Documents/Projects/VIMC/measles-dynamice/R/")
fname = paste0(rname, "WHO-MR/figures/") # figures go here

# scenarios
scenario.name = c("Basecase",           # 1
                  "ContinuingTrends",    # 2  
                  "GaviOptimistic",      # 3
                  "IntensifiedInvestments") # 4 

# load the simulations and put them in the same data.table
for (j in 1:length(scenario.name)){
  scenario.j <- scenario.name[j]
  infile <- paste0("who_burden_", scenario, ".csv")
  temp <- fread(paste0(rname, "WHO-MR/burden/", infile),
                stringsAsFactors = F, check.names = F)
  temp[, scenario := scenario.j]
  if (j ==1) who.all <- temp else who.all <- rbind(who.all, temp) # don't use rbind in a long loop
}

# aggregate results for easier plotting
age.breaks <- c(-1,5,10,40,120) # set limits for different age groups
age.labels <- c("<5", "5-10", "10-40", ">40") # set labels
# add new column age2 with age groups
who.all[, age2 := cut(age, 
                breaks = age.breaks, 
                labels = age.labels)]
# create new data.table with aggregated ages
who.all2 <- who.all[, .(cases = sum(cases), 
                        deaths = sum(deaths), 
                        dalys = sum(dalys)),
                    by = .(country, scenario, year, age2)]
# create new data.table that aggregates by year
who.totals <- who.all[, .(cases = sum(cases), 
                          deaths = sum(deaths), 
                          dalys = sum(dalys)),
                    by = .(country, scenario, year)]


# do some plots

plotwhat <- c("deaths", "cases", "dalys")

# quick plot - could wrap this into a function if needed later
for (j in 1:length(scenario.name)){
  scenario.j <- scenario.name[j]
  for (i in 1:length(plotwhat)){
    toplot = plotwhat[i]
    p <- ggplot(who.all2[scenario == scenario.j,], 
                aes_string(x = "year", y = toplot)) +
      geom_line(aes( col=age2, group = interaction(age2, country))) +
      facet_grid(country ~ .) +
      theme_bw(base_size = 6) +
      labs(x="calendar year",    y=toplot) +
    #  theme(legend.position = "none") + 
      scale_color_viridis(name = "", discrete = T)
    # plot file 
    pdf(file =  paste0(fname, scenario.j, "_", toplot, "_agg-ages_legend.pdf"))
    # split pdf into pages
    for (k in 1:5) {
      print (p + facet_wrap_paginate(.~country,scales = "free_y", nrow=4, ncol=5, page=k))
    }
    dev.off()
  }
}


for (j in 1:length(scenario.name)){
  scenario.j <- scenario.name[j]
  
  for (i in 1:length(plotwhat)){
    toplot = plotwhat[i]
    p <- ggplot(subset(who.totals, scenario==scenario.j), aes_string(x = "year", y = toplot)) +
      geom_line(aes( group = interaction(scenario, country))) +
      facet_grid(country ~ .) +
      theme_bw(base_size = 6) +
      labs(x="calendar year",    y=paste0("total ", toplot)) +
      theme(legend.position = "none") 
      pdf(file =  paste0(fname,scenario.j, "_total_", toplot, ".pdf"))
    for (k in 1:6) {
      print (p + facet_wrap_paginate(.~country,scales = "free_y", nrow=4, ncol=4, page=k))
    }
    dev.off()
  }
}



# also plot coverage 

for (j in 1:length(scenario.name)){
  print(j)
  scenario = scenario.name[j]

  data_coverage_routine 	<- paste0("WHO/coverage_routine_", scenario, ".csv")
  data_coverage_sia 		<- paste0("WHO/coverage_sia_", scenario, ".csv" )
  coverage_routine	<- fread(paste0(rname,"input/",data_coverage_routine), 
                            stringsAsFactors = F, check.names = F)
  coverage_sia		  <- fread(paste0(rname, "input/",data_coverage_sia), 
                           stringsAsFactors = F, check.names = F)
  coverage_sia$vaccine <- NULL
  coverage_sia[, c("age_first", "age_last") := list(a0/52, a1/52)]
  if(j ==4){ # there was a problem for this scenario - coverage column was called "0" -  rename
    coverage_routine[, coverage := `0`]
    coverage_sia[, coverage := `0`]
  }
  
  pdf(file =  paste0(fname,"who_coverage_routine", scenario, ".pdf"))
  p <- ggplot(coverage_routine, aes_string(x = "year")) +
    geom_line (aes(y = coverage, col=vaccine, group = interaction("vaccine", "age_first", "country_code"))) +
    # facet_grid(country ~ .) +
    theme_bw(base_size = 8) +
    labs (
      x="calendar year",
      y="vaccine coverage") +
    theme(legend.position = "none") 
  
  for (k in 1:5) {
    print (p + facet_wrap_paginate(.~country_code, nrow=4, ncol=5, page=k))
  }
  dev.off ()
  
  pdf(file =  paste0(fname,"who_coverage_sia_", scenario, ".pdf"))
  p <- ggplot(coverage_sia, aes_string(x = "year")) +
    geom_point(aes(y = coverage, col=age_first, group = interaction("country_code", "age_first"))) +
    # facet_grid(country ~ .) +
    theme_bw(base_size = 6) +
    labs (
      x="calendar year",
      y="vaccine coverage") +
    theme(legend.position = "none") 
  
  for (k in 1:5) {
    print (p + facet_wrap_paginate(.~country_code, scales = "free_y",nrow=4, ncol=5, page=k))
  }
  dev.off ()
  
}

# who.plot <- function(datain, variable, legend = F, viridis = F){
#   # function that plots the results of WHO-MR runs for all scenarios and saves files individually
#   # creates separate plots for deaths, cases, dalys
#   # goup interaction doesn't work below in aes_string()  - not sure why
#   require(ggplot2)
#   require(rgdal)
#   require(scales)
#   
#   dataname = deparse(substitute(datain)) # extract the filename to correctly label the saved file
#   scenario.name = c("Basecase",           # 1
#                     "ContinuingTrends",    # 2
#                     "GaviOptimistic",      # 3
#                     "IntensifiedInvestments") # 4
#   plotwhat <- c("deaths", "cases", "dalys")
#   
#   for (j in 1:length(scenario.name)){
#     scenario.j <- scenario.name[j]
#     for (i in 1:length(plotwhat)){
#       toplot = plotwhat[i]
#       p <- ggplot(datain[scenario == scenario.j,],
#                   aes_string(x = "year", y = toplot)) +
#         geom_line(aes_string( col=variable, group = interaction(variable, "country"))) +
#         facet_grid(country ~ .) +
#         theme_bw(base_size = 6) +
#         labs(x="calendar year",    y=toplot)
#       
#       if (!legend){
#         p <- p+theme(legend.position = "none")
#       }
#       if (viridis){
#         require(viridis)
#         p <- p + scale_color_viridis(name = "", discrete = T)
#       }
#       # plot file
#       figname <- paste0(fname, dataname, "_", scenario.j, "_", toplot, "_", variable, "_legend-",legend,".pdf")
#       pdf(file =  figname)
#       # split pdf into pages
#       for (k in 1:5) {
#         print (p + facet_wrap_paginate(.~country,scales = "free_y", nrow=4, ncol=5, page=k))
#       }
#       dev.off()
#     }
#   }
# }

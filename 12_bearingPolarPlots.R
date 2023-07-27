source('./R/courseToRose.R')

cogTable <-  qs::qread(file = glue('./tables/coGpi/coGpiTable_allsp.qs'))

cogDistTable <-qs::qread(file = glue('tables/distBear_coGpi/distBear_coGpi_all72spp.qs'))
qs::qsave(x =cogDistTable, file = glue('tables/distBear_coGpi/coGpiDistBearTable_72allsp.qs'))

cogDistTable <- cogDistTable %>% mutate(coursefrom2011 = (bearingfrom2011 + 360) %% 360)
cogDistTable <- cogDistTable %>% mutate(direction = sapply(coursefrom2011, courseToRose))

traits <- read.csv('./inputs/birdSpecies_traits.csv')
traits2 <- traits %>% dplyr::select(Group,species,Family)
colnames(traits2) <- c('Group', 'species', 'Family')
cogDistTable <- left_join(cogDistTable, traits2,by= 'species' )


rose_labs2 <- c(
  "North",  "Northeast",
  "East",  "Southeast",
  "South",  "Southwest",
  "West",  "Northwest",
  'North')
# 

cogDistTable31 <- cogDistTable %>% filter(year == '2031')
cogDistTable91 <- cogDistTable %>% filter(year == '2091')
#get direction from bearing for the near and long terms 
cogDistTable31<- cogDistTable31 %>% 
  select(year, species, Group,model, distancefrom2011, coursefrom2011) %>% 
  mutate(direction = sapply(coursefrom2011, courseToRose))

cogDistTable91<- cogDistTable91 %>% 
  select(year, species, Group, model, distancefrom2011, coursefrom2011) %>% 
  mutate(direction = sapply(coursefrom2011, courseToRose))

cogDistTable <- cogDistTable %>% mutate(bear.cut = cut(coursefrom2011, breaks, 
                                                       right = FALSE,
                                                       labels = seq(1,360, by = 45)))



a <- cogDistTable %>% dplyr::select(year, model, bear.cut,species,coursefrom2011, 
                                    distancefrom2011, bearingfrom2011T)
#using course
courseDistPlot31 <- ggplot(data=cogDistTable31,
                           aes(coursefrom2011, distancefrom2011, color = Group)) +
  geom_segment(aes(xend = coursefrom2011, yend = 0.1)) +
  geom_point() +
  # scale_y_continuous(limits = 0, max(a31$distancefrom2011)) +
  scale_x_continuous(limits = c(0,360), 
                     expand = c(0,0),
                     breaks= seq(0,360, 45),
                     labels = c("N","NE","E", "SE", "S","SW","W","NW","N")) +

  coord_polar(start = 6.28, direction = 1) +
  xlab("Direction") +
  ylab('Distance (km)') +
  ggtitle(label = glue(' a) 2011-2031')) +
  theme_bw() + 
  theme(plot.title = element_text(size = 16,face = 'bold',hjust = 0, vjust = 0.7),
        plot.subtitle = element_text(size = 12),
        axis.title = element_text(size = 14),
        axis.text.x = element_text(size = 12),
        axis.text.y = element_text(size = 12),
        legend.text = element_text(size = 12),
        strip.text = element_text(size = 12),
        legend.position = "bottom") +
  facet_wrap(~model)


out <- ('./graphs/figs/polar')
ifelse(!dir.exists(out), dir.create(out), 'Folder already exists')
ggsave(plot = courseDistPlot31,
       filename = glue('{out}/courseDistancePlot_2031_color.png'), 
       units = 'in', width = 14, height = 7, dpi = 700)


courseDistPlot91 <- ggplot(data=cogDistTable91,
                           aes(coursefrom2011, distancefrom2011, color = Group)) +
  geom_segment(aes(xend = coursefrom2011, yend = 0.1)) +
  geom_point() +
  scale_x_continuous(limits = c(0,360), 
                     expand = c(0,0),
                     breaks= seq(0,360, 45),
                     labels = c("N","NE","E", "SE", "S","SW","W","NW","N")) +
  scale_fill_manual(values = c('CanESM2' = "#FF6A00",'CCSM4' = "#C15CCB", 
                               'INM-CM4' = "#00868B")) +
  coord_polar(start = 6.28, direction = 1) + #start is 360 grados in radians = 6.28
  xlab("Direction") +
  ylab('Distance (km)') +
  ggtitle(label = glue(' b) 2011-2091')) +
  theme_bw() + 
  theme(plot.title = element_text(size = 16,face = 'bold',hjust = 0, vjust = 0.7),
        plot.subtitle = element_text(size = 12),
        axis.title = element_text(size = 14),
        axis.text.x = element_text(size = 12),
        axis.text.y = element_text(size = 12),
        legend.text = element_text(size = 12),
        strip.text = element_text(size = 12),
        legend.position = "bottom") +
  facet_wrap(~model)


out <- ('./graphs/figs/polar')
ifelse(!dir.exists(out), dir.create(out), 'Folder already exists')
ggsave(plot = courseDistPlot91,
       filename = glue('{out}/courseDistancePlot_2091_color.png'), 
       units = 'in', width = 14, height = 7, dpi = 700)

##save both plots together.
gall <-ggsave(glue('{out}/courseDistPlotbothYears.png'), 
              ggpubr::ggarrange(courseDistPlot31,courseDistPlot91,
                                heights = c(1,1), 
                                widths = c(1,1),
                                align = 'hv', ncol = 1,
                                #labels = c('a)', "b)"),
                                vjust = 3,
                                legend = 'bottom',
                                common.legend = T), 
              units = 'in', height = 10, width = 14, dpi = 700)

# Load libraries ----------------------------------------------------------
require(pacman)
pacman::p_load(dplyr,emmeans,glue,multcomp, qs, tidyr,tidyverse)

g <- gc(reset = TRUE)
rm(list = ls())

# Load data ---------------------------------------------------------------4
root <- './outputs'
dirs <- fs::dir_ls(root, type = 'directory')
spcs <- basename(dirs)
spcs <- spcs[1:72]

# Function to use ---------------------------------------------------------
logZonalDF <- function(spc){
  
  spc <- spcs[1]
  cat('-------------------------------------------------------------\n')
  cat('To start ', spc, '\n')
  cat('-------------------------------------------------------------\n')
  
  dir <- grep(spc, dirs, value = TRUE)
  fle <- fs::dir_ls(dir, regexp = '.qs')
  tbl <- qs::qread(file = glue('./qs/zonal/{spc}_logZonal2.qs'))
  tbl <- tbl %>%  mutate(specie = spc)
  return(tbl)
}   

# Apply the function ------------------------------------------------------

zonalDF <- map(.x = spcs, .f = logZonalDF)

znl <- bind_rows(zonalDF) 


#qs::qsave(x = znl, file = glue('./qs/zonal/allSpp_logZonal2_region2.qs'))
df <- qs::qread(file = glue('./qs/zonalOccPi/all_sp_occPiZonal1191.qs'))

df <- df %>% group_by(specie, model) %>% 
  mutate(region =as.factor(region),
         model = as.factor(model))

df$region <- relevel(df$region, ref = 'Middle')

# Calculate test statistics using aov function
mod1 <- lm (average~ region + model, data = df)
#mod2 <- lm (average~ region * model, data = df) #interaction was not significant choose more parsimonious model 
anova(mod1)
print(summary(mod1))
coef(summary.lm(mod1))
leastSquare2 <- emmeans::emmeans(mod1, pairwise~ region, adjust = 'tukey')

#all pairwise comparison 
m1_emm <- emmeans::emmeans(mod1, specs = pairwise ~ region:model)
marginal <- emmeans::emmeans(mod1, ~ region)
CLD = multcomp::cld(marginal, alpha = 0.05,
                    letters = letters,
                    adjust = 'tukey')
pairs(marginal, adjust = 'tukey')
marginal2 <- marginal %>% as.data.frame()

#put results in a df
meansDT<-m1_emm$emmeans %>% as.data.frame()
m1_simple <- emmeans::contrast(m1_emm,
                               method = "pairwise",
                               simple = "each",
                               combine = TRUE,
                               adjust = "tukey") %>%
  summary(infer = TRUE)

##now create plot of the mean effects 
my_colors <- c("#FF6A00", "#C15CCB",  "#00868B")

meanPlot<- ggplot() + 
  geom_point(data = meansDT, aes(x = region, y = emmean, group = model, color = model),
             position = position_dodge(width = 0.8)) +
  geom_errorbar(data = meansDT, aes(x = region, ymin = lower.CL, ymax = upper.CL, 
                                    color = model), width = 0.2,
                position = position_dodge(width = 0.8)) +
  labs(x = "Zone", y = "Estimate", color = "") +
  scale_x_discrete(limits = c('North', 'Middle', 'South')) +
  scale_color_manual(values = my_colors) +  # Assign custom colors
  theme_bw() +
  theme(plot.title = element_text(size = 14, face = 'bold'),
        axis.title.x = element_text(size = 12, face = 'bold'),
        axis.title.y = element_text(size = 12, face = 'bold'),
        axis.text.y = element_text( vjust = 0.5,  hjust = 0.5, size = 11),
        axis.text.x = element_text( vjust = 0.5,  hjust = 0.5, size = 11),
        aspect.ratio = 1,
        legend.position = 'right',
        legend.text = element_text(size= 11)) 

meanPlot2<- ggplot() + 
  geom_point(data = marginal2, aes(x = region, y = emmean),
             position = position_dodge(width = 0.8)) +
  geom_errorbar(data = marginal2, aes(x = region, ymin = lower.CL, ymax = upper.CL),
                width = 0.2,
                position = position_dodge(width = 0.8)) +
  labs(x = "Zone", y = "Estimate", color = "") +
  scale_x_discrete(limits = c('North', 'Middle', 'South')) +
  scale_color_manual(values = my_colors) +  # Assign custom colors
  theme_bw() +
  theme(plot.title = element_text(size = 14, face = 'bold'),
        axis.title.x = element_text(size = 12, face = 'bold'),
        axis.title.y = element_text(size = 12, face = 'bold'),
        axis.text.y = element_text( vjust = 0.5,  hjust = 0.5, size = 11),
        axis.text.x = element_text( vjust = 0.5,  hjust = 0.5, size = 11),
        aspect.ratio = 1,
        legend.position = 'right',
        legend.text = element_text(size= 11)) 

out <- glue('./graphs/figs/zonal_emmeans/effectsregion_GCM.png')
ggsave(plot = meanPlot, filename = out, units = 'in', width = 5, height = 4, dpi = 300)


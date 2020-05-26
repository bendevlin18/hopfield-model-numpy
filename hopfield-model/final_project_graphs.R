

### plotting data from synaptic deletion simulation experiments for quant neurobio ###

remove(list = ls())
library(dplyr)
library(reshape2)
library(ggplot2)
library(plotly)

setwd('C:\\Users\\Minecraft in 4K\\Dropbox\\spr_2020_classes\\quant_neurobio')

df <- read.csv('final_results_mean_tidy.csv')

df_new <- df %>%
  group_by(del_level, del_type) %>%
  summarize(mean_mem_count = mean(n_mem))

# cleaning the data a bit so it looks nice on the graph
df_new$del_level[1:4] = 0

## create ggplot line graph

ggplot(df_new, aes(x = del_level, y = mean_mem_count, color = del_type))+
  geom_smooth(method = 'loess', size = 2)+
  geom_point(size = 3)+
  geom_hline(yintercept = c(0, 0), linetype = 'dotted')+
  ylab('Mean Memory Retrieval\n')+
  xlab('\n % Deletion Level')+
  ggtitle('Optimal Memory Retrieval based \non different pruning Strategies')+
  scale_x_continuous(expand = c(0,0))+
  theme_classic()+
  theme(
    text = element_text(size = 20),
    legend.title = element_blank(),
    legend.text = element_text(size = 15),
    plot.title = element_text(size = 25, hjust = 0.5)
  )

ggsave('fig1.svg', width = 10, height = 7, units = c("in"))


df_2 <- read.csv('final_results_mean_tidy_fig2.csv')

df_2 <- filter(df_2, del_type == 'min_val' | del_type == 'random')

df_2_grouped <- df_2 %>%
  group_by(n_neurons, del_type, n_memories, n_syn) %>%
  summarize(mean = mean(n_memories))


### plot figure 2 ###

ggplot(df_2_grouped, aes(x = n_syn, y = mean, color = interaction(del_type,n_neurons)))+
  geom_smooth(method = 'gam', size = 2, se = F)+
  geom_vline(xintercept = 640000, linetype = 'dotted', size = 1.5)+
  geom_point()+
  ylab('Mean Memory Retrieval\n')+
  xlab('\n Number of Synapses')+
  ggtitle('Relationship between network\n size and memory capacity')+
  theme_classic()+
  scale_color_manual(values = c('#f8766d', '#bdbdbd', '#7cae00', '#bdbdbd', '#00bfc4', '#bdbdbd', '#c77cff', '#bdbdbd'),
                     labels = c('800 Neurons', '', '900 Neurons', '', 
                                '1,000 Neurons', '', '1,100 Neurons', ''))+
  theme(
    text = element_text(size = 20),
    legend.title = element_blank(),
    legend.text = element_text(size = 15),
    plot.title = element_text(size = 25, hjust = 0.5)
  )

ggsave('fig2.svg', width = 10, height = 7, units = c("in"))

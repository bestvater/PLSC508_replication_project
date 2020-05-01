# read in edgelists and node attributes from replication file
attab <- read.csv('attab.csv', stringsAsFactors = F)
attab <- attab[!is.na(attab$ID), ]
attab$Tribe <- ifelse(attab$Tribe == 'ATESO' | attab$Tribe == 'KUMAM', attab$Tribe, 'OTHER')
attab$Tribe <- ifelse(is.na(attab$Tribe), 'OTHER', attab$Tribe)
attab$ID <- paste('id_', attab$ID, sep = '')

attmu <- read.csv('attmu.csv', stringsAsFactors = F)
attmu <- attmu[!is.na(attmu$ID), ]
#attmu$Tribe[attmu$Tribe == 'RUYANKOLE'] <- 'RUNYANKOLE'
attmu$Tribe <- ifelse(attmu$Tribe == 'ATESO' | attmu$Tribe == 'KUMAM', attmu$Tribe, 'OTHER')
attmu$Tribe <- ifelse(is.na(attmu$Tribe), 'OTHER', attmu$Tribe)
attmu$ID <- paste('id_', attmu$ID, sep = '')

elab <- read.csv('elab.csv', stringsAsFactors = F)
elab$Source <- paste('id_', elab$Source, sep = '')
elab$Target <- paste('id_', elab$Target, sep = '')
elab <- as.matrix(elab[,1:2])

elmu <- read.csv('elmu.csv', stringsAsFactors = F)
elmu$Source <- paste('id_', elmu$Source, sep = '')
elmu$Target <- paste('id_', elmu$Target, sep = '')
elmu <- as.matrix(elmu[,1:2])

require(network)

# build Abalang network
n <- nrow(attab)
abnet <- network.initialize(n, dir = T)
network.vertex.names(abnet) <- attab$ID
set.vertex.attribute(abnet, 'Hear', attab$Hear)
set.vertex.attribute(abnet, 'Attend', attab$Attend)
set.vertex.attribute(abnet, 'Seed', attab$Seed)
set.vertex.attribute(abnet, 'Tribe', attab$Tribe)

abnet[elab] <- 1
set.vertex.attribute(abnet, 'degree', sna::degree(abnet, cmode = 'indegree'))

# build Mugana network
n <- nrow(attmu)
munet <- network.initialize(n, dir = T)
network.vertex.names(munet) <- attmu$ID
set.vertex.attribute(munet, 'Hear', attmu$Hear)
set.vertex.attribute(munet, 'Attend', attmu$Attend)
set.vertex.attribute(munet, 'Seed', attmu$Seed)
set.vertex.attribute(munet, 'Tribe', attmu$Tribe)

munet[elmu] <- 1
set.vertex.attribute(munet, 'degree', sna::degree(munet, cmode = 'indegree'))


require(ggplot2)
require(GGally)
require(ggnetwork)
theme_set(theme_bw())

set.seed(42)

abnet_fr <- ggnetwork(abnet, layout = 'fruchtermanreingold', arrow.gap = 0.01)
munet_fr <- ggnetwork(munet, layout = 'fruchtermanreingold', arrow.gap = 0.01)

ggplot(data = abnet_fr, aes(x, y, xend=xend, yend=yend))+
  geom_edges(color = 'grey75', 
             curvature = 0.1,
             arrow = arrow(length = unit(2, 'pt'), type = 'closed'))+
  geom_nodes(aes(color = Tribe, size = degree))+
  theme_blank()+
  scale_size_continuous(range = c(0,4))+
  guides(size = F)+
  labs(title = 'Social network of Abalang (Ethnically-homogeneous village)')

ggsave('./figures/abalang_net.pdf', height = 7, width = 9, units = 'in')

ggplot(data = munet_fr, aes(x, y, xend=xend, yend=yend))+
  geom_edges(color = 'grey75', 
             curvature = 0.1,
             arrow = arrow(length = unit(2, 'pt'), type = 'closed'))+
  geom_nodes(aes(color = Tribe, size = degree))+
  theme_blank()+
  scale_size_continuous(range = c(0,4))+
  guides(size = F)+
  labs(title = 'Social network of Mugana (Ethnically-heterogeneous village)')

ggsave('./figures/mugana_net.pdf', height = 7, width = 9, units = 'in')

attab$Village <- 'Abalang'
attmu$Village <- 'Mugana'

attall <- rbind(attab, attmu)

ggplot(data = attall, aes(x = Tribe, group = Village))+
  geom_bar(stat = 'count', position = 'dodge', aes(fill = Village))+ 
  theme(axis.text.x = element_text(angle = 90, hjust = 1))+
  theme(legend.position="bottom")+
  labs(title = 'Distribution of Ethnic Identity in Abalang and Mugana Villages',
       x = NULL,
       y = NULL)

ggsave('./figures/ethnicity.pdf', height = 4, width = 9, units = 'in')

ggplot(data = attall, aes(x = Hear))+
  geom_bar(stat = 'count')+
  facet_grid(cols = vars(Village))+
  labs(title = 'Distribution of information diffusion',
       x = 'Respondent received seeded information',
       y = NULL)

ggsave('./figures/info_diffusion.pdf', height = 4, width = 9, units = 'in')

ggplot(data = attall, aes(x = Attend))+
  geom_bar(stat = 'count')+
  facet_grid(cols = vars(Village))+
  labs(title = 'Distribution of respondents acting upon diffused information',
       x = 'Respondent acted upon seeded information',
       y = NULL)

ggsave('./figures/info_action.pdf', height = 4, width = 9, units = 'in')

set.vertex.attribute(abnet, 'betweenness', sna::betweenness(abnet))
set.vertex.attribute(munet, 'betweenness', sna::betweenness(munet))
set.vertex.attribute(abnet, 'eigencent', sna::evcent(abnet))
set.vertex.attribute(munet, 'eigencent', sna::evcent(munet))

plot_df <- as.data.frame(cbind('Abalang', get.vertex.attribute(abnet, 'degree')))
plot_df <- rbind(plot_df, cbind('Mugana', get.vertex.attribute(munet, 'degree')))
names(plot_df) <- c('network', 'degree')
plot_df$degree <- as.numeric(plot_df$degree)

plot_df <- as.data.frame(matrix(data = NA, nrow = length(get.vertex.attribute(abnet, 'degree')), ncol = 3))
names(plot_df) <- c('network', 'iteration', 'degree')
for(i in 1:length(get.vertex.attribute(abnet, 'degree'))){
  plot_df$network[i] <- 'Abalang'
  plot_df$iteration[i] <- 1
  plot_df$degree[i] <- get.vertex.attribute(abnet, 'degree')[i]
}

plot_df2 <- as.data.frame(matrix(data = NA, nrow = length(get.vertex.attribute(munet, 'degree')), ncol = 3))
names(plot_df2) <- c('network', 'iteration', 'degree')
for(i in 1:length(get.vertex.attribute(munet, 'degree'))){
  plot_df2$network[i] <- 'Mugana'
  plot_df2$iteration[i] <- 0
  plot_df2$degree[i] <- get.vertex.attribute(munet, 'degree')[i]
}

plot_df <- rbind(plot_df, plot_df2)

ggplot(data = plot_df, aes(x = degree, group = network, fill = network))+
  geom_density(kernel = 'gaussian', alpha = 0.5)+
  labs(x = 'In-degree Centrality', fill = 'Village', title = 'Comparing degree distributions in Abalang and Mugana networks')

ggsave('./figures/degree_comparison.pdf', height = 5, width = 9, units = 'in')

plot_df <- as.data.frame(matrix(data = NA, nrow = length(get.vertex.attribute(abnet, 'betweenness')), ncol = 3))
names(plot_df) <- c('network', 'iteration', 'betweenness')
for(i in 1:length(get.vertex.attribute(abnet, 'betweenness'))){
  plot_df$network[i] <- 'Abalang'
  plot_df$iteration[i] <- 1
  plot_df$betweenness[i] <- get.vertex.attribute(abnet, 'betweenness')[i]
}

plot_df2 <- as.data.frame(matrix(data = NA, nrow = length(get.vertex.attribute(munet, 'betweenness')), ncol = 3))
names(plot_df2) <- c('network', 'iteration', 'betweenness')
for(i in 1:length(get.vertex.attribute(munet, 'betweenness'))){
  plot_df2$network[i] <- 'Mugana'
  plot_df2$iteration[i] <- 0
  plot_df2$betweenness[i] <- get.vertex.attribute(munet, 'betweenness')[i]
}

plot_df <- rbind(plot_df, plot_df2)

ggplot(data = plot_df, aes(x = betweenness, group = network, fill = network))+
  geom_density(kernel = 'gaussian', alpha = 0.5)+
  labs(x = 'Betweenness Centrality', fill = 'Village', title = 'Comparing betweenness distributions in Abalang and Mugana networks')

ggsave('./figures/betweenness_comparison.pdf', height = 5, width = 9, units = 'in')

plot_df <- as.data.frame(matrix(data = NA, ncol = 6, nrow = 2))
names(plot_df) <- c('Village', 'density', 'reciprocity', 'transitivity', 'degree_assortativity', 'ethnic_assortativity')

plot_df$Village <- c('Abalang', 'Mugana')
plot_df$density <- c(sna::gden(abnet),
                     sna::gden(munet))

plot_df$reciprocity <- c(sna::grecip(abnet, measure='correlation'),
                         sna::grecip(munet, measure='correlation'))

plot_df$transitivity <- c(sna::gtrans(abnet, mode="digraph", measure=c("weak")),
                          sna::gtrans(munet, mode="digraph", measure=c("weak")))

abnet_ig <- intergraph::asIgraph(abnet)
munet_ig <- intergraph::asIgraph(munet)

plot_df$degree_assortativity <- c(igraph::assortativity_degree(abnet_ig, directed = T),
                                  igraph::assortativity_degree(munet_ig, directed = T))

plot_df$ethnic_assortativity <- c(NA,#igraph::assortativity_nominal(abnet_ig, types = as.factor(igraph::V(abnet_ig)$Tribe),  directed = T),
                                  igraph::assortativity_nominal(munet_ig, types = as.factor(igraph::V(munet_ig)$Tribe),  directed = T))

stargazer::stargazer(plot_df, summary = F, type = 'latex', align = F, header = F, 
                     out = './tables/graph_characteristics.tex',
                     title = 'Graph-level Descriptive Statistics')

plot_df <- reshape2::melt(plot_df, id.vars = 'Village')

ggplot(plot_df, aes(x = variable, y = value, group = Village, fill = Village))+
  geom_bar(stat = 'identity', position = 'dodge')+
  labs(x = NULL, title = 'Comparaing Graph-Level Characteristics, Abalang and Mugana')+ 
  theme(axis.text.x = element_text(angle = 90, hjust = 1))

ggsave('./figures/graph_characteristics.pdf', height = 4, width = 9, units = 'in')

require(ergm)

# density, reciprocity, transitivity, degree assortativity, ethnic assortativity

set.seed(41)

m1 <- ergm(abnet ~ edges + mutual + gwnsp + idegree(0) + gwodegree(0) +
             nodeifactor('Tribe', levels = -2) +
             nodematch('Tribe', diff = T, levels = -2))


summary(m1)

ergm.degeneracy(m1)

mcmc.diagnostics(m1)

gofm1 <- gof(m1)

par(mfrow=c(2,2))
par(oma=c(0.5,2,1,0.5))
plot(gofm1)

set.seed(42)
m2 <- ergm(munet ~ edges + mutual + gwnsp + gwodegree(0) + idegree(c(0)) +
             nodeifactor('Tribe', levels = -3) + 
             nodematch('Tribe', diff = T, levels = -3))

summary(m2)

ergm.degeneracy(m2)

mcmc.diagnostics(m2)

gofm2 <- gof(m2)

par(mfrow=c(2,2))
par(oma=c(0.5,2,1,0.5))
plot(gofm2)

stargazer::stargazer(m1, m2, type = 'latex', out = './tables/ERGM_results.tex', 
                     #font.size = "footnotesize",
                     single.row = T,
                     title = 'ERGM Results for Abalang and Mugana Networks')

plot_df <- as.data.frame(matrix(data = NA, nrow = 0, ncol = 3))
names(plot_df) <- c('network', 'iteration', 'betweenness')

plot_df2 <- as.data.frame(matrix(data = NA, nrow = length(get.vertex.attribute(munet, 'betweenness')), ncol = 3))
names(plot_df2) <- c('network', 'iteration', 'betweenness')
for(i in 1:length(get.vertex.attribute(munet, 'betweenness'))){
  plot_df2$network[i] <- 'Mugana'
  plot_df2$iteration[i] <- 0
  plot_df2$betweenness[i] <- get.vertex.attribute(munet, 'betweenness')[i]
}

plot_df <- rbind(plot_df, plot_df2)


for(j in 2:11){
  m2_sim <- simulate(m2, seed = j)
  set.vertex.attribute(m2_sim, 'betweenness', sna::betweenness(m2_sim))
  plot_df2 <- as.data.frame(matrix(data = NA, nrow = length(get.vertex.attribute(munet, 'betweenness')), ncol = 3))
  names(plot_df2) <- c('network', 'iteration', 'betweenness')
  for(i in 1:length(get.vertex.attribute(m2_sim, 'betweenness'))){
    plot_df2$network[i] <- 'Mugana sim'
    plot_df2$iteration[i] <- j
    plot_df2$betweenness[i] <- get.vertex.attribute(m2_sim, 'betweenness')[i]
  }
  plot_df <- rbind(plot_df, plot_df2)
}

plot_df <- purrr::map_df(plot_df, rev)

ggplot(data = plot_df, aes(x = betweenness, group = iteration, fill = network))+
  geom_density(kernel = 'gaussian', alpha = 0.3)+
  labs(x = 'Betweenness Centrality', fill = 'Village', title = 'Comparing betweenness distributions in real and simulated Mugana networks')

ggsave('./figures/betweenness_comparison_sim.pdf', height = 5, width = 9, units = 'in')

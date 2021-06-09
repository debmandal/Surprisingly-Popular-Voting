theme_set(theme_bw() + theme(
  plot.title = element_text(size = 25, hjust = 0.5),
  panel.grid.major = element_line(color = "gray70", size = 0.1),
  panel.grid.minor = element_blank(),
  axis.text.x = element_text(colour = "black", size = 25, hjust = .5, vjust = .5, face = "plain"),
  axis.text.y = element_text(colour = "black", size = 25, hjust = .5, vjust = .5, face = "plain"),
  axis.title.x = element_text(colour = "black", face="bold",size = 30, angle = 0, hjust = .5, vjust = 0),
  axis.title.y = element_text(colour = "black", face="bold", size = 30, angle = 90, hjust = .5, vjust = .5),
  legend.position="top",legend.title=element_text(size=25),legend.text = element_text(size=25),
  legend.box.background = element_rect(colour = "black",size=1.5),
  strip.background = element_rect(fill = "gray95"),
  strip.text.x = element_text(size = 25, face="bold")
))

dfcombtop <- read_csv('data/csvs/Combined_Voting_SPVoting_Rank.csv')
dfcombtop$type <- factor(dfcombtop$type, levels = c('Plurality', 'Plurality w/ Runoff', 'Borda', 'Copeland', 'IRV', 'Maximin', 'SPVoting'))
dfcombtop$Rule <- factor(dfcombtop$Rule, levels = c('Plurality', 'Plurality w/ Runoff', 'Borda', 'Copeland', 'IRV', 'Maximin', 'SPVoting', 'Rank-None', 'Rank-Top', 'Rank-Rank'))
dfcombtop %>% ggplot(aes(x=type, y=y, group=Rule,fill=fill)) + 
  geom_col(width = 0.7, position = position_dodge(0.75)) + geom_bar(width = 0.7, position = position_dodge(0.75), stat='identity', color = 'black') + 
  geom_errorbar(aes(ymin = ymin, ymax = ymax), width=0.2, size=1, position = position_dodge(0.75)) +
  scale_fill_brewer(palette = 'Spectral') +
  xlab('Voting Rule') + ylab('Average Prediction Error') + guides(fill=guide_legend(title="Method")) +
  scale_x_discrete(breaks=c('Plurality', 'Plurality w/ Runoff', 'Borda', 'Copeland', 'IRV', 'Maximin', 'SPVoting'), labels = c('Plurality', 'Plurality w/\nRunoff', 'Borda', 'Copeland', 'IRV', 'Maximin', 'SPVoting\n(Rank-*)'))

dfcombrank <- read_csv('data/csvs/Combined_Voting_SPVoting_Rank.csv')
dfcombrank$type <- factor(dfcombrank$type, levels = c('Plurality', 'Plurality w/ Runoff', 'Borda', 'Copeland', 'IRV', 'Maximin', 'SPVoting'))
dfcombrank$Rule <- factor(dfcombrank$Rule, levels = c('Plurality', 'Plurality w/ Runoff', 'Borda', 'Copeland', 'IRV', 'Maximin', 'SPVoting', 'Rank-None', 'Rank-Top', 'Rank-Rank'))
dfcombrank %>% ggplot(aes(x=type, y=y, group=Rule,fill=fill)) + 
  geom_col(width = 0.7, position = position_dodge(0.75)) + geom_bar(width = 0.7, position = position_dodge(0.75), stat='identity', color = 'black') + 
  geom_errorbar(aes(ymin = ymin, ymax = ymax), width=0.2, size=1, position = position_dodge(0.75)) +
  scale_fill_brewer(palette = 'Spectral') +
  xlab('Voting Rule') + ylab('Average Kendall-Tau Distance') + guides(fill=guide_legend(title="Method")) +
  scale_x_discrete(breaks=c('Plurality', 'Plurality w/ Runoff', 'Borda', 'Copeland', 'IRV', 'Maximin', 'SPVoting'), labels = c('Plurality', 'Plurality w/\nRunoff', 'Borda', 'Copeland', 'IRV', 'Maximin', 'SPVoting\n(Rank-*)'))

dftop <- read_csv('data/csvs/Top_all_SPVoting.csv')
dftop$treatment <- factor(dftop$treatment, levels = c('Top-None', 'Top-Top', 'Top-Rank', 'Rank-None', 'Rank-Top', 'Rank-Rank'))
dftop$type <- factor(dftop$type, levels = c('SPVoting', 'Vote', 'Prediction'))
dftop %>% ggplot(aes(x=treatment, y=y, group=type,fill=type)) + 
  geom_col(width = 0.7, position = position_dodge(0.75)) + #geom_bar(width = 0.7, position = position_dodge(0.75), stat='identity', color = 'black') + 
  geom_errorbar(aes(ymin = ymin, ymax = ymax), width=0.2, size=1, position = position_dodge(0.75)) +
  scale_fill_brewer(palette = 'Spectral') +
  xlab('Elicitation Format') + ylab('Average Prediction Error') + guides(fill=guide_legend(title="Type"))

dfrank <- read_csv('data/csvs/KT_all_SPVoting.csv')
dfrank$treatment <- factor(dfrank$treatment, levels = c('Top-None', 'Top-Top', 'Top-Rank', 'Rank-None', 'Rank-Top', 'Rank-Rank'))
dfrank$type <- factor(dfrank$type, levels = c('SPVoting', 'Vote', 'Prediction'))
dfrank %>% ggplot(aes(x=treatment, y=y, group=type,fill=type)) + 
  geom_col(width = 0.7, position = position_dodge(0.75)) + #geom_bar(width = 0.7, position = position_dodge(0.75), stat='identity', color = 'black') + 
  geom_errorbar(aes(ymin = ymin, ymax = ymax), width=0.2, size=1, position = position_dodge(0.75)) +
  scale_fill_brewer(palette = 'Spectral') +
  xlab('Elicitation Format') + ylab('Average Kendall Tau Distance') + guides(fill=guide_legend(title="Type"))


responses <- read_csv('~/Documents/SPVoting-Analysis/Analysis/question_responses.csv')

# use regex to find the predictions
responses$prediction <- gsub(".*\\[", "", responses$response)
responses$options <- strsplit(gsub('\\[|\\]|[[:space:]]', '', responses$options), ",")
responses$prediction <- strsplit(gsub('\\[|\\]|[[:space:]]', '', responses$prediction), ",")
responses$signal <- gsub("\\].*", "", responses$response)
responses$signal <- strsplit(gsub('\\[|\\]|[[:space:]]', '', responses$signal), ",")

responses$treatment <- as.factor(responses$treatment)
levels(responses$treatment) <- c('Top-None', 'Top-Top', 'Top-Rank', 'Rank-None', 'Rank-Top', 'Rank-Rank')
responses$domain <- as.factor(responses$domain)
levels(responses$domain) <- c('Geography','Movies','Paintings')

TALoss <- function(r1,alt)
{
  res1 <- r1

  if(strtoi(res1[[1]][1]) == strtoi(alt) ){
    return(0)
  }
  else{
    return(1)
  }
  
}
KTDist.top <- function(r1, alt)
{
  order1 <- c(r1)
  pos <- match(alt, order1)
  return(pos + 0.5)
}
#assume list input as ranks r1 and r2
KTDist.list <- function(r1,r2)
{
  order2 <- c(r2)
  
  #print(order2)
  
  order1 <- c(r1)
  
  #print(order1)
  #print(order2)
  
  #true <- strtoi(order1)
  #reported <- strtoi(order2)
  
  #print(order1)
  #print(order2)
  locs <- c(1:4)
  for (i in 1:4) {
    locs[i] <- match(order2[i], order1)
  }
  dist <- 0
  for(i in 1:3)
  {
    for(j in (i+1):4)
    {
      if(locs[j] < locs[i])
      {
        dist <- dist + 1
      }
    }
  }
  return(dist)
}

getTopError <- function(predictions) {
  return(map2_dbl(predictions$options,  predictions$prediction, TALoss))
}

getRankError <- function(predictions) {
  top_alts <- map(predictions$prediction, function(x){return(x[[1]][1])})
  return(map2_dbl(predictions$options, top_alts, TALoss))
}

getTopErrorSig <- function(signals){
  return(map2_dbl(signals$options, signals$signal, TALoss))
}

getRankErrorSig <- function(signals){
  top_alts <- map(signals$signal, function(x){return(x[[1]][1])})
  return(map2_dbl(signals$options,top_alts, TALoss))
}

df <- tibble(
  treatment := NA,
  type := NA,
 # domain := NA,
  y := 0,
  ymin := 0,
  ymax := 0
)

#for(dom in c('Geography', 'Movies', 'Paintings'))
#{
  #rank error for all domains combined
  for(tr in c('Top-Top', 'Top-Rank', 'Rank-Top', 'Rank-Rank'))
  {
    
    if(tr %in% c('Top-Top', 'Rank-Top'))
    {
     # dftop <- responses %>% filter(treatment == tr & domain == dom) %>% select(question, options, prediction)
      dftop <- responses %>% filter(treatment == tr) %>% select(question, options, prediction)
      res <- getTopError(dftop)
      print(c(tr, 'prediction', mean(res), 1.975*sd(res)/sqrt(length(res))) )
      #df <- df %>% add_row(treatment = tr, type = 'Prediction', domain=dom, y=mean(res), ymin = mean(res) - 1.975*sd(res)/sqrt(length(res)), ymax = mean(res) + 1.975*sd(res)/sqrt(length(res)) )
      df <- df %>% add_row(treatment = tr, type = 'Prediction', y=mean(res), ymin = mean(res) - 1.975*sd(res)/sqrt(length(res)), ymax = mean(res) + 1.975*sd(res)/sqrt(length(res)) )
    }
    else if(tr %in% c('Top-Rank', 'Rank-Rank'))
    {
      #dfrank <- responses %>% filter(treatment == tr & domain == dom) %>% select(question, options, prediction)
      dfrank <- responses %>% filter(treatment == tr) %>% select(question, options, prediction)
      res <- getRankError(dfrank)
      print(c(tr, 'prediction', mean(res), 1.975*sd(res)/sqrt(length(res))) )
      #df <- df %>% add_row(treatment = tr, type = 'Prediction', domain=dom, y=mean(res), ymin = mean(res) - 1.975*sd(res)/sqrt(length(res)), ymax = mean(res) + 1.975*sd(res)/sqrt(length(res)) )
      df <- df %>% add_row(treatment = tr, type = 'Prediction', y=mean(res), ymin = mean(res) - 1.975*sd(res)/sqrt(length(res)), ymax = mean(res) + 1.975*sd(res)/sqrt(length(res)) )
    }
  }
  
  #sginal error for all domains combined
  for(tr in c('Top-Top', 'Top-Rank', 'Rank-Top', 'Rank-Rank'))
  {
    
    if(tr %in% c('Top-Top', 'Top-Rank'))
    {
      dftop <- responses %>% filter(treatment == tr) %>% select(question, options, signal)
      #dftop <- responses %>% filter(treatment == tr & domain == dom) %>% select(question, options, signal)
      res <- getTopErrorSig(dftop)
      print(c(tr, 'Vote', mean(res), 1.975*sd(res)/sqrt(length(res))) )
      #df <- df %>% add_row(treatment = tr, type = 'Vote', domain=dom, y=mean(res), ymin = mean(res) - 1.975*sd(res)/sqrt(length(res)), ymax = mean(res) + 1.975*sd(res)/sqrt(length(res)) )
      df <- df %>% add_row(treatment = tr, type = 'Vote', y=mean(res), ymin = mean(res) - 1.975*sd(res)/sqrt(length(res)), ymax = mean(res) + 1.975*sd(res)/sqrt(length(res)) )
    }
    else if(tr %in% c('Rank-Top', 'Rank-Rank'))
    {
      #dfrank <- responses %>% filter(treatment == tr & domain == dom) %>% select(question, options, signal)
      dfrank <- responses %>% filter(treatment == tr) %>% select(question, options, signal)
      res <- getRankErrorSig(dfrank)
      print(c(tr, 'Vote', mean(res), 1.975*sd(res)/sqrt(length(res))) )
      #df <- df %>% add_row(treatment = tr, type = 'Vote', domain=dom, y=mean(res), ymin = mean(res) - 1.975*sd(res)/sqrt(length(res)), ymax = mean(res) + 1.975*sd(res)/sqrt(length(res)) )
      df <- df %>% add_row(treatment = tr, type = 'Vote', y=mean(res), ymin = mean(res) - 1.975*sd(res)/sqrt(length(res)), ymax = mean(res) + 1.975*sd(res)/sqrt(length(res)) )
    }
  }
#}


  
#!/usr/bin/env Rscript
args = commandArgs(trailingOnly=TRUE)

# test if there is at least one argument: if not, return an error
if (length(args)==0) {
  stop("At least one argument must be supplied (type)", call.=FALSE)
}

type_domain <- toString(args[1])

library(tidyverse)
library(boot)
#add functions
###### analysis starts here ########
# df <- read_csv('./question_responses.csv')
# df$treatment <- as.factor(df$treatment)
# levels(df$treatment) <- c('TopNone', 'TopTop', 'TopRank', 'RankNone', 'RankTop', 'RankRank')
# df$domain <- as.factor(df$domain)
# levels(df$domain) <- c('Geography','Movies','Paintings')

Validate <- function(treatment, options, response)
{    
  res <- str_split(str_sub(options,2),", ")
  res4 <- str_split(res[[1]][4],"]")
  order <- strtoi(c(res[[1]][1], res[[1]][2], res[[1]][3], res4[[1]][1]))
  
  if( (treatment == 'TopNone' | treatment == 'TopTop') | treatment == 'TopRank')
  {
    #print('Here')
    top_alt <- strtoi( str_split(str_sub(response,3),"]")[[1]][1], base=10 )
    if(length(which(top_alt %in% order)) == 0){
      return(0)
    }
    else{
      return(1)
    }
    if(treatment == 'TopTop'){
      #check prediction
      res <- str_split(str_sub(response,3),", ")[[1]]
      pred_alt <- strtoi(str_split(str_sub(res[2],2), ']')[[1]][1] , base=10)
      if(length(which(pred_alt %in% order)) == 0){
        return(0)
      }
      else{
        return(1)
      }
    }
    else if(treatment == 'TopRank'){
      #check rank prediction
      res <- str_split(str_sub(response,3),", ")[[1]]
      alt1 <- strtoi( str_sub(res[2], 2) )
      alt2 <- strtoi(res[3])
      alt3 <- strtoi(res[4])
      alt4 <- strtoi(str_split(res[5],']')[[1]][1])
      
      if(length(which(alt1 %in% order)) == 0){
        return(0)
      }else if(length(which(alt2 %in% order)) == 0){
        return(0)
      }else if(length(which(alt3 %in% order)) == 0){
        return(0)
      }else if(length(which(alt4 %in% order)) == 0){
        return(0)
      }else{
        return(1)
      }
    }
  }
  #return(2)
}

Validate.V <- Vectorize(Validate)
## Functions


TopNoneAggr <- function(response){
  dat <- tibble(response)
  names(dat) <- c('response')
  
  dat$top_alternative <- sapply(dat$response, extract_top)
  frq <- table(dat$top_alternative)
  #print(names(which(frq == max(frq))))
  most_freq <- names(which(frq == max(frq)))[1]  #first one if there is a tie
  return(strtoi(most_freq))
}

TopNoneAggr_Rank <- function(response){
  
  #topnone aggregator but return rank
  dat <- tibble(response)
  names(dat) <- c('response')
  
  dat$top_alternative <- sapply(dat$response, extract_top)
  frq <- table(dat$top_alternative)
  frqs <- sort(frq, decreasing=TRUE)
  #print(frqs)
  str <- paste0('[',toString(names(frqs)))
  return(paste0(str, ']'))
}

#r1 is the predicted rank
KTDist <- function(r1,r2)
{
  res2 <- str_split(str_sub(r2,2),", ")
  res24 <- str_split(res2[[1]][4],"]")
  order2 <- c(res2[[1]][1], res2[[1]][2], res2[[1]][3], res24[[1]][1])
  
  #print(order2)
  
  res1 <- str_split(str_sub(r1,2),", ")
  if(length(res1[[1]]) == 2)
  {
    #print(res1[[1]][2])
    res12 <- str_split(res1[[1]][2],"]")
    #print(res12)
    #find elements in order2 that are absent in order1
    order1 <- c(res1[[1]][1], res12[[1]][1])
    #print(order1)
    diff <- order2[!(order2 %in% order1)]
    #print(diff)
    order1 <- c(order1, diff)
  }
  else if(length(res1[[1]]) == 3)
  {
    res13 <- str_split(res1[[1]][3],"]")
    #find elements in order2 that are absent in order1
    order1 <- c(res1[[1]][1], res1[[1]][2], res13[[1]][1])
    diff <- order2[!(order2 %in% order1)]
    order1 <- c(order1, diff)
  }
  else
  {
    #length is 4 otherwise
    res14 <- str_split(res1[[1]][4],"]")
    order1 <- c(res1[[1]][1], res1[[1]][2], res1[[1]][3], res14[[1]][1])
  }
  
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

extract_top <- function(arg){
  res1 <- str_split(str_sub(arg,3),"]")
  return(strtoi(res1[[1]][1],base=10) )
}

extract_top_q <- function(arg){
  res1 <- str_split(str_sub(arg,2),",")
  return(strtoi(res1[[1]][1]))
}

#Questions <- df %>% select(c('domain','question','options')) %>% unique()
#Questions$top_alternative <- map_int(Questions$options, extract_top_q)

extract_predicted <- function(arg){
  res1 <- str_split(arg, ", ")
  res2 <- str_sub(res1[[1]][2], 2)
  res3 <- str_split(res2,"]")
  
  return(strtoi(res3[[1]][1]))
}

#pred_alt is the top alternative in prediction report
posterior_probs <- function(pred_alt, options, v)
{
  low <- (1-v)/3
  high <- v
  
  res <- str_split(str_sub(options,2),", ")
  res4 <- str_split(res[[1]][4],"]")
  order <- strtoi(c(res[[1]][1], res[[1]][2], res[[1]][3], res4[[1]][1]))
  
  probs <- c(0,0,0,0)
  idx <- which(order == pred_alt)
  for(i in 1:4){
    if(idx == i)
    {
      probs[i] <- high
    }
    else
    {
      probs[i]<- low
    }
    
  }
  return(toString(probs) )
}

TopTopAggr <- function(top_alt, post_probs, options)
{
  #compute prediction normalized vote
  dat <- tibble(top_alt, post_probs, options)
  names(dat) <- c('top_alternative','post_probs', 'options')
  
  ns <- nrow(dat)
  
  dat <- dat %>% group_by(top_alternative, options) %>% summarise(obs_freq = n() / ns, avg_probs = AvgPost(post_probs) )
  dat <- dat %>% mutate(position = locate(top_alternative, options))
  
  
  dat$normalized_votes <- 0
  
  for(i in 1:nrow(dat))
  {
    alt <- dat[i,]$top_alternative
    val <- 0
    posn <- dat[i,]$position
    
    vstr <- str_split(dat[i,]$avg_probs, ", ")
    current_probs <- as.double(vstr[[1]] )
    
    
    for(j in 1:nrow(dat))
    {
      if(j != i){
        posn_j <- dat[j,]$position
        vstr <- str_split(dat[j,]$avg_probs, ", ")
        other_probs <- as.double(vstr[[1]] )
        #print(other_probs[posn] / current_probs[posn_j])
        val <- val + (other_probs[posn] / current_probs[posn_j])
      }
      
    }
    dat[i,]$normalized_votes <- val * dat[i,]$obs_freq
  }
  
  idx <- which(dat$normalized_votes == max(dat$normalized_votes))
  
  return(dat[idx[1],]$top_alternative)
}

TopTopAggr_Rank <- function(top_alt, post_probs, options)
{
  #compute prediction normalized vote
  dat <- tibble(top_alt, post_probs, options)
  names(dat) <- c('top_alternative','post_probs', 'options')
  
  ns <- nrow(dat)
  
  dat <- dat %>% group_by(top_alternative, options) %>% summarise(obs_freq = n() / ns, avg_probs = AvgPost(post_probs) )
  dat <- dat %>% mutate(position = locate(top_alternative, options))
  
  
  dat$normalized_votes <- 0
  
  for(i in 1:nrow(dat))
  {
    alt <- dat[i,]$top_alternative
    val <- 0
    posn <- dat[i,]$position
    
    vstr <- str_split(dat[i,]$avg_probs, ", ")
    current_probs <- as.double(vstr[[1]] )
    
    
    for(j in 1:nrow(dat))
    {
      if(j != i){
        posn_j <- dat[j,]$position
        vstr <- str_split(dat[j,]$avg_probs, ", ")
        other_probs <- as.double(vstr[[1]] )
        #print(other_probs[posn] / current_probs[posn_j])
        val <- val + (other_probs[posn] / current_probs[posn_j])
      }
      
    }
    dat[i,]$normalized_votes <- val * dat[i,]$obs_freq
  }
  #print(dat[order(-dat$normalized_votes),])
  newdat <- dat[order(-dat$normalized_votes),]  #sort by normalized_votes (descending)
  str <- toString(newdat$top_alternative)
  
  str1 <- paste0('[',str)
  return(paste0(str1, ']'))
}

locate <- function(top_alt, options)
{
  res <- str_split(str_sub(options,2),", ")
  res4 <- str_split(res[[1]][4],"]")
  order <- strtoi(c(res[[1]][1], res[[1]][2], res[[1]][3], res4[[1]][1]))
  
  return(which(order == top_alt) )
}

AvgPost <- function(post_probs)
{
  #average posterior probabilities across the users
  dat <- tibble(post_probs)
  names(dat) <- c('post_probs')
  
  probs <- c(0,0,0,0)
  for(i in 1:nrow(dat))
  {
    vstr <- str_split(dat[i,]$post_probs, ", ")
    #print(vstr[[1]])
    current_probs <- as.double(vstr[[1]] )
    
    probs <- probs + current_probs
  }
  probs <- probs / nrow(dat)
  
  return(toString(probs))
}

#functions for TopRank
extract_rank <- function(response){
  res <- str_split(response, "]")
  res <- str_sub(res[[1]][2],4)
  res1 <- str_split(res, ", ")
  res14 <- str_split(res1[[1]][4], "]")
  return(toString(c(res1[[1]][1], res1[[1]][2], res1[[1]][3], res14[[1]][1])  ) )
  
}

posterior_probs_rank <- function(pred_rank, options, score)
{
  #score <- c(27, 9, 3, 1) / 40
  
  res <- str_split(str_sub(options,2),", ")
  res4 <- str_split(res[[1]][4],"]")
  order <- strtoi(c(res[[1]][1], res[[1]][2], res[[1]][3], res4[[1]][1]))
  
  probs <- c(0,0,0,0)
  rank <- strtoi(str_split(pred_rank, ", ")[[1]])
  
  #print(rank)
  #print(order)
  for(i in 1:4){
    idx <- which(order == rank[i])
    probs[idx] <- score[i]
  }
  
  return(toString(probs) )
}

#Information Signal: Rank
GenPairs <- function(options)
{
  res <- str_sub(str_split(options, "]")[[1]][1], 2)
  
  set <- str_split(res, ", ")[[1]]
  a <- set[1]
  b <- set[2]
  c <- set[3]
  d <- set[4]
  
  
  return(list(c(a,b), c(a,c), c(a,d), c(b,c) , c(b,d), c(c,d)))
}

#Given a pair of alternatives a,b: retrieve the dataset for a,b (information report: 1 if a > b and 0 o.w.)

infoab <- function(response, a, b)
{
  res <- str_sub(str_split(response, ']')[[1]][1], 3)
  alts <- strtoi(str_split(res, ", ")[[1]] )
  #find positions of a and b
  idxa <- which(alts == a)
  idxb <- which(alts == b)
  if (idxa < idxb){ return(1L)}
  else if(idxa > idxb){ return(0L)}
  else{print('Cant find a or b')}
}

#different info functions for Top* treatments
infotopab <- function(response, a, b)
{
  res <- str_sub(str_split(response, ']')[[1]][1], 3)
  alt <- strtoi(str_split(res, ", ")[[1]] )
  #check if alt equals a or b
  if(alt == a){return(1L)}
  else if(alt == b){return(0L)}
  else{return(-1L)}
}

# return the predicted probability that a > b (by other voters)
predab <- function(response, treatment, a, b, alpha, beta)
{
  if(treatment == 'RankNone' | treatment == 'TopNone'){return(0.5)}
  
  res <- str_sub(str_split(response,']')[[1]][2], 4)
  pred_alts <- strtoi(str_split(res, ", ")[[1]] )
  if(treatment == 'RankTop' | treatment == 'TopTop')
  {
    if(pred_alts == a){return(alpha)}
    else if(pred_alts == b){return(beta)} #change beta to 1-alpha
    else {return(0.5)}
  }
  
  if(treatment == 'RankRank' | treatment == 'TopRank')
  {
    idxa <- which(pred_alts == a)
    idxb <- which(pred_alts == b)
    if(idxa < idxb){return(alpha)}
    else if(idxa > idxb){return(beta)} #change beta to 1-alpha
    else {print('Same location for prediction report')}
  }
}

Aggregate <- function(information, prediction)
{
  idx1 <- which(information == 1)
  frac1 <- length(idx1)/length(information)
  pred1 <- mean(prediction[idx1])
  
  idx0 <- which(information == 0)
  frac0 <- length(idx0)/length(information)
  pred0 <- mean(1- prediction[idx0])
  
  if(length(idx0) == 0){return(1)}
  if(length(idx1) == 0){return(0)}
  
  if(frac1 > pred1 & frac0 > pred0)
  {print('Multiple alternatives surprisingly popular') 
    if(frac1 - pred1 >= frac0 - pred0)
      return(1)
    else
      return(0)
  }
  
  if(frac1 >= pred1){return(1)}
  else {return(0)}
}

Aggregate.II <- function(information, prediction)
{
  #compute prediction normalized votes for the two alternatives.
  idx1 <- which(information == 1)
  idx0 <- which(information == 0)
  if(length(idx1) == 0){return(0)}
  else if(length(idx0) == 0){return(1)}
  
  #predicted prob of alternative 0
  prediction_0 <- 1 - prediction
  p11 <- mean(prediction[idx1])
  p10 <- mean(prediction[idx0])
  p01 <- mean(prediction_0[idx1])
  p00 <- mean(prediction_0[idx0])
  #compute normalized votes
  nv1 <- length(idx1) / (length(idx1) + length(idx0)) * (1 + p01/p10)
  nv0 <- length(idx0) / (length(idx1) + length(idx0)) * (1 + p10/p01)
  
  if(nv1 >= nv0)
    return(1)
  else
    return(0)
}
#r2 is a list of ordered preferences.
KTDist_new <- function(r1,r2)
{
  res1 <- str_split(str_sub(r1,2),", ")
  res14 <- str_split(res1[[1]][4],"]")
  order1 <- c(res1[[1]][1], res1[[1]][2], res1[[1]][3], res14[[1]][1])
  
  #res2 <- str_split(str_sub(r2,3),", ")
  #res24 <- str_split(res2[[1]][4],"]")
  #order2 <- c(res2[[1]][1], res2[[1]][2], res2[[1]][3], res24[[1]][1])
  
  #true <- strtoi(order1)
  #reported <- strtoi(order2)
  
  #print(order1)
  #print(order2)
  dist <- 0
  for (o in r2)
  {
    loc1 <- match(o[1], order1)
    loc2 <- match(o[2], order1)
    if(loc1 > loc2){dist <- dist + 1}
  }
  
  return(dist)
}

#select winning rank from the tournament
ToRank <- function(lpairs){
  alts <- c()
  for(v in lpairs){
    alts <- c(alts, v[1], v[2])
  }
  alts <- alts %>% unique()
  score <- rep(0, length(alts))
  loc.lpairs <- lpairs
  det.rank <- c()
  
  for(j in 1:3){
    score <- c(rep(0, length(alts)))
    #determine the max scores
    for(v in loc.lpairs){
      pos <- match(v[1], alts)
      score[pos] <- score[pos] + 1
    }
    locs <- which(score == max(score))
    #select a winner uniformly at random
    if(length(locs) == 1)
    {
      winner <- alts[locs]
    }
    else{
      probs <- rep(1,length(locs)) * (1/length(locs))
      
      wpos <- sample(locs,size = 1,prob = probs)
      winner <- alts[wpos]
    }
    
    det.rank <- c(det.rank, winner)
    #remove the winner from the list loc.lpairs
    del.indx <- c()
    for(jj in 1:length(loc.lpairs))
    {
      if(winner %in% c(loc.lpairs[[jj]][1], loc.lpairs[[jj]][2]) ){
        del.indx <- c(del.indx, jj)
      }
    }
    loc.lpairs <- loc.lpairs[-del.indx]
    
  }
  #add the last element
  last.elem <- setdiff(alts, det.rank)
  return(c(det.rank, last.elem))
}

mse_ktdist <- function(d)
{
  #d <- data[indices,]
  
  Q.test.temp <- Q.test
  Q.test.dist.temp <- Q.test.dist
  
  tr <- toString(d[1,"treatment"]$treatment)
  #print(tr)
  for (i in 1:nrow(Q.test.temp)) {
    dfsub <- d %>% filter(domain ==  Q.test[i,]$domain & question == Q.test[i,]$question & treatment == tr)
    #generate table for each pair of alternatives
    pairs <- GenPairs(Q.test.temp[i,]$options)
    ordered_pairs <- list()
    for(v in pairs)
    {
      #print(ordered_pairs)
      v1 <- strtoi(v[1])
      v2 <- strtoi(v[2])
      #solve aggregation problem for pair v (different for top and rank information)
      if(tr %in% c('RankRank', 'RankTop', 'RankNone')) {
        dfsub$information <- map_int(dfsub$response, infoab, v1, v2 )
      }
      else {
        dfsub$information <- map_int(dfsub$response, infotopab, v1, v2 )
      }
      #print('done info')
      dfsub$prediction <- map2_dbl(dfsub$response, dfsub$treatment, predab, v1, v2, alpha_0, beta_0)
      #aggregate using surpringly popular algorithm for two alternatives
      #different for Top* and Rank* alternatives
      if(tr %in% c('RankRank', 'RankTop') ) {
        agg_alt <- Aggregate.II(dfsub$information, dfsub$prediction)
        if(agg_alt == 1)
          ordered_pairs <- c(ordered_pairs, list(c(v1,v2)))
        else
          ordered_pairs <- c(ordered_pairs, list(c(v2, v1)))
      }
      else if(tr %in%  c('RankNone', 'TopNone') ){
        #ignore entries with -1 information
        idx1 <- which(dfsub$information == 1)
        numofones <- length(idx1)
        idx2 <- which(dfsub$information == 0)
        numoftwos <- length(idx2)
        if(numofones > numoftwos)
          ordered_pairs <- c(ordered_pairs, list(c(v1,v2)))
        else
          ordered_pairs <- c(ordered_pairs, list(c(v2, v1)))
      }
      else if(tr %in% c('TopTop', 'TopRank') ){
        nidx <- which(dfsub$information == -1)
        
        agg_alt <- Aggregate.II(dfsub$information[-nidx], dfsub$prediction[-nidx])
        if(agg_alt == 1)
          ordered_pairs <- c(ordered_pairs, list(c(v1,v2)))
        else
          ordered_pairs <- c(ordered_pairs, list(c(v2, v1)))
      }
    }
   
    tempstr <- paste0(ToRank(ordered_pairs), collapse = ", ")
    Q.test.temp[i,tr] <- paste('[',tempstr,']',sep='')
    #Q.test.temp[i,tr] <- paste0(ordered_pairs, collapse = "~")
    #print(Q.test.temp[i,tr])
    #Q.test[i,tr] <- KTDist_new(Q.test[i,]$options, ordered_pairs) 
    
  }#end loop through Questions
  #compute mse over the questions
  
  for (i in 1:nrow(Q.test.temp)) {
    # ord_pairs <- str_split(Q.test.temp[i,tr], '~')[[1]]
    # #print(ord_pairs)
    # list_pairs <- list()
    # for (j in 1:length(ord_pairs)) {
    #   temp <- str_split(ord_pairs[j], ", ")[[1]]
    #   first <- strtoi(str_sub(temp[1], 3))
    #   second <- strtoi(str_split(temp[2], pattern = "\\)")[[1]][1])
    #   #print(c(first,second))
    #   list_pairs <- c(list_pairs, list(c(first,second)) )
    #   
    # }
    Q.test.dist.temp[i,tr] <- KTDist(Q.test.temp[i,]$options, Q.test.temp[i,tr][[1]]) 
    #print(Q.test.dist.temp[i,tr])
    #print(KTDist_new(Q.test[i,]$options, list_pairs) )
  }
  
  return(Q.test.dist.temp %>% select(tr) %>% colMeans())
}


#sample with replacement from each question
sample_q <- function(dfres,dom)
{
  if(dom!='all')
  {
    dfsamp <- dfres %>%  group_by(question) %>% select(question,ID) %>% do(sample_frac(.,replace = TRUE) ) %>% ungroup() 
  }
  else{
    dfsamp <- dfres %>% group_by(question, domain) %>% select(question,domain, ID) %>% do(sample_frac(.,replace = TRUE) ) %>% ungroup() 
  }
  return(dfsamp$ID)
}

mse_ktdist_boot <- function(x)
{
  tr <- x[1]
  dm <- x[2]
  if(dm != 'all')
  {
    idx <- which(df.test$treatment == tr & df.test$domain == dm)
  }
  else
  {
    idx <- which(df.test$treatment == tr)
  }
  B <- 500
  n = length(idx)
  samples <- matrix(0,B,n)
  
  for (i in 1:B) {
    samples[i,] <- sample_q(df.test[idx,], dm)
  }
  #print(samples[1,])
  #print(length(idx))
  boot.stats <- apply(samples, 1, function(p) {return(mse_ktdist(df.test[p,]) ) } )
  se <- sd(boot.stats)
  
  #result <- boot(data = df.test[idx,], statistic = mse_ktdist, R = 100)
  #ret <- boot.ci(result, type = "norm")
  #ymin <- quantile(boot.stats, 0.05)
  #ymax <- quantile(boot.stats, 0.95)
  #y <- (ymin + ymax) / 2
  y <-  Q.test.dist %>% select(tr) %>% colMeans()
  ymin <- y - 1.975*se
  ymax <- y + 1.975*se
  return( paste(y,ymin,ymax,sep = '/') )
}


## Analysis

df <- read_csv('~/Documents/SPVoting-Analysis/Analysis/question_responses.csv')
df$treatment <- as.factor(df$treatment)
levels(df$treatment) <- c('TopNone', 'TopTop', 'TopRank', 'RankNone', 'RankTop', 'RankRank')
df$domain <- as.factor(df$domain)
levels(df$domain) <- c('Geography','Movies','Paintings')

Questions <- df %>% select(c('domain','question','options')) %>% unique()
Questions$top_alternative <- map_int(Questions$options, extract_top_q)

#create training and test set
#samples <- sample(c(1:20), size=5, replace=F)
samples <- c(6,8,5,18,15)
Q.train <- Questions %>% group_by(domain) %>% filter(question %in% samples) %>% ungroup()
Q.test <- Questions %>% group_by(domain) %>% filter(! (question %in% samples)  ) %>% ungroup()
df.train <- df %>% group_by(domain) %>% filter(question %in% samples) %>% ungroup()
df.test <- df %>% group_by(domain) %>% filter(! (question %in% samples)  ) %>% ungroup()


Trts <- tibble(treatment = c('RankRank', 'RankTop', 'RankNone', 'TopRank', 'TopTop', 'TopNone'), domain = c(rep(type_domain, 6)), alpha = c(rep(0.55,6)), beta = c(rep(0.1,6)))
Trts$val <- ' '

if(type_domain != 'all')
{
  Q.train <- Q.train %>% filter(domain == type_domain)
  Q.test <- Q.test %>% filter(domain == type_domain)
  df.train <- df.train %>% filter(domain == type_domain)
  df.test <- df.test %>% filter(domain == type_domain)
}
#Treatments
Q.train$RankRank <- NA
Q.train$RankTop <- NA
Q.train$RankNone <- NA
Q.train$TopRank <- NA
Q.train$TopTop <- NA
Q.train$TopNone <- NA

Q.test$RankRank <- NA
Q.test$RankTop <- NA
Q.test$RankNone <- NA
Q.test$TopRank <- NA
Q.test$TopTop <- NA
Q.test$TopNone <- NA


for(tr in c('RankRank', 'RankTop', 'RankNone', 'TopRank', 'TopTop', 'TopNone'))
{
  cur_dist <- 6
  alpha_0 <- 0.55
  beta_0 <- 0.1
  for(alpha in seq(0.55,0.90,0.025))
  {
    for(beta in seq(0.1,0.45,0.025))
    {
      for (i in 1:nrow(Q.train)) {
        dfsub <- df.train %>% filter(domain ==  Q.train[i,]$domain & question == Q.train[i,]$question & treatment == tr)
        #generate table for each pair of alternatives
        pairs <- GenPairs(Q.train[i,]$options)
        ordered_pairs <- list()
        for(v in pairs)
        {
          v1 <- strtoi(v[1])
          v2 <- strtoi(v[2])
          #solve aggregation problem for pair v (different for top and rank information)
          if(tr %in% c('RankRank', 'RankTop', 'RankNone')) {
            dfsub$information <- map_int(dfsub$response, infoab, v1, v2 )
          }
          else {
            dfsub$information <- map_int(dfsub$response, infotopab, v1, v2 )
          }
          #print('done info')
          dfsub$prediction <- map2_dbl(dfsub$response, dfsub$treatment, predab, v1, v2, alpha, beta)
          #aggregate using surpringly popular algorithm for two alternatives
          #different for Top* and Rank* alternatives
          if(tr %in% c('RankRank', 'RankTop') ) {
            agg_alt <- Aggregate.II(dfsub$information, dfsub$prediction)
            if(agg_alt == 1)
              ordered_pairs <- c(ordered_pairs, list(c(v1,v2)))
            else
              ordered_pairs <- c(ordered_pairs, list(c(v2, v1)))
          }
          else if(tr %in%  c('RankNone', 'TopNone') ){
          #ignore entries with -1 information
            idx1 <- which(dfsub$information == 1)
            numofones <- length(idx1)
            idx2 <- which(dfsub$information == 0)
            numoftwos <- length(idx2)
            if(numofones > numoftwos)
              ordered_pairs <- c(ordered_pairs, list(c(v1,v2)))
            else
              ordered_pairs <- c(ordered_pairs, list(c(v2, v1)))
          }
          else if(tr %in% c('TopTop', 'TopRank') ){
            nidx <- which(dfsub$information == -1)

            agg_alt <- Aggregate.II(dfsub$information[-nidx], dfsub$prediction[-nidx])
            if(agg_alt == 1)
              ordered_pairs <- c(ordered_pairs, list(c(v1,v2)))
            else
              ordered_pairs <- c(ordered_pairs, list(c(v2, v1)))
          }
        }
      #Q.train[i,tr] <- KTDist_new(Q.train[i,]$options, ordered_pairs)
      tempstr <- paste0(ToRank( ordered_pairs), collapse = ", ")
      Q.train[i,tr] <- KTDist(Q.train[i,]$options, paste('[',tempstr,']',sep=''))

    }#end loop through Questions

     new_dist <- as.double( Q.train %>% select(c(tr)) %>% summarise_all(mean) )
     #print(c(alpha,beta,new_dist))
     if(new_dist < cur_dist)
     {
      cur_dist <- new_dist
      alpha_0 <- alpha
      beta_0 <- beta
     }

   }#end beta

 }#end alpha

  print(tr)
  print(c(alpha_0, beta_0))
  #save value
  idx <- which(Trts$treatment == tr)
  Trts[idx,]$alpha <- alpha_0
  Trts[idx,]$beta <- beta_0
  # evaluate on test dataset

 

}

#alpha_0 <- 0.55
#beta_0 <- 0.1

  for(tr in c('RankRank', 'RankTop', 'RankNone', 'TopRank', 'TopTop', 'TopNone'))
  {
    #retrieve parameter values
    idx <- which(Trts$treatment == tr)
    alpha_0 <- Trts[idx,]$alpha 
    beta_0 <- Trts[idx,]$beta
    for (i in 1:nrow(Q.test)) {
      dfsub <- df.test %>% filter(domain ==  Q.test[i,]$domain & question == Q.test[i,]$question & treatment == tr)
      #generate table for each pair of alternatives
      pairs <- GenPairs(Q.test[i,]$options)
      ordered_pairs <- list()
      for(v in pairs)
      {
        v1 <- strtoi(v[1])
        v2 <- strtoi(v[2])
        #solve aggregation problem for pair v (different for top and rank information)
        if(tr %in% c('RankRank', 'RankTop', 'RankNone')) {
          dfsub$information <- map_int(dfsub$response, infoab, v1, v2 )
        }
        else {
          dfsub$information <- map_int(dfsub$response, infotopab, v1, v2 )
        }
        #print('done info')
        dfsub$prediction <- map2_dbl(dfsub$response, dfsub$treatment, predab, v1, v2, alpha_0, beta_0)
        #aggregate using surpringly popular algorithm for two alternatives
        #different for Top* and Rank* alternatives
        if(tr %in% c('RankRank', 'RankTop') ) {
          agg_alt <- Aggregate.II(dfsub$information, dfsub$prediction)
          if(agg_alt == 1)
            ordered_pairs <- c(ordered_pairs, list(c(v1,v2)))
          else
            ordered_pairs <- c(ordered_pairs, list(c(v2, v1)))
        }
        else if(tr %in%  c('RankNone', 'TopNone') ){
          #ignore entries with -1 information
          idx1 <- which(dfsub$information == 1)
          numofones <- length(idx1)
          idx2 <- which(dfsub$information == 0)
          numoftwos <- length(idx2)
          if(numofones > numoftwos)
            ordered_pairs <- c(ordered_pairs, list(c(v1,v2)))
          else
            ordered_pairs <- c(ordered_pairs, list(c(v2, v1)))
        }
        else if(tr %in% c('TopTop', 'TopRank') ){
          nidx <- which(dfsub$information == -1)

          agg_alt <- Aggregate.II(dfsub$information[-nidx], dfsub$prediction[-nidx])
          if(agg_alt == 1)
            ordered_pairs <- c(ordered_pairs, list(c(v1,v2)))
          else
            ordered_pairs <- c(ordered_pairs, list(c(v2, v1)))
        }
      }
      tempstr <- paste0(ToRank( ordered_pairs), collapse = ", ")
      Q.test[i,tr] <- paste('[',tempstr,']',sep='')
      #Q.test[i,tr] <- KTDist_new(Q.test[i,]$options, ordered_pairs)

    }#end loop through Questions
  }

Q.test.dist <- Q.test
 for(tr in c('RankRank', 'RankTop', 'RankNone', 'TopRank', 'TopTop', 'TopNone'))
 {
  for (i in 1:nrow(Q.test)) {
     # ord_pairs <- str_split(Q.test[i,tr], '~')[[1]]
     # #print(ord_pairs)
     # list_pairs <- list()
     # for (j in 1:length(ord_pairs)) {
     #   temp <- str_split(ord_pairs[j], ", ")[[1]]
     #   first <- strtoi(str_sub(temp[1], 3))
     #   second <- strtoi(str_split(temp[2], pattern = "\\)")[[1]][1])
     #   #print(c(first,second))
     #   list_pairs <- c(list_pairs, list(c(first,second)) )
     # 
     # }
     
     Q.test.dist[i,tr] <- KTDist(Q.test[i,]$options, Q.test[i,tr][[1]]) 
     #print(KTDist_new(Q.test[i,]$options, list_pairs) )
   }
 }

Q.test.dist$TopNone <- as.double(Q.test.dist$TopNone)
Q.test.dist$TopTop <- as.double(Q.test.dist$TopTop)
Q.test.dist$TopRank <- as.double(Q.test.dist$TopRank)
Q.test.dist$RankNone <- as.double(Q.test.dist$RankNone)
Q.test.dist$RankTop <- as.double(Q.test.dist$RankTop)
Q.test.dist$RankRank <- as.double(Q.test.dist$RankRank)

#Q.test.dist %>%  summarise(topnone = mean(TopNone), toptop = mean(TopTop), toprank = mean(TopRank), ranknone = mean(RankNone), ranktop = mean(RankTop), rankrank = mean(RankRank) )
#Q.test.dist %>%  group_by(domain) %>% summarise(topnone = mean(TopNone), toptop = mean(TopTop), toprank = mean(TopRank), ranknone = mean(RankNone), ranktop = mean(RankTop), rankrank = mean(RankRank) )

#save dataframes
#suffix <- paste0(iteration, ".csv")
#write_csv(Q.test, paste0('results/Q.test_', suffix))
#write_csv(Q.test.dist, paste0('results/Q.test.dist_', suffix))



print('We are here!!!')
df.test$ID <- seq.int(nrow(df.test))
for(i in 1:nrow(Trts))
{
  Trts[i,"val"] <- mse_ktdist_boot(c(Trts[i,]$treatment, Trts[i,]$domain) )
  print(Trts[i,"val"])
}
Trts <- Trts %>% separate(val, into = c('y','ymin','ymax'), sep='/', convert = TRUE)
print(Trts)
# 
# Trts %>% separate(val, into = c('y','ymin','ymax'), sep='/', convert = TRUE) %>% 
write_csv(Trts, paste0('Trts_', type_domain, '.csv') )
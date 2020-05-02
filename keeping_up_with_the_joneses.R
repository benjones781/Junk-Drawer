#Will we run out of surnames?
#Or, more precisely, will one name become the majority and eventually be the only one left?

#No, probably not. But it was interesting question I thought about during lunch one day.
#So I created a simple game with the following rules:
#1. only males and females from the same generation from different families can get married
#2. offspring always take the male's name
#3. offspring can only come from married couples

#things one could toy around with: generation 0, no common ancestors, number of kids each married couple can have, percent p of males and females which want to get married, etc.
#the default settings here are: all males and females want to get married (p=1), every married couple always generate 3 offspring

#I was going to do a full, fancy writeup with LaTeX and shit, but there's other things I'd like to do.
#If I don't catch up on 'Tiger King', I'll be the laughing stock of the office slack channel.

library(waffle)
library(dplyr)
library(ggthemes)
setwd("~/Desktop")

#starting generation (number and size of families)
generationZero <- function (families, n) {
  generation0 <- data.frame(GENERATION=integer(),
                            FAMILY=character(), 
                            SEX=character(),
                            MARRIED=integer(),
                            stringsAsFactors=FALSE)
  
  for (i in 1:length(families)) {
    tempGen <- data.frame(GENERATION=integer(),
                          FAMILY=character(), 
                          SEX=character(),
                          MARRIED=integer(),
                          stringsAsFactors=FALSE)
    tempGen[1:(2*n), 1] <- 0
    tempGen[1:(2*n), 2] <- families[i]
    tempGen[1:n, 3] <- 'm'
    tempGen[(n+1):nrow(tempGen), 3] <- 'f'
    tempGen[1:(2*n), 4] <- 0
    
    generation0 <- rbind(generation0, tempGen)
  }
  
  return(generation0)
}

#shuffle dataframe
shuffle <- function(gen) {
  genShuffle <- gen[sample(nrow(gen)),]
  return(genShuffle) 
}

#p% of males that wish to get married
malesToBeMarried <- function(gen, p=1) {
  males <- subset(gen, gen$SEX=="m")
  malesMarried <- males[sample(nrow(males), size=p*nrow(males), replace=FALSE),] #in this case, everyone wants to get married, could adjust
  malesMarried <- shuffle(malesMarried)
  return(malesMarried)
}

#p% of females that wish to get married
femalesToBeMarried <- function(gen, p=1) {
  females <- subset(gen, gen$SEX=="f")
  femalesMarried <- females[sample(nrow(females), size=p*nrow(females), replace=FALSE),] #in this case, everyone wants to get married, could adjust
  femalesMarried <- shuffle(femalesMarried)
  return(femalesMarried)
}

#pair males and females
marriage <- function(males, females) {
  
  if (nrow(males) < 1 || nrow(females) < 1) {
    return(-1)
  }
  
  for (i in 1:nrow(males)) {
    if (nrow(males) < 1 || nrow(females) < 1) {
      break
    }
    #attempt to pair up a male and female
    #for each male, attempt to randomly pair up with a female of a different family and remove female (so another male can't also marry to that same female)
    attempt <- try(females <- females[-sample(which(females$FAMILY!=males[i,2]), 1),], silent = TRUE)
    if (class(attempt) == 'try-error') {
      males[i,4] <- -1
    } else {
      males[i,4] <- 1
    }
  }
  males[i,4] <- as.integer(males[i,4]) #might not actualy be necessary, idk
  return(males)
}

#generate offspring from the male/female pairs
nextGeneration <- function (males, i) {
  m <- subset(males, males$MARRIED == 1)
  
  nextGen <- data.frame(GENERATION=integer(),
                        FAMILY=character(), 
                        SEX=character(),
                        MARRIED=integer(),
                        stringsAsFactors=FALSE)
  
  if (nrow(m) < 1) {
    return(-1)
  }
  
  for (e in 1:length(table(m$FAMILY))) {
    tempGen <- data.frame(GENERATION=integer(),
                          FAMILY=character(), 
                          SEX=character(),
                          MARRIED=integer(),
                          stringsAsFactors=FALSE)
    
    offspring <- sum(3*table(m$FAMILY)[e]) #each marriage results in 3 offspring, could adjust
    
    tempGen[1:offspring, 1] <- i
    m$FAMILY <- as.factor(m$FAMILY)
    tempGen[1:offspring, 2] <- levels(m$FAMILY)[e]
    tempGen[1:offspring, 4] <- 0
    
    for (u in 1:nrow(tempGen)) {
      tempGen[u, 3] <- sample(c("m", "f"), 1, replace=TRUE)
    }
    
    nextGen <- rbind(nextGen, tempGen)
  }
  nextGen[, 1] <- as.integer(nextGen[, 1])
  nextGen[, 4] <- as.integer(nextGen[, 4])
  return(nextGen)
}

#given a starting generation, simulate for n generations or until no more legal moves
simulate <- function (startingGen, n) {
  count <- 1
  gen <- shuffle(startingGen)
  genList <- list()
  
  for (i in 1:n) {
    gen <- shuffle(gen)
    mTbM <- malesToBeMarried(gen)
    fTbM <- femalesToBeMarried(gen)
    if (nrow(mTbM) < 1 || nrow(fTbM) < 1) {
      print('nobody wanted to marry or no legal marriages')
      break
    }
    maleMarriages <- marriage(mTbM, fTbM)
    if (maleMarriages == -1 || nrow(maleMarriages) < 1) {
      print('no legal marriages')
      break
    }
    if (nrow(nextGeneration(maleMarriages, count)) < 1 || nextGeneration(maleMarriages, count) == -1) {
      print('0 offspring or no legal marriages')
      break
    }
    gen <- nextGeneration(maleMarriages, count)
    count <- count + 1
    
    genList[[i]] <- data.frame(gen)
  }
  return(genList)
}

#visulize each generation with waffles
visualizeResults <- function (sim) {
  for (i in 1:length(sim)) {
    sim[[i]] %>% 
      count(FAMILY, SEX) -> waffleChartDF
    
    waffleChart <- ggplot(waffleChartDF, aes(fill = SEX, values = n)) +
      geom_waffle(color = "white", size = .5, n_rows = 10, flip = TRUE) +
      facet_wrap(~FAMILY, nrow = 1, strip.position = "bottom") +
      #scale_x_discrete() + 
      #scale_y_continuous(limits = c(0, 10)) + 
      coord_equal(ratio=1) + 
      #scale_y_continuous(labels = function(x) x * 10, expand = c(0,0)) + 
      scale_fill_tableau(name=NULL) + 
      #coord_equal() + 
      labs(title = "Family Population by Sex",
           subtitle = paste("Generation ", i)) + 
      theme(panel.grid = element_blank(),
            axis.line=element_blank(),
            axis.text.x=element_blank(), axis.text.y=element_blank(),
            axis.ticks=element_blank(),
            axis.title.x=element_blank(), axis.title.y=element_blank(),
            legend.position="bottom",
            panel.background=element_blank(), panel.border=element_blank(), panel.grid.major=element_blank(),
            panel.grid.minor=element_blank(),plot.background=element_blank()) + 
      guides(fill = guide_legend(reverse = TRUE))
    
    ggsave(paste('generation_', i, '.pdf', sep = ''), plot = waffleChart)
  }
}

genZero <- generationZero(c('a','b','c','d'), 5)
results <- simulate(genZero, 15)
visualizeResults(results)

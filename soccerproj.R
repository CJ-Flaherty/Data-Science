library(tidyverse)
library(ggplot2)
library(nycflights13)
library(rvest)
library(leaflet)
library(stringr)
library(data.table)
library(shiny)
library(plyr)
library(grid)
library(gridExtra)
skill <- read.csv("newfullskills.csv")

s.burn.url <- "http://www.spotrac.com/epl/burnley-f.c/payroll/"

s.url <- "http://www.spotrac.com/epl/burnley-f.c/payroll/"

years <- 2008:2018

teams <- c("afc-bournemouth","arsenal-f.c", "brighton-hove-albion", "burnley-f.c", "chelsea-f.c",
           "crystal-palace", "everton-f.c", "huddersfield-town", "leicester-city", "liverpool-f.c",
           "manchester-city-f.c", "manchester-united-f.c", "newcastle-united-f.c", "southampton-f.c",
           "stoke-city-f.c", "swansea-city-a.f.c", "tottenham-hotspur-f.c", "watford", 
           "west-bromwich-albion-f.c", "west-ham-united-f.c")


url1 <- "http://www.spotrac.com/epl/"
url2 <-"/payroll/"

sal.data <- vector("list", 20)
index = 1

#for(i in years){
  for(j in teams){
    #i = 2015
    
    url <- paste(url1, j, url2, sep = "")
    
    
    dat <- NULL
    name <- paste(j, "sal", sep = ".")
    
    dat <- url %>%
      read_html() %>% 
      html_nodes("table") %>% 
      .[[1]] %>% 
      html_table() 
    
    assign(name, dat)
    t <- data.frame(dat[[1]], dat[[2]], dat[[3]], dat[[4]], dat[[5]], dat[[6]], dat[[7]])
    
     sal.data[[index]] = t
     index = index +1
    
  }
#}

sal.data <- compact(sal.data)

for(i in length(sal.data)){



  j <- sal.data[[i]]
  white.loc <- str_locate(j[,1], "\t[:alpha:]")[,1]
  names <- str_sub(j[,1], start = (white.loc + 1))
  j[1] <- names
  colnames(j) = c("Name", "Pos.", "Base Salary", "Signing Bonus", "USD Annual Salary", "Annual Salary",
                  " GBP Weekly Salary")
  j[,5] <- as.numeric(gsub('[$,]', '', j[,5]))
  j[,7] <- as.numeric(gsub('[£,]', '', j[,7]))

  tit <- paste("team", 1, sep = "")
  assign(tit, j)
  
}
  








skills <- fread("FullData.csv")
sh <- join(skills, southhampton, by = "Name" , type = "inner", match = "all")

url.start <- "https://www.transfermarkt.co.uk/premier-league/jahrestabelle/wettbewerb/GB1/saison_id/"



prem.teams <- vector("list", length(years))

index = 1

for( i in years){
  
prem.url <- paste(url.start, i , sep = "")

tab <- prem.url %>%
  read_html() %>%
  html_nodes("table") %>% 
  .[[5]] %>%
  html_table() 



t <-as.vector(tab[,3])

prem.teams[[index]] = t
index = index + 1
}
prem.teams

teams <- vector("list", length = 20)
index1 = 1


for( i in 1:20){
  


  
p<- sal.data[[i]]
colnames(p) = c("Name", "Pos.", "GBP Base Salary", "GBP Signing Bonus", "USD Annual Salary", "GBP Annual Salary",
                "GBP Weekly Salary")
white.loc <- str_locate(p$Name, "\t[:alpha:]")[,1]
names <- str_sub(p$Name, start = white.loc + 1)
p$Name <- names

p$`USD Annual Salary` <- as.numeric(gsub('[$,]', '', 
                                                    p$`USD Annual Salary`))
p$`GBP Weekly Salary` <- as.numeric(gsub('[£,]', '', 
                                                    p$`GBP Weekly Salary`))

p$`GBP Signing Bonus` <- as.numeric(gsub('[£,]', '', 
                                         p$`GBP Signing Bonus`))

p$`GBP Base Salary` <- as.numeric(gsub('[£,]', '', 
                                         p$`GBP Base Salary`))

p$`GBP Annual Salary` <- as.numeric(gsub('[£,]', '', 
                                         p$`GBP Annual Salary`))

tit <- paste("team", i, sep = '')

tit <- as.data.frame(tit)

assign(teams[i], p)

#skills.data <- join(skills, tit, by = "Name" , type = "inner", match = "all")





}

for(i in teams){
  View(i)
}

skills <- fread("FullData.csv")

skills <- join(skills, team1, by = "Name", type = "full", match = "all")
skills <- join(skills, team2, by = "Name", type = "full", match = "all")
skills <- join(skills, team3, by = "Name", type = "full", match = "all")
skills <- join(skills, team4, by = "Name", type = "full", match = "all")
skills <- join(skills, team5, by = "Name", type = "full", match = "all")
skills <- join(skills, team6, by = "Name", type = "full", match = "all")
skills <- join(skills, team7, by = "Name", type = "full", match = "all")
skills <- join(skills, team8, by = "Name", type = "full", match = "all")
skills <- join(skills, team9, by = "Name", type = "full", match = "all")
skills <- join(skills, team10, by = "Name", type = "full", match = "all")
skills <- join(skills, team11, by = "Name", type = "full", match = "all")
skills <- join(skills, team12, by = "Name", type = "full", match = "all")
skills <- join(skills, team13, by = "Name", type = "full", match = "all")
skills <- join(skills, team14, by = "Name", type = "full", match = "all")
skills <- join(skills, team15, by = "Name", type = "full", match = "all")
skills <- join(skills, team16, by = "Name", type = "full", match = "all")
skills <- join(skills, team17, by = "Name", type = "full", match = "all")
skills <- join(skills, team18, by = "Name", type = "full", match = "all")
skills <- join(skills, team19, by = "Name", type = "full", match = "all")
skills <- join(skills, team20, by = "Name", type = "full", match = "all")




















blanks <- rep(c(), 9)

sal.pos.data <- data.frame(matrix(NA, nrow = 20, ncol = 9))

colnames(sal.pos.data) <- c("Club", "Def.spending", "Mid.spending", "Att.spending", "GK.spending", 
                           "Def.percent", "Mid.percent", "Att.percent", "GK.percent")

url.s <- "http://www.spotrac.com/epl/"
url.f <- "/positional/"

index = 1

for(j in teams){
  #i = 2015
  
  
  
  url <- paste(url.s, j, url.f, sep = "")
  
  
  dat <- NULL
  name <- j
  
  dat <- url %>%
    read_html() %>% 
    html_nodes("table") %>% 
    .[[1]] %>% 
    html_table() 
  

  sal.pos.data[index,1] <- name 
  sal.pos.data[index,2] <- dat[2,4]
  sal.pos.data[index, 3] <- dat[3,4]
  sal.pos.data[index, 4] <- dat[1,4]
  sal.pos.data[index, 5] <- dat[4,4]
  sal.pos.data[index, 6] <- dat[2,5]
  sal.pos.data[index, 7] <- dat[3,5]
  sal.pos.data[index, 8] <- dat[1,5]
  sal.pos.data[index, 9] <- dat[4,5]
  
  
  index = index +1
  
}

table <- data.frame(matrix(NA, nrow = 20, ncol = 9))

colnames(table) <- c("Club", "Wins", "Draws", "Losses", "Goals.for", "Goals.against", "Goal.diff", 
                     "Points")

goal.dat <- "https://www.bbc.co.uk/sport/football/premier-league/table" %>%
  read_html() %>% 
  html_nodes("table") %>% 
  .[[1]] %>% 
  html_table() 

goal.dat <- goal.dat[-c(21), ]

table$Club <- goal.dat$Team

table$Wins <- goal.dat$W

table$Draws <- goal.dat$D

table$Losses <- goal.dat$L

table$Goals.for <- goal.dat$F

table$Goals.against <- goal.dat$A

table$Goal.diff <- goal.dat$GD

table$Points <- goal.dat$Pts

prem.table <- table



for(i in 15:48){
  icol <- colnames(skill)[i]
  skill[, icol] <- str_replace(skill[, icol],"\\+[0-9]","")
  skill[, icol] <- str_replace(skill[, icol],"\\-[0-9]","")
}


for(i in 15:48){
  icol <- colnames(skill)[i]
  skill[, icol] <- as.numeric(skill[, icol])
  skill[, icol] <- as.numeric(skill[, icol])
  
}


skill.small <- skill


    

transfers.url <- "https://www.transfermarkt.co.uk/premier-league/transfers/wettbewerb/GB1"


transfers.url %>%
  read_html() %>% 
  html_nodes("table") %>% 
  .[[4]] %>% 
  html_table()

sal.pos.data.res <- sal.pos.data

sal.pos.data$Def.spending <- gsub('[£,]', '', sal.pos.data$Def.spending)
sal.pos.data$Def.spending <- as.numeric(sal.pos.data$Def.spending)

sal.pos.data$Mid.spending <- gsub('[£,]', '', sal.pos.data$Def.spending)
sal.pos.data$Mid.spending <- as.numeric(sal.pos.data$Def.spending)

sal.pos.data$Att.spending <- gsub('[£,]', '', sal.pos.data$Def.spending)
sal.pos.data$Att.spending <- as.numeric(sal.pos.data$Def.spending)

sal.pos.data$GK.spending <- gsub('[£,]', '', sal.pos.data$Def.spending)
sal.pos.data$GK.spending <- as.numeric(sal.pos.data$Def.spending)

sal.rank <- sal.pos.data[order(-sal.pos.data$tot.wage),]


sal.pos.data[1,1] <- "Bournemouth"


sal.pos.data <- sal.pos.data %>%
  mutate(tot.wage = Def.spending + Mid.spending + Att.spending)

prem.alpha <- prem.table[order(prem.table$Club),]

sal.alpha <- sal.pos.data[order(sal.pos.data$Club), ]

sal.alpha <- sal.alpha[,-1]

tot.tab <- cbind(prem.alpha,sal.alpha)

tot.tab <- tot.tab[, -9]

tot.tab.res <- tot.tab





tot.tab <- tot.tab.res






tot.tab$Def.Spend.Rank <- NA

order.Def.Rank<-order(-tot.tab$Def.spending)

tot.tab$Def.Spend.Rank[order.Def.Rank] <- 1:nrow(tot.tab)




tot.tab$table.pos <- NA

order.pos <-order(-xtfrm(tot.tab$Points))

tot.tab$table.pos[order.pos] <- 1:nrow(tot.tab)




tot.tab$Att.Spend.Rank <- NA

order.att.rank <-order(-tot.tab$Att.spending)

tot.tab$Att.Spend.Rank[order.att.rank] <- 1:nrow(tot.tab)




tot.tab$goals.for.rank <- NA

order.goals.for.rank <- order(-xtfrm(tot.tab$Goals.for))

tot.tab$goals.for.rank[order.goals.for.rank] <- 1:nrow(tot.tab)



tot.tab$goals.against.rank <- NA

order.goals.against.rank <- order(-xtfrm(tot.tab$goals.against.rank))

tot.tab$goals.against.rank[order.goals.against.rank] <- 1:nrow(tot.tab)

tot.tab$wage.rank <-NA

order.wage.rank <-order(tot.tab$wage.rank)

tot.tab$wage.rank[order.wage.rank] <- 1:nrow(tot.tab)


tot.tab <- tot.tab %>%
  mutate(overunderpos = tot.tab$wage.rank - tot.tab$table.pos)

tot.tab<-tot.tab %>%
  mutate(overundergoalsfor = tot.tab$Att.Spend.Rank - tot.tab$goals.for.rank )

tot.tab <- tot.tab %>%
  mutate(overundergoalsagainst = tot.tab$Def.Spend.Rank - tot.tab$goals.against.rank)






skill %>%
  ggplot(aes(x = Finishing, y = Wage)) +
           geom_point()

skill.small <- skill %>%
  subset(Dribbling <= 100 &Acceleration <= 100 & Jumping <= 100 &Strength <= 100 & Stamina <= 100 & Vision <= 100 & Positioning <= 100 & Finishing <= 100)

library(gplots)

mm <- matrix(tot.tab, ncol = 26)

heatmap.2(x = mm, Rowv = FALSE, Colv = FALSE, dendogram = "none",
          cellnote = mm, notecol = "black", notecex = 2,
          trace = "none", key = FALSE,margins = c(1,26))

library(DT)

tot.tab.pos <- tot.tab %>%
  mutate(oupos = ifelse(overunderpos > 0, 1, 0))



tot.tab.score <- tot.tab %>%
  mutate(oufor = ifelse(overundergoalsfor > 0, 1, 0))

tot.tab.concede <-tot.tab %>%
  mutate(ouconcede = ifelse(overundergoalsagainst > 0, 1, 0 ))



over.under.pos.table <- datatable(tot.tab.pos) %>%
  formatStyle(
    1:ncol(tot.tab.pos),
    target = 'row',
    backgroundColor = styleEqual(c(0,1), c('orangered', 'steelblue'))
  )

over.under.for.table <- datatable(tot.tab.score) %>%
  formatStyle(
    columns = 1:ncol(tot.tab.score),
    target = 'row',
    backgroundColor = styleEqual(c(0,1), c('orangered', 'steelblue'))
    
  )


over.under.concede.table <- datatable(tot.tab.concede) %>%
  formatStyle(
    1:ncol(tot.tab.concede),
    target = 'row',
    backgroundColor = styleEqual(c(0,1), c('orangered', 'steelblue'))
    
  )




url <- "https://www.premierleague.com/stats/top/players/goals?se=79"


url %>%
  read_html() %>% 
  html_nodes("table") %>% 
  .[[1]] %>% 
  html_form()

url %>%
  read_html %>%
  html_form()


test <- google_form("1M9B8DsYNFyDjpwSK6ur_bZf8Rv_04ma3rmaaBiveoUI")
f0 <- html_form(test)[[1]]
f1 <- set_values(f0, entry.564397473 = "abc")


teams <- c("afc-bournemouth","arsenal-f.c", "brighton-hove-albion", "burnley-f.c", "chelsea-f.c",
           "crystal-palace", "everton-f.c", "huddersfield-town", "leicester-city", "liverpool-f.c",
           "manchester-city-f.c", "manchester-united-f.c", "newcastle-united-f.c", "southampton-f.c",
           "stoke-city-f.c", "swansea-city-a.f.c", "tottenham-hotspur-f.c", "watford", 
           "west-bromwich-albion-f.c", "west-ham-united-f.c")






urls <-"http://www.spotrac.com/epl/transfers/"

index2 = 1

trans.data <- vector("list", 20)
for(j in 1:20){


  

  
  url <- paste(urls, teams[j], sep = "")
  
  
  dat <- NULL
  name <- paste(teams[j], "transfer", sep = ".")
  
  dat <- url %>%
    read_html() %>% 
    html_nodes("table") %>% 
    .[[1]] %>% 
    html_table() 
  
  #df <- data.frame(Reduce(rbind, dat))
  
  assign(name, dat)
  
  t <- data.frame(dat[1:8])
  
  trans.data[[index2]] <- t
  index2 = index2 +1
 
  
}

trans.data.fin <- vector("list", 20)

for(q in 1:20){
  q = 1
  tem <- data.frame(tot = tab$Transfer.Fee)
  tab <- trans.data[[q]]
  tab$Transfer.Fee <- as.numeric(gsub('[£,]', '', 
                                                      tab$Transfer.Fee))
  
  #s <- strsplit(trans.data[[j]]$Details[1], "from ", fixed= true)[[1]][2]
  #from <- tolower(substring(s, 1,3))
  ourteam <- tolower(substring(teams[q], 1, 3))
  #tab$Transfer.Fee <- ifelse(tolower(substring(tab$Details, "from ", fixed = true)[[1]][2]) == ourteam, tab$Transfer.Fee, tab$Transfer.Fee * -1)
  tem$tot <- with(tab, ifelse(tolower(substring(tab$Details, "from ")[[1]][2]) == ourteam, -tab$Transfer.Fee, tab$Transfer.Fee))
  
  trans.spend <- sum(tem$transfer)
  
  
  
  name <- paste(ourteam, "transfer.fin", sep = ".")
  
  assign(name, trans.spend)
  

  
}


trans.data.fin.save <- trans.data.fin
 




for(k in 1:20){
  tab <- trans.data.fin[[k]]
  tab %>%
    mutate(transfervalsum <- sum(transferval))
}
trans.data.fin

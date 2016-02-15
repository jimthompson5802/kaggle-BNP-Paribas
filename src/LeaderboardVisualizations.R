###
# Produce visualizations of Leaderboard scores
# Instructions:
#   1) Download Leaderboard zip file 
#   2) Set DATA.DIR to the directonry containing the leaderboard zip file
#   3) Set TEAM.NAME to identifier of one team whose scores are higlighted
#
# output will be a png image called "leaderboard_analysis.png in the current working directory
###

library(lubridate)
library(ggplot2)
library(grid)
library(plyr)

DATA.DIR <- "./data"  # directory where publicleaderboarddata.zip is stored

TEAM.NAME <- "JMT5802"  # specify team name 

# read the leaderboard data
lb.df <- read.csv(unz(paste0(DATA.DIR,"/publicleaderboarddata.zip"),
                       "otto-group-product-classification-challenge_public_leaderboard.csv"),
                  stringsAsFactors=FALSE)

# convert date/time to numeric representation
lb.df$SubmissionDate <- ymd_hms(lb.df$SubmissionDate)

# determine number of teams and leader scores by date/time
lb.df$number.teams <- sapply(1:nrow(lb.df),function(pos,df){length(unique(df[1:pos,"TeamName"]))},lb.df)
lb.df$leader.score <- sapply(1:nrow(lb.df),function(pos,df){min(df[1:pos,"Score"])},lb.df)
lb.df$report.date <- as.Date(lb.df[,"SubmissionDate"])

# get only data during the competition
lb.df <- subset(lb.df,report.date < as.Date("2015-05-19"))

# function to get team standings by day
teamStandings <- function(this.date,lb.df) {
    df <- lb.df[lb.df$report.date <= this.date,]

    df <- ddply(df,.(TeamName),summarize,Score=min(Score))

    df <- df[order(df$Score),]
    team.rank <- 1:nrow(df)
    
    return(cbind(team.rank,df))

}

# function to get score and ranking for specified team as of specified date
teamRanking <- function(this.date,lb.df) {
    standings <- teamStandings(this.date,lb.df)
    
    leader.score <- standings[1,"Score"]
    leader.team <- standings[1,"TeamName"]
    number.teams <- nrow(standings)
    team.name <- standings[standings$TeamName == TEAM.NAME,"TeamName"]
    team.rank <- standings[standings$TeamName == TEAM.NAME,"team.rank"]
    team.score <- standings[standings$TeamName == TEAM.NAME,"Score"]
    
    if (length(team.rank) > 0 ) {
        team.percentile <- 100*(1-team.rank/number.teams)
    } else {
        team.percentile <- NA
        team.rank <- NA
        team.score <- NA
    }
    
    return(data.frame(report.date=this.date,
                leader.team=leader.team,
                leader.score=leader.score, number.teams=number.teams,
                team.rank=team.rank,team.percentile=team.percentile,
                team.score=team.score,stringsAsFactors=FALSE))
}


# get team ranking for specified user by competition day
ll <- lapply(unique(lb.df$report.date),teamRanking,lb.df)
ranking.df <- do.call(rbind,ll)

# determine submission date for last submission
last.submission.date <- as.Date(tail(lb.df,1)$SubmissionDate)

this.theme <- theme(axis.text=element_text(color="black",size=15),
                    axis.title=element_text(color="black",size=15),
                    title=element_text(size=15))

# plot all scores and identify selected team
p1 <- ggplot() + 
    # plot points for all teams except for selected team
    geom_point(data=lb.df[lb.df$TeamName != TEAM.NAME,], aes(x=SubmissionDate, y=Score),color="grey60") +
    # plot scores for selected team
    geom_point(data=lb.df[lb.df$TeamName == TEAM.NAME,],aes(x=SubmissionDate,y=Score),
                                                          pch=18,color="red",size=3) +
    # identify selected team
    geom_text(data=head(lb.df[lb.df$TeamName == TEAM.NAME,],1),aes(x=SubmissionDate, y=Score,
                                                                   vjust=-0.2, hjust=0.5, lineheight=0.8,
                                                                   label=paste("Team:\n",TEAM.NAME),
                                                                   fontface="bold")) +
    ylab("(Better) MLL (Worse)") +
    xlab("Submission Date") +
    ggtitle(paste("Kaggle: Otto Group Product Classification Challenge\nAll Participant Scores as of",last.submission.date)) +
    this.theme

# plot leader score for the first 24 hours of competition
lb24.df <- lb.df[lb.df$SubmissionDate < (lb.df$SubmissionDate[1]+hms("24:00:00")),]
p2 <- ggplot(data=lb24.df) +
    geom_line(aes(x=SubmissionDate,y=leader.score),color="blue", size=1.25) +
#     geom_text(data=head(lb24.df,1),aes(x=SubmissionDate, y=leader.score,
#                                        vjust=1.0, hjust=-0.2, linegeight=0.8,
#                                        label=Score)) +
#     geom_text(data=tail(lb24.df,1), aes(x=SubmissionDate, y=Score,
#                                         vjust=1.0, hjust=1.0, linegeight=0.8,
#                                         angle=0,
#                                         label=leader.score)) +
    ylab("(Better) MLL (Worse)") +
    xlab("Submission Date") +
    ggtitle("Leader Score During First 24 Hours of Competition") +
    this.theme
    
    
# plot selected team vs leader score
p3 <- ggplot(data=ranking.df) +
    #plot leader score
    geom_line(aes(x=report.date,y=leader.score),color="blue",size=1.25) +
    # identify leader
    geom_text(data=head(ranking.df,1),aes(x=report.date, y=leader.score,
                                          vjust=-0.2, hjust=0, lineheight=0.8,
                                          label=paste("Leader"))) +
    # plot slected team score
    geom_line(aes(x=report.date, y=team.score), color="red",size=1.25) +
    # identify selected team
    geom_text(data=head(ranking.df[!is.na(ranking.df$team.score),],1),aes(x=report.date, y=team.score,
                                                                   vjust=2, hjust=-0.4, lineheight=0.8,
                                                                   label=paste(TEAM.NAME))) +
    xlab("Submission Date") +
    ylab("(Better)  MLL  (Worse)") +
    ggtitle(paste("Comparision of\nLeader Score vs. Team:",TEAM.NAME)) +
    this.theme

p4 <- ggplot(ranking.df) +
    geom_bar(aes(x=report.date, y=team.percentile),color="red", fill="red",stat="identity") +
    scale_y_continuous(limits=c(0,100),minor_breaks = seq(0 , 100, 5), breaks = seq(0, 100, 10)) +
    xlim(min(ranking.df$report.date),max(ranking.df$report.date)) +
    xlab("SubmissionDate") +
    ylab("(Lower)  Percentile  (Higher)") +
    ggtitle(paste("Team Standing for",TEAM.NAME)) +
    theme(panel.grid.major.y=element_line(color="grey50", linetype="dashed")) +
    this.theme

    
# display 4 charts on one page
png(filename="./model_results/leaderboard_analysis.png",width=8.5, height=11,units="in",res=600)
grid.newpage()
pushViewport(viewport(layout=grid.layout(2,2)))

print(p1, vp=viewport(layout.pos.row=1, layout.pos.col = 1:2))
# print(p2, vp=viewport(layout.pos.row=2, layout.pos.col = 1))
print(p3, vp=viewport(layout.pos.row=2, layout.pos.col = 1))
print(p4, vp=viewport(layout.pos.row=2, layout.pos.col = 2))
dev.off()

# for Presentation
png(filename="./model_results/score_overview.png", width=11, height=8.5, units="in", res=600)
grid.newpage()
pushViewport(viewport(layout=grid.layout(2,6)))
print(p1, vp=viewport(layout.pos.row=1, layout.pos.col = 1:6))
print(p2, vp=viewport(layout.pos.row=2, layout.pos.col = 2:5))
dev.off()

# for Presentation
png(filename="./model_results/score_overview2.png", width=11, height=8.5, units="in", res=600)
grid.newpage()
pushViewport(viewport(layout=grid.layout(1,2)))
print(p3, vp=viewport(layout.pos.row=1, layout.pos.col = 1))
print(p4, vp=viewport(layout.pos.row=1, layout.pos.col = 2))
dev.off()

# for slides
png(filename="./presentations/score_overview1.png",width=11,height=8.5, units="in",res=600)
print(p1)
dev.off()

png(filename="./presentations/score_overview2.png",width=11,height=8.5, units="in",res=600)
print(p2)
dev.off()


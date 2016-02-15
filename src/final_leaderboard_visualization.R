###
#  Add data for the final standings
#  this assumes that code in LeaderboardVisualizations.R has already run
###

final.date <- "2015-05-19"
post.date <- "2015-05-22"
leader.final.score <- 0.38243
team.final.score <- 0.45987
team.final.position <- 890
team.post.score <- 0.49759
team.post.position <- 1334
total.teams <- 3514

team.final.percentile <- 100*(1-(team.final.position-1)/total.teams)
team.post.percentile <- 100*(1-(team.post.position-1)/total.teams)

leader.final.df <- data.frame(report.date=as.Date(final.date),score=leader.final.score)
team.final.df <- data.frame(report.date=as.Date(final.date),score=team.final.score,
                            percentile=team.final.percentile) 
team.post.df <- data.frame(report.date=as.Date(post.date),score=team.post.score,
                           percentile=team.post.percentile)




p3.final <- p3 + geom_point(data=leader.final.df,aes(x=report.date,y=score), 
                    color="blue3", pch=4,cex=5) +
    geom_point(data=team.final.df,aes(x=report.date,y=score),color="orange3", 
               pch=4,cex=5) +
    annotate("text", x=leader.final.df$report.date, y=leader.final.df$score,
             label="Final Score",color="black",
             hjust=1,vjust=-1) +
    annotate("text",x=team.final.df$report.date, y=team.final.df$score,
             label="Final Score",color="black", hjust=1,vjust=1.2)
#     geom_point(data=team.post.df,aes(x=report.date,y=score),color="red")

print(p3.final)


p4.final <- p4 + geom_bar(data=team.final.df, aes(x=report.date, y=percentile),
                          color="orange3", fill="orange3",stat="identity") +
    annotate("text",x=team.final.df$report.date,y=team.final.df$percentile,
             label=paste0("Final: ",format(team.final.df$percentile,digits=5),"%"), 
             color="black", vjust=.25,hjust=-0.05, angle=90, size=4)
#     geom_bar(data=team.post.df, aes(x=report.date, y=percentile),
#              color="red", fill="red",stat="identity") 
print(p4.final)

    
# display 4 charts on one page
png(filename="./model_results/leaderboard_analysis.png",width=8.5, height=11,units="in",res=600)
grid.newpage()
pushViewport(viewport(layout=grid.layout(2,2)))

print(p1, vp=viewport(layout.pos.row=1, layout.pos.col = 1:2))
# print(p2, vp=viewport(layout.pos.row=2, layout.pos.col = 1))
print(p3.final, vp=viewport(layout.pos.row=2, layout.pos.col = 1))
print(p4.final, vp=viewport(layout.pos.row=2, layout.pos.col = 2))
dev.off()


# for Presentation
png(filename="./model_results/score_overview2.png", width=11, height=8.5, units="in", res=600)
grid.newpage()
pushViewport(viewport(layout=grid.layout(1,2)))
print(p3.final, vp=viewport(layout.pos.row=1, layout.pos.col = 1))
print(p4.final, vp=viewport(layout.pos.row=1, layout.pos.col = 2))
dev.off()

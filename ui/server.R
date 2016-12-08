
library(dplyr)
library(plotly)
library(tidyr)
passing_temp = read.csv("./../data/passing_temp.csv") %>% select(GSIS_ID,TEAM,FULL_NAME,PROFILE_ID,passing_yds,temp)
player = readRDS("./../data/player.rds") %>% filter(status=="Active") %>% filter(position %in% c("RB","QB","WR"))
play_player = readRDS("./../data/play_player.rds") %>% filter(player_id %in% player$player_id)
function(input, output, session) {

    output$teamBox = renderUI( selectInput("team", "Choose Team #1:",
                                            choices =  c("All", c(unique(player[player$status== "Active","team"]))), 
                                            selected = 'All'))
        output$postionBox = renderUI( selectInput("position", "Choose Position:",
                                            choices =  c(unique(player[player$status== "Active" & player$position %in% c("QB","RB","WR"),"position"])), 
                                            selected = 'QB'))
     output$playerBox = renderUI(
       if(input$team == 'All') {
            selectInput("player", "Choose Player #1:",
                                            choices =  sort(player[ player$position==input$position & !is.na(player$full_name) & player$status== "Active","full_name"]),
                                            selected = ifelse(player$position=="QB","Matt Ryan",
                                                              ifelse(player$position=="WR","Julio Jones",
                                                                     ifelse(player$position=="RB","Devonta Freeman",NA))))
           } else {

     selectInput("player", "Choose Player #1:",
                                            choices =  sort(player[player$team == input$team & player$position==input$position & !is.na(player$full_name) & player$status== "Active","full_name"]),
                                          selected = "Matt Ryan")
           })
     
      output$teamBox_p2 = renderUI( selectInput("team_p2", "Choose Team #2:",
                                            choices =  c("All", c(unique(player[player$status== "Active","team"]))), 
                                            selected = 'All'))

     output$playerBox_p2 = renderUI(
       if(input$team_p2 == 'All') {
            selectInput("player_p2", "Choose Player #2:",
                                            choices =  sort(player[ player$position==input$position & !is.na(player$full_name) & player$status== "Active","full_name"]),
                                            selected = "Drew Brees")
           } else {

     selectInput("player_p2", "Choose Player #2:",
                                            choices =  sort(player[player$team == input$team_p2 & player$position==input$position & !is.na(player$full_name) & player$status== "Active","full_name"]),
                                          selected = "Drew Brees")
           })
  
  outputOptions(output, "teamBox", suspendWhenHidden = FALSE)
  outputOptions(output, "postionBox", suspendWhenHidden = FALSE)
  outputOptions(output, "playerBox", suspendWhenHidden = FALSE)
    
  outputOptions(output, "teamBox_p2", suspendWhenHidden = FALSE)
  outputOptions(output, "playerBox_p2", suspendWhenHidden = FALSE)
  getActiveValues = function(){
    return(list(
           id = player[player$full_name==input$player & !is.na(player$full_name),"player_id"],
           id2 = player[player$full_name==input$player_p2 & !is.na(player$full_name),"player_id"],
         position_ids = player[player$status== "Active" & player$position %in% input$position,"player_id"]
    ))}
         
       output$rate <- renderValueBox({
id = getActiveValues()$id
position_ids =getActiveValues()$position_ids
              #play_player[play_player$player_id==
    # wrs_avg= data.frame(avg_yrds=   mean( ,"receiving_yds"]))
      wrs_avg =  play_player %>% 
        filter(player_id %in% position_ids) %>% 
        group_by(gsis_id, player_id) %>% 
        summarise(tot_receiving_yds_per_game = sum(receiving_yds),
                  tot_rushing_yds_per_game  = sum(rushing_yds),
                  tot_passing_yds_per_game  = sum(passing_yds)
                  ) %>% 
       group_by(player_id) %>% 
        summarise(avg_receiving_yds = mean(tot_receiving_yds_per_game),
                  avg_rushing_yds=mean(tot_rushing_yds_per_game),
                  avg_passing_yds=mean(tot_passing_yds_per_game)
                  )
      
      wrs_player_avg =  play_player %>% 
        filter(player_id == id) %>% 
        group_by(gsis_id, player_id) %>% 
        summarise(tot_receiving_yds_per_game = sum(receiving_yds),
                  tot_rushing_yds_per_game  = sum(rushing_yds),
                  tot_passing_yds_per_game  = sum(passing_yds)
                  ) %>%
       group_by(player_id) %>% 
       summarise(avg_receiving_yds = mean(tot_receiving_yds_per_game),
                  avg_rushing_yds=mean(tot_rushing_yds_per_game),
                  avg_passing_yds=mean(tot_passing_yds_per_game)
                  )
        if(input$position=="WR"){
    valueBox(
      value = formatC(wrs_player_avg$avg_receiving_yds[1], digits = 1, format = "f"),
      subtitle = paste(input$player,"Avg Receiving Yds/Game"),
      icon = icon("area-chart"),
      color = if (wrs_player_avg$avg_receiving_yds[1] >=mean(wrs_avg$avg_receiving_yds)) "yellow" else "aqua"
    )
    }else if(input$position=="RB"){
        valueBox(
      value = formatC(wrs_player_avg$avg_rushing_yds[1], digits = 1, format = "f"),
      subtitle = paste(input$player,"Avg Rushing Yds/Game"),
      icon = icon("area-chart"),
      color = if (wrs_player_avg$avg_rushing_yds[1] >=mean(wrs_avg$avg_rushing_yds)) "yellow" else "aqua"
        )}else if(input$position=="QB"){
        valueBox(
      value = formatC(wrs_player_avg$avg_passing_yds[1], digits = 1, format = "f"),
      subtitle = paste(input$player,"Avg Passing Yds/Game"),
      icon = icon("area-chart"),
      color = if (wrs_player_avg$avg_passing_yds[1] >=mean(wrs_avg$avg_passing_yds)) "yellow" else "aqua"
    )}
  })

       output$count <- renderValueBox({
      id2 = getActiveValues()$id2
position_ids =getActiveValues()$position_ids
              #play_player[play_player$player_id==
    # wrs_avg= data.frame(avg_yrds=   mean( ,"receiving_yds"]))
      wrs_avg =  play_player %>% 
        filter(player_id %in% position_ids) %>% 
        group_by(gsis_id, player_id) %>% 
        summarise(tot_receiving_yds_per_game = sum(receiving_yds),
                  tot_rushing_yds_per_game  = sum(rushing_yds),
                  tot_passing_yds_per_game  = sum(passing_yds)
                  ) %>% 
       group_by(player_id) %>% 
        summarise(avg_receiving_yds = mean(tot_receiving_yds_per_game),
                  avg_rushing_yds=mean(tot_rushing_yds_per_game),
                  avg_passing_yds=mean(tot_passing_yds_per_game)
                  )
      
      wrs_player_avg =  play_player %>% 
        filter(player_id == id2) %>% 
        group_by(gsis_id, player_id) %>% 
        summarise(tot_receiving_yds_per_game = sum(receiving_yds),
                  tot_rushing_yds_per_game  = sum(rushing_yds),
                  tot_passing_yds_per_game  = sum(passing_yds)
                  ) %>%
       group_by(player_id) %>% 
       summarise(avg_receiving_yds = mean(tot_receiving_yds_per_game),
                  avg_rushing_yds=mean(tot_rushing_yds_per_game),
                  avg_passing_yds=mean(tot_passing_yds_per_game)
                  )
        if(input$position=="WR"){
    valueBox(
      value = formatC(wrs_player_avg$avg_receiving_yds[1], digits = 1, format = "f"),
      subtitle = paste(input$player_p2,":Avg Receiving Yds/Game"),
      icon = icon("area-chart"),
      color = if (wrs_player_avg$avg_receiving_yds[1] >=mean(wrs_avg$avg_receiving_yds)) "yellow" else "aqua"
    )
    }else if(input$position=="RB"){
        valueBox(
      value = formatC(wrs_player_avg$avg_rushing_yds[1], digits = 1, format = "f"),
      subtitle = paste(input$player_p2,":Avg Rushing Yds/Game"),
      icon = icon("area-chart"),
      color = if (wrs_player_avg$avg_rushing_yds[1] >=mean(wrs_avg$avg_rushing_yds)) "yellow" else "aqua"
        )}else if(input$position=="QB"){
        valueBox(
      value = formatC(wrs_player_avg$avg_passing_yds[1], digits = 1, format = "f"),
      subtitle = paste(input$player_p2,":Avg Passing Yds/Game"),
      icon = icon("area-chart"),
      color = if (wrs_player_avg$avg_passing_yds[1] >=mean(wrs_avg$avg_passing_yds)) "yellow" else "aqua"
    )}
       })
       
      output$dashboard_plot1 = renderPlotly({
        id = getActiveValues()$id
        id2 = getActiveValues()$id2
        position_ids =getActiveValues()$position_ids
        dat  =  play_player %>%
filter(player_id %in% c(id,id2)) %>%
group_by(gsis_id, player_id) %>%
summarise(tot_receiving_yds_per_game = sum(receiving_yds),
tot_rushing_yds_per_game  = sum(rushing_yds),
tot_passing_yds_per_game  = sum(passing_yds)
) %>% mutate(game = substr(gsis_id, 1,8)) %>% ungroup() %>% 
  mutate(gamewk =format(as.Date(game, format= "%Y%m%d") + 1,"%Y%U")) %>%
  group_by(player_id, gamewk) %>%         summarise(tot_receiving_yds_per_game = sum(tot_receiving_yds_per_game),
                  tot_rushing_yds_per_game  = sum(tot_rushing_yds_per_game),
                  tot_passing_yds_per_game  = sum(tot_passing_yds_per_game)
                  ) 

get2plyrstats = function(dat, statin){
  dat2 = dat %>% select_("gamewk", "player_id", statin) %>% 
    spread_("player_id", statin) %>% 
    select(1, p1 = 2, p2 = 3) %>% group_by(gamewk) %>% 
    summarise(p1_yrds = sum(p1), p2_yrds = sum(p2))
  
  dat2[is.na(dat2)] = 0 
  return(dat2)
}

choose_pos <- function(position) {
  switch(position,
         WR = "tot_receiving_yds_per_game",
         QB = "tot_passing_yds_per_game",
         RB = "tot_rushing_yds_per_game")
}

dat2 = get2plyrstats(dat, choose_pos(input$position))


 p = plot_ly(dat2, y = p1_yrds, x = gamewk, name = input$player) 
 p <- add_trace(p,y=p2_yrds, x = gamewk, name = input$player_p2) %>% layout(xaxis=list(type='category'))
 p
    
      })
   
        output$splashImage <- renderImage({
    # A temp file to save the output.
    # This file will be removed later by renderImage
    # Return a list containing the filename
    list(src = "./../public/splash.png",
         contentType = 'image/png',
         width = 500,
         height = 400,
         alt = "This is alternate text")
  }, deleteFile = FALSE)
        
               output$dataflowImage <- renderImage({
    # A temp file to save the output.
    # This file will be removed later by renderImage
    # Return a list containing the filename
    list(src = "./../public/dataflow.png",
         contentType = 'image/png',
         width = 400,
         height = 300,
         alt = "This is alternate text")
  }, deleteFile = FALSE)
  
  
}



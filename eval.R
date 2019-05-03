eval=function(p1,p2,num.games=100,verbose=FALSE){
  p1_wins = 0
  p2_wins = 0
  total_turns = 0
  for (i in 1:num.games){
    game = play.game(p1,p2)
    winner = game$winner
    turns = game$turns
    #print(paste("Game",i,"took",turns,"turns"))
    if (winner == 1){
      p1_wins = p1_wins + 1
    } else {
      p2_wins = p2_wins + 1
    }
    total_turns = total_turns + turns
  }
  return(list(p1=(p1_wins/num.games),avg_turns=(total_turns/num.games)))
}


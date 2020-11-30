##The optimal stopping problem
set.seed(9527)

##A Model for "Strict Success"
simulate_dating=function(population=100,
                         phase1_fraction=1/exp(1), #先找出前幾個（比率）
                         num_iterations=1) {
  #is_soulmate=logical(length=num_iterations)
  is_soulmate=rep(0,length=num_iterations)
  phase1_size=round(population*phase1_fraction) #總數*比率 = 先從前N個找出分數最好的
  #
  for(case_idx in 1:num_iterations) {
    #scores of potential mates
    score=rnorm(population)
    optimal_score=max(score)
    #
    # we date the first phase1_size people
    # and note the maximum score in that group
    cutoff_score = max(score[1:phase1_size])
    #
    # now select as your life partner the next date with a better score
    spouse_index=phase1_size +
      which(score[(phase1_size+1):population] 
            > cutoff_score)[1]
    
    # pick the last one if nobody better came along before then
    if (is.na(spouse_index)) {
      spouse_index=population
    }
    
    is_soulmate[case_idx]=
      (score[spouse_index]==optimal_score)
  }
  mean(is_soulmate)
}


phase1_fractions=seq(0.05, 0.95, 0.01)
means=rep(NA,length(phase1_fractions))
for(idx in 1:length(phase1_fractions)) {
  means[idx]=simulate_dating(
    population=100,
    phase1_fraction=phase1_fractions[idx],
    num_iterations=10000)
}


plot(means ~ phase1_fractions)
phase1_fractions[which.max(means)]




##A model for "Average Score"
simulate_dating_2=function(population=1000,
                           phase1_fraction=1/exp(1),
                           num_iterations=1){
  spouse_scores=rep(NA, num_iterations)
  phase1_size=round(population*phase1_fraction)
  
  for(case_idx in 1:num_iterations){
    #scores of potential mates
    score=rnorm(population)
    
    #we date the first phase1_size people
    #and note the maximum score in that group
    cutoff_score=max(score[1:phase1_size])
    optimal_score=max(score)
    
    # now select as your life partner the next date with a better score
    spouse_index=phase1_size +
      which(score[(phase1_size+1):population] 
            > cutoff_score)[1]
    
    # pick the last one if nobody better came along before then
    if(is.na(spouse_index)) {
      spouse_index=population
    }
    
    spouse_scores[case_idx]=score[spouse_index]
  }
  
  mean(spouse_scores)
}

phase1_fractions=seq(0.05, 0.95, 0.01)
means=rep(NA, length(phase1_fractions))
for(idx in 1:length(phase1_fractions)) {
  means[idx]=simulate_dating_2(
    population=100,
    phase1_fraction=phase1_fractions[idx],
    num_iterations=10000)
}

plot(means ~ phase1_fractions)
phase1_fractions[which.max(means)]
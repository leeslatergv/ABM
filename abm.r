Staff1 <- data.frame(AgentNo = 1,
                    State = "U",
                    Experience = 0,
                    Motivation = runif(1,0,1))

# Create population
nPop1 = 10
for(i in 2:nPop1){
    Staff2 <- data.frame(AgentNo = i,
                    State = "U",
                    Experience = 0,
                    Motivation = runif(1,0,1))
    Staff1 <- rbind(Staff1, Staff2)
}
Staff1

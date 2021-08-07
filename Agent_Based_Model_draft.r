# Recruitment
# 1. Firm has vacancy [x]
# 2. Firm advertises vacancy [x]
# 3. Individuals look at vacancy based on Motivation [x]
# 4. If the wage offered is greater than their current one, they apply [x]
# 5. Firms selct three candidates with most Experience [x]
# 6. If number of vacancies > applicants, offer to all applicants.  [?]
# 7. If vacancies < applicants, offer based on motivation and experience with some random. [x]
# 8. Indivdiduals accept and change employer + employer has one less vacancy. [x]

# # Retention
# Properties
# Parameters and attributes
# Behaviours

# THings that need doing
# -- account for people who move although are in employment - make this condiitonal on income difference,
# and make sure if unsuccessful, they don't lose their job


pacman::p_load(tidyverse, gdata)

rm(list=ls())

# Parameters
timeframe = 3
nPop1 = 10
nTrust1 = 10


# Starting data frames
Trust1 <- data.frame(TrustNo = 1,
                     Region = "Yorkshire & the Humber",
                     Size = 5000,
                     Vacancies = 3,
                     JobAdverts = 0,
                     WageOffered = 25000)

Staff1 <- data.frame(StaffNo = 1,
                    State = "Unemployed",
                    Experience = 0,
                    Motivation = runif(1,0,1),
                    Income = 0,
                    WorksForTrust = 0)

# # Create population of Trusts
# Create some element of wage difference between trusts
WageOffered <- function(x){
    round(runif(1, 25000, 28000))
}
# Generate vacancies in trusts
Vacanciesfunction <- function(x){
    round(runif(1, 3, 5))
}
# Generate some regions to draw from
Region = c("Yorkshire & the Humber", "London & South East",
 "North West", "North East", "Midlands", "South West")
# Now create the trusts
for (i in 2:nTrust1) {
    Trust2 <- data.frame(TrustNo = i,
                     Region = sample(Region, 1, replace = TRUE),
                     Size = 5000,
                     Vacancies = Vacanciesfunction(.),
                     JobAdverts = 0,
                     WageOffered = WageOffered(.))
    Trust1 <- rbind(Trust1, Trust2)
}
    Trust1$TrustNo <- as.numeric(rownames(Trust1))


# Run through the recruitment cycle over 'timeframe' number of years
for (time in 1:timeframe){
# Add new vacancies to Trust
for (newvacancies in 1:(nrow(Trust1))){
    Trust1$Vacancies[newvacancies] <- Trust1$Vacancies[newvacancies] + round(runif(1,1,3),0)
}
# Generate new agents. Fresh graduates from uni.
for (i in 2:nPop1) {
    # Staff come out of uni the same, but differ in motivation.
    Staff2 <- data.frame(StaffNo = i,
                    State = "Unemployed",
                    Experience = 0,
                    Motivation = runif(1,0,1),
                    Income = 0,
                    WorksForTrust = 0)
    Staff1 <- rbind(Staff1, Staff2)
    
}
# Update staff number. Probably a better way of doing this in the loop.
Staff1$StaffNo <- as.numeric(rownames(Staff1))

# Run through the trusts
for (i in 1:(nrow(Trust1))){
    # Determine if trust has vacancy
    TrustVacancy1 <- Trust1$Vacancies[i]
    # If have vacancy, post one job advert. 
    # Trusts can't advertise more posts than vacancies.
    if((TrustVacancy1 >= 1) & TrustVacancy1 > Trust1$JobAdverts[i]) {
        Trust1$JobAdverts[i] <- Trust1$JobAdverts[i] + 1
    }
}

# Run through the staff
for (j in 1:(nrow(Staff1))){
    # Create a 'chance of applying' based on some
    # random element and their own motivation
    ChanceOfApplying <- (runif(1,0,1) + Staff1$Motivation[j]) / 2
    # Unemployed staff have a better chance of applying
    if (Staff1$State[j] == "Unemployed") {
        if(ChanceOfApplying > 0.4 & sum(Trust1$Vacancies) > 0) {
            Staff1$State[j] <- "Applying"
    } 
    # Employed staff have a higher threshold for applying.
    # Could add condition here around wage difference.
    if (Staff1$State[j] == "Employed") {
        if(ChanceOfApplying > 0.75 & sum(Trust1$Vacancies) > 0){
            Staff1$State[j] <- "Applying"
        }
    }
    }
}
   # Grab those who have applied and the vacancies
    JobAppliers <- Staff1[ which(Staff1$State == "Applying"), ]
    JobVacancies <- Trust1[ which(Trust1$Vacancies > 0), ]
    # Set up blank columns which will be populated by who is applying.
    JobAppliers$AppliedTo <- "0"
    JobVacancies$AppliedBy <- "0"
# For all of those who are applying for job
for (k in 1:(nrow(JobAppliers))) {
    # Take their current income
    CurrentIncome <- JobAppliers$Income[k]
    # See which jobs are more than their current income
    EligibleJobs <- JobVacancies[ which(JobVacancies$WageOffered >
     CurrentIncome), ]
    # Apply to one of these eligible jobs, being more likely to apply to higher
    # salary roles
    ApplyTo <- EligibleJobs[sample(nrow(EligibleJobs), 1, replace = FALSE, prob = JobVacancies$WageOffered), -ncol(EligibleJobs)]
    # Update field to return which job they've applied for
    JobAppliers$AppliedTo[k] <- ApplyTo$TrustNo
}

#  Now we want to select, for each vacancy, one of those who have appled
#  This should be reflective of their motivation, experience, and some
#  stochastic element
SuccessfulApplicantAgg <- data.frame()
# Run through the trusts
for (l in 1:(nrow(Trust1))) {
    # For trusts that have a vacancy
    if (Trust1$Vacancies[l] != 0){
    # Take the trust number of the trust with a vacancy
    FindAppliers <- Trust1$TrustNo[l]
    # Get those individuals who applied to that vacancy
    FindAppliers1 <- JobAppliers[which(JobAppliers$AppliedTo == FindAppliers
        ), ]
        # If the vacancy actually had someone apply for it
        if (dim(FindAppliers1)[1] != 0) {
            # Assign them a probability of being offered job - a combination
            # of experience motivation and a stochastic element
            FindAppliers1$Probability <- (FindAppliers1$Experience +
            FindAppliers1$Motivation + runif(1,0,1))
            # Find a successful applicant from those that applied, based on the
            # probability variable you just created (creates another stochastic
            # element, designed to simulate things like interview prep,
            # unconscious bias)
            SuccessfulApplicant <- FindAppliers1[sample(nrow(FindAppliers1),
            1, replace = FALSE, prob = FindAppliers1$Probability), ]
            # Also create chance of unsuccessful interview. If the
            # probability variable is lower than 0.7, we take this
            # to mean that even the preferred candidate from interview
            # was not offered a role. The vacancy remains unfilled.
            SuccessfulApplicant <- SuccessfulApplicant[which(SuccessfulApplicant$Probability > 0.7), ]
            SuccessfulApplicantAgg <- rbind(SuccessfulApplicantAgg, SuccessfulApplicant)
        }
    }
}

# Now we need to take the successful applicants, update the trusts
# vacancy figures, size, adverts for those who have had successful
#  appointees, and address the employment status and income 

# Running through the successful applicants
for (a in 1:(nrow(SuccessfulApplicantAgg))) {
    # Save the Trust number of Trust the successful applicant will join.
    TrustLookUp <- SuccessfulApplicantAgg$AppliedTo[a]
    # Save the staff number of the new employee.
    StaffLookUp <- SuccessfulApplicantAgg$StaffNo[a]
    # Save their new income - taken from the Trust that offered them the job.
    SuccessfulApplicantAgg$Income[a] <- Trust1$WageOffered[Trust1$TrustNo == TrustLookUp]
    # Take this income and store it in the main Staff1 data.frame. Match with staff number.
    Staff1$Income[Staff1$StaffNo == StaffLookUp] <- SuccessfulApplicantAgg$Income[a]
    # Update the Staff1 dataframe with their new employment status.
    Staff1$State[Staff1$StaffNo == StaffLookUp] <- "Employed"
    # Update which Trust they now work for.
    Staff1$WorksForTrust[Staff1$StaffNo == StaffLookUp] <- SuccessfulApplicantAgg$AppliedTo[a]
    # Update details for the trust - the new number number of employes,
    # take the job advert down, and remove a vacancy as this has now been filled.
    Trust1$Size[Trust1$TrustNo == TrustLookUp] <- Trust1$Size[Trust1$TrustNo == TrustLookUp] + 1
    Trust1$JobAdverts[Trust1$TrustNo == TrustLookUp] <- Trust1$JobAdverts[Trust1$TrustNo == TrustLookUp] - 1
    Trust1$Vacancies[Trust1$TrustNo == TrustLookUp] <- Trust1$Vacancies[Trust1$TrustNo == TrustLookUp] - 1
}
# For those unsuccessful and without a job, change them from 'applying' to 'unemployed'
Staff1$State[Staff1$Income == 0] <- "Unemployed"

for (i in 1:(nrow(Staff1))) {
# A year passes. For those employed...
    if (Staff1$State[i] == "Employed"){
    # Employees get between 1-3% annual national pay increase.
    # You could get this to reflect 
    Staff1$Income[i] <- round(Staff1$Income[i] * (runif(1,1.01,1.03)),0)
    # Add an extra year of experience for those who are employed
    Staff1$Experience[i] <- Staff1$Experience[i] + 1
    }
}
}


library(knitr)
library(tidyverse)

# Preparing and cleaning the data
dat <- read.csv("/Users/mac/Desktop/PresidentialElection/2020polls_MC.csv")
head(dat, 5)

diff_prop <- dat$Prop_Biden - dat$Prop_Trump
polls_dat <- cbind(dat, diff_prop)
head(polls_dat, 5)

# Function to randomly generate a value uniformly distributed 
#   with specified input parameters
# Return the generated value
sim_adj_diff <- function(min_diff, max_diff) {
  
  adj_diff <- runif(n = 1, min = min_diff, max = max_diff)
  
  # Assume the State will rerun the election when a tie happens
  while (adj_diff == 0) {
    adj_diff <- runif(n = 1, min = min_diff, max = max_diff)
  }
  
  return (adj_diff)
}


# Function to set up the original poll data based on the margin of error.
# This greatly improved the computational efficiency
new_poll_df <- function(poll_df, mr_error) {
  
  # Calculate the lower and upper bound of the proportion of votes in the poll
  min_diff <- poll_df$diff_prop - mr_error 
  max_diff <- poll_df$diff_prop + mr_error
  poll_with_mr <- cbind(poll_df, min_diff, max_diff)
  
  return (poll_with_mr)
}


# Function to simulate an election in all electoral States
# Return an array of votes received by both candidates
sim_vote_usa <- function(poll_with_mr) {
  count_B_votes <- 0
  count_T_votes <- 0
  
  # For each State, simulate an election
  for (i in 1:dim(poll_with_mr)[1]) {
    min_unif <- poll_with_mr$min_diff[i]
    max_unif <- poll_with_mr$max_diff[i]
    
    # Check if the electoral State has strong partisanship
    # If the proportion of votes received by Biden adjusted by margin of error 
    #   still indicate no chance for Biden to win the State (max_unif < 0), 
    #   then Trump wins the State directly. 
    #   This reduced computational time
    if (max_unif < 0) {
      count_T_votes <- count_T_votes + poll_with_mr$Seats[i]
    } else if (min_unif > 0) {
      count_B_votes <- count_B_votes + poll_with_mr$Seats[i]
    } else {
      
      # Assume the adjusted difference in proportions to be uniformly distributed
      #   in [prop_B - prop_T margin of error, prop_B - prop_T + margin of error]
      adj_diff <- sim_adj_diff(min_unif, max_unif)
      
      # We assume Biden wins a State if adj_diff > 0
      # Note that adj_diff is a non-zero value
      if (adj_diff > 0) {
        count_B_votes <- count_B_votes + poll_with_mr$Seats[i]
      } else if (adj_diff < 0) {
        count_T_votes <- count_T_votes + poll_with_mr$Seats[i]
      }
    }
  }
  
  return (array(c(count_B_votes, count_T_votes)))
}


# Set seed to ensure the results can be reproducible
set.seed(538)

# Number of iterations per trial
N <- 10000

# Total number of trials
total_trial <- 10

# Function to perform 10 trials, with each trial has 
#   10000 Monte Carlo simulations
# Return a data frame that record the averaged probability 
#   of winning and the averaged total votes for two candidates
MC_simulate <- function(poll_df, mr_err) {
  
  # Step 1: Getting the Poll Data Frame with lower and upper bound
  poll_with_mr <- new_poll_df(poll_df, mr_err)
  
  # Matrix to record the averaged probability of winning and
  #   the averaged total votes for two candidates
  montecarlo_results <- matrix(nrow = total_trial, ncol = 4)
  
  # Step 2: For each trail, perform 10000 simulations of Presidential Election
  #         Keep track of the winning times of both candidates
  for (i in 1:total_trial) {
    
    # Initialize the count of winning times for both candidates
    count_B_trial <- 0
    count_T_trial <- 0
    count_tie_trial <- 0
    
    # Array to record the total votes in each simulation
    record_biden_votes <- array() 
    record_trump_votes <- array()
    
    # Step 3: For each Electoral State, simulate an election;
    #         Record the votes of both candidates;
    #         Repeat for 10000 times
    for (j in 1:N) {
      temp_election <- sim_vote_usa(poll_with_mr)
      
      # Winner needs to have more than 270 electoral votes
      if (temp_election[1] > 270) {
        count_B_trial <- count_B_trial + 1
      } else if (temp_election[2] > 270) {
        count_T_trial <- count_T_trial + 1
      } else {
        # A tie
        count_tie_trial <- count_tie_trial + 1
      }
      
      # Arrays to record the total votes received from each simulation
      record_biden_votes <- append(record_biden_votes, temp_election[1])
      record_trump_votes <- append(record_trump_votes, temp_election[2])
    }
    
    # Step 4: Calculate the averaged probability of winning for both candidates
    montecarlo_results[i, 1] <- count_B_trial / N
    montecarlo_results[i, 2] <- count_T_trial / N
    montecarlo_results[i, 3] <- mean(record_biden_votes, na.rm = TRUE)
    montecarlo_results[i, 4] <- mean(record_trump_votes, na.rm = TRUE)
    
    # Rename the columns
    colnames(montecarlo_results) <- c("Est_Prob_Biden", "Est_Prob_Trump", 
                                      "Est_Votes_Biden", "Est_Votes_Trump")
  }
  
  # Return the resulted data frame
  montecarlo_results <- as.data.frame(montecarlo_results)
  
  return(montecarlo_results)
}


############################################################################
###  The following code simulates a Monte Carlo election with MR = 0.16  ###
############################################################################

# Initialize the parameters, margin of error = 0.16
mr_err <- 0.16
example_mr_016 <- new_poll_df(polls_dat, mr_err)


# The resulted data frame includes the lower and upper bound of proportion of 
#   votes. Since the function $MC_simulate(mr_err)$ already include the code, we 
#   don't need to perform this code for each margin of error. 


# Now perform the Monte Carlo simulations, 
#   for 10 trail, with 10000 simulated Presidential election in each trail
df_mr_016 <- MC_simulate(polls_dat, mr_err)
# This is a very computational expensive process
# This code (already improved) took about 5 minutes to complete the task
# Information about the Version of R will be provided in the Appendix 5.2
# Now we save the resulted file, and retrieved later
saveRDS(df_mr_016, "/Users/mac/Desktop/PresidentialElection/election_MR_0.16_CLT.csv")
# df_mr_016 <- readRDS("/Users/mac/Desktop/PresidentialElection/election_MR_016.csv")

head(df_mr_016)

# Save the results in to local file
# Note that the result is a data.frame, which may not be opened directly
df_mr_016

hist(df_mr_016$Est_Votes_Biden,
     xlab = "Total Electoral Votes",
     main = "Histogram of electoral votes for Biden")

mean(df_mr_016$Est_Votes_Biden)   # 339.6409
mean(df_mr_016$Est_Votes_Trump)   # 198.3591
mean(df_mr_016$Est_Prob_Biden)    # 0.982095
mean(df_mr_016$Est_Prob_Trump)    # 0.013845

############################################################################
###                       End of The Demonstration                       ###
############################################################################


############################################################################
###  Repeat the same process, with Margin of error 0.04, 0.08, and 0.12  ###
############################################################################

df_mr_004 <- MC_simulate(polls_dat, 0.04)
df_mr_008 <- MC_simulate(polls_dat, 0.08)
df_mr_012 <- MC_simulate(polls_dat, 0.12)
saveRDS(df_mr_004, "/Users/mac/Desktop/PresidentialElection/election_MR_0.04_CLT.csv")
saveRDS(df_mr_008, "/Users/mac/Desktop/PresidentialElection/election_MR_0.08_CLT.csv")
saveRDS(df_mr_012, "/Users/mac/Desktop/PresidentialElection/election_MR_0.12_CLT.csv")

result_004 <- readRDS("/Users/mac/Desktop/PresidentialElection/election_MR_0.04_CLT.csv")
result_008 <- readRDS("/Users/mac/Desktop/PresidentialElection/election_MR_0.08_CLT.csv")
result_012 <- readRDS("/Users/mac/Desktop/PresidentialElection/election_MR_0.12_CLT.csv")
result_016 <- readRDS("/Users/mac/Desktop/PresidentialElection/election_MR_0.16_CLT.csv")

# Matrix to store the averaged votes for both candidate for each margin of error
votes_all_mr <- cbind(sort(result_004$Est_Votes_Biden), sort(result_008$Est_Votes_Biden),
                      sort(result_012$Est_Votes_Biden), sort(result_016$Est_Votes_Biden),
                      sort(result_004$Est_Votes_Trump), sort(result_008$Est_Votes_Trump),
                      sort(result_012$Est_Votes_Trump), sort(result_016$Est_Votes_Trump))
colnames(votes_all_mr) <- c("VB_0.04", "VB_0.08", 
                            "VB_0.12", "VB_0.16", 
                            "VT_0.04", "VT_0.08",
                            "VT_0.12", "VT_0.16")
votes_all_mr <- as.data.frame(votes_all_mr)

head(votes_all_mr, 5)
# saveRDS(votes_all_mr, "/Users/mac/Desktop/PresidentialElection/election_all_MR.csv")

par(mfrow = c(1, 2))
vote_boxplot1 <- boxplot(votes_all_mr[, 1:4],
                         xlab = "Votes received by each candidate")

vote_boxplot1 <- boxplot(votes_all_mr[, 2:3],
                         xlab = "Votes received by Trump")

vote_boxplot2 <- boxplot(votes_all_mr[, 4:6],
                         xlab = "Votes received by Trump")

# Matrix to store the averaged prob of winning for both candidate for each margin of error
prob_all_mr <- cbind(sort(result_004$Est_Prob_Biden), sort(result_008$Est_Prob_Biden),
                     sort(result_012$Est_Prob_Biden), sort(result_016$Est_Prob_Biden),
                     sort(result_004$Est_Prob_Trump), sort(result_008$Est_Prob_Trump),
                     sort(result_012$Est_Prob_Trump), sort(result_016$Est_Prob_Trump))
colnames(prob_all_mr) <- c("PB_0.04", "PB_0.08", 
                           "PB_0.12", "PB_0.16", 
                           "PT_0.04", "PT_0.08",
                           "PT_0.12", "PT_0.16")

prob_all_mr <- as.data.frame(prob_all_mr)
prob_all_mr
# saveRDS(prob_all_mr, "/Users/mac/Desktop/PresidentialElection/election_prob_all_MR.csv")
kable(prob_all_mr)

par(mfrow = c(1, 2))

prob_boxplot1 <- boxplot(prob_all_mr[, 1:3],
                         xlab = "Margin of Error",
                         main = "Estimated probability that Biden wins",
                         ylab = "Prob")

prob_boxplot2 <- boxplot(prob_all_mr[, 4:6],
                         xlab = "Margin of Error",
                         main = "Estimated probability that Trump wins",
                         ylab = "Prob")


###################################################################################
###  The following code simulates 1000 elections for 1000 times with MR = 0.12  ###
###################################################################################
total_trial <- 1000
N <- 1000
clt_montecarlo <- MC_simulate(polls_dat, 0.12)
saveRDS(clt_montecarlo, "/Users/mac/Desktop/PresidentialElection/election_MR_0.12_clt_1000.csv")

# The R code for 1000 runs with 1000 simulations for each run
clt_montecarlo <- readRDS("/Users/mac/Desktop/PresidentialElection/election_MR_0.12_clt_1000.csv")

# The histogram of electoral votes for Biden and Trump
# This shows the Central Limit Theorem
clt_hist1 <- hist(clt_montecarlo$Est_Votes_Biden, 
                  xlab = "Electoral votes",
                  main = "Electoral votes for Biden")

clt_hist2 <- hist(clt_montecarlo$Est_Votes_Trump, 
                  xlab = "Electoral votes",
                  main = "Electoral votes for Trump")

clt_hist3 <- hist(clt_montecarlo$Est_Prob_Biden,
                  main = "Probability of voting for Biden",
                  xlab = "Prob")

clt_hist4 <- hist(clt_montecarlo$Est_Prob_Trump, 
                  main = "Probability of voting for Trump",
                  xlab = "Prob")

# Calculate the confidence interval
mean(clt_montecarlo$Est_Votes_Biden)    # 342.2171
mean(clt_montecarlo$Est_Votes_Trump)    # 195.7829
mean(clt_montecarlo$Est_Prob_Biden)     # 0.989433
mean(clt_montecarlo$Est_Prob_Trump)     # 0.00783

sd(clt_montecarlo$Est_Votes_Biden)      # 1.018773
sd(clt_montecarlo$Est_Votes_Trump)      # 1.018773
sd(clt_montecarlo$Est_Prob_Biden)       # 0.002803026
sd(clt_montecarlo$Est_Prob_Trump)       # 0.003221784

# [mean - 3 * sd, mean + 3 * sd]
# # 339.1607
c(mean(clt_montecarlo$Est_Votes_Biden) - 3 * sd(clt_montecarlo$Est_Votes_Biden))
# 192.7266
c(mean(clt_montecarlo$Est_Votes_Trump) - 3 * sd(clt_montecarlo$Est_Votes_Trump))
# 0.9797676
c(mean(clt_montecarlo$Est_Prob_Biden) - 3 * sd(clt_montecarlo$Est_Prob_Biden))
# -0.0005790792
c(mean(clt_montecarlo$Est_Prob_Trump) - 3 * sd(clt_montecarlo$Est_Prob_Trump))


The following is the R code for simulating the reelection.


real_dat <- read.csv("/Users/mac/Desktop/PresidentialElection/Popular vote backend - Sheet1.csv")

N <- 10000
total_trial <- 10

# Clean the data set
real_election <- real_dat %>%
  select(state, dem_percent, rep_percent, EV, dem_this_margin)

real_election$dem_percent <- as.numeric(
  sub("%", "", real_election$dem_percent, fixed=TRUE)) / 100
real_election$rep_percent <- as.numeric(
  sub("%", "", real_election$rep_percent, fixed=TRUE)) / 100
real_election$dem_this_margin <- as.numeric(
  sub("%", "", real_election$dem_this_margin, fixed=TRUE)) / 100

real_election <- real_election %>%
  arrange(sort(abs(dem_this_margin)))

kable(head(real_election, 15))

# Set seed to ensure the results can be reproducible
set.seed(270)

# Number of iterations per trial
N <- 10000

# Total number of trials
total_trial <- 10

# Repeat the process, with MR = 0.02, 0.04,..., 0.12
re_df_mr_002 <- MC_simulate(real_election, 0.02)
re_df_mr_004 <- MC_simulate(real_election, 0.04)
re_df_mr_006 <- MC_simulate(real_election, 0.06)
re_df_mr_008 <- MC_simulate(real_election, 0.08)
re_df_mr_010 <- MC_simulate(real_election, 0.10)
re_df_mr_012 <- MC_simulate(real_election, 0.12)

# Save and Retrieve Results
saveRDS(re_df_mr_002, "/Users/mac/Desktop/PresidentialElection/reelection_MR_0.02.csv")
saveRDS(re_df_mr_004"/Users/mac/Desktop/PresidentialElection/reelection_MR_0.04.csv")
saveRDS(re_df_mr_006"/Users/mac/Desktop/PresidentialElection/reelection_MR_0.06.csv")
saveRDS(re_df_mr_008"/Users/mac/Desktop/PresidentialElection/reelection_MR_0.08.csv")
saveRDS(re_df_mr_010"/Users/mac/Desktop/PresidentialElection/reelection_MR_0.10.csv")
saveRDS(re_df_mr_012"/Users/mac/Desktop/PresidentialElection/reelection_MR_0.12.csv")

reelect_results_002 <- readRDS("/Users/mac/Desktop/PresidentialElection/reelection_MR_0.02.csv")
reelect_results_004 <- readRDS("/Users/mac/Desktop/PresidentialElection/reelection_MR_0.04.csv")
reelect_results_006 <- readRDS("/Users/mac/Desktop/PresidentialElection/reelection_MR_0.06.csv")
reelect_results_008 <- readRDS("/Users/mac/Desktop/PresidentialElection/reelection_MR_0.08.csv")
reelect_results_010 <- readRDS("/Users/mac/Desktop/PresidentialElection/reelection_MR_0.10.csv")
reelect_results_012 <- readRDS("/Users/mac/Desktop/PresidentialElection/reelection_MR_0.12.csv")


# Matrix to store the averaged votes received by Biden with each margin of error
votes_biden_re <- cbind(sort(reelect_results_002$Est_Votes_Biden),
                       sort(reelect_results_004$Est_Votes_Biden),
                       sort(reelect_results_006$Est_Votes_Biden),
                       sort(reelect_results_008$Est_Votes_Biden),
                       sort(reelect_results_010$Est_Votes_Biden),
                       sort(reelect_results_012$Est_Votes_Biden))
colnames(votes_biden_re) <- c("VB_0.02", "VB_0.04",
                              "VB_0.06", "VB_0.08",
                              "VB_0.10", "VB_0.12")
votes_biden_re <- as.data.frame(votes_biden_re)
saveRDS(votes_biden_re, "/Users/mac/Desktop/PresidentialElection/votes_biden_re.csv")

# Matrix to store the averaged votes received by Trump with each margin of error
votes_trump_re <- cbind(sort(reelect_results_002$Est_Votes_Trump),
                        sort(reelect_results_004$Est_Votes_Trump),
                        sort(reelect_results_006$Est_Votes_Trump),
                        sort(reelect_results_008$Est_Votes_Trump),
                        sort(reelect_results_010$Est_Votes_Trump),
                        sort(reelect_results_012$Est_Votes_Trump))
colnames(votes_biden_re) <- c("VT_0.02", "VT_0.04",
                              "VT_0.06", "VT_0.08",
                              "VT_0.10", "VT_0.12")
votes_trump_re <- as.data.frame(votes_trump_re)
saveRDS(votes_trump_re, "/Users/mac/Desktop/PresidentialElection/votes_trump_re.csv")



# Matrix to record the averaged probability of winning for Biden
prob_biden_re <- cbind(sort(reelect_results_002$Est_Prob_Biden),
                       sort(reelect_results_004$Est_Prob_Biden),
                       sort(reelect_results_006$Est_Prob_Biden),
                       sort(reelect_results_008$Est_Prob_Biden),
                       sort(reelect_results_010$Est_Prob_Biden),
                       sort(reelect_results_012$Est_Prob_Biden))
colnames(prob_biden_re) <- c("PB_0.02", "PB_0.04",
                             "PB_0.06", "PB_0.08",
                             "PB_0.10", "PB_0.12")
prob_biden_re <- as.data.frame(prob_biden_re)
saveRDS(prob_biden_re, "/Users/mac/Desktop/PresidentialElection/prob_biden_re.csv")

# Matrix to store the averaged votes received by Trump with each margin of error
prob_trump_re <- cbind(sort(reelect_results_002$Est_Prob_Trump),
                        sort(reelect_results_004$Est_Prob_Trump),
                        sort(reelect_results_006$Est_Prob_Trump),
                        sort(reelect_results_008$Est_Prob_Trump),
                        sort(reelect_results_010$Est_Prob_Trump),
                        sort(reelect_results_012$Est_Prob_Trump))
colnames(prob_trump_re) <- c("PT_0.02", "PT_0.04",
                             "PT_0.06", "PT_0.08",
                             "PT_0.10", "PT_0.12")
prob_trump_re <- as.data.frame(prob_trump_re)
saveRDS(prob_trump_re, "/Users/mac/Desktop/PresidentialElection/prob_trump_re.csv")

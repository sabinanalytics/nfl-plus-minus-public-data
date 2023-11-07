data {
  int<lower=1> n_plays; //number of data points
  int<lower=0,upper=1> y[n_plays]; //response variable, y is a set of integers
  real pm_replacement;//replacement level plus-minus for all players
  
  vector [n_plays] home;// variable denoting home/away/neutral (-1 for away, 0 for neutral, 1 for home)
  
  int<lower=1> n_players; //number of Players
  int<lower=1, upper=n_players> player_index[n_players]; //player id

  int<lower=1> n_position; //number of unique position groups
  int<lower=1, upper=n_position> positions[n_players]; //the main position for each player
  
  int<lower=1> n_player_plays; //total number of plays by all players (should be about n_players*n_plays)
  

  vector[n_players] player_prior_mean        ; //prior mean for each player
  // vector[n_players] player_prior_sd_multiple        ; // multiple of player variance based on position

  // vector<lower = 0, upper=1>[n_position] posWeights; // weights for each position to help with the SOFT sum constraint to 1 of p
  // real sdSumWeights; //standard deviation of sum of weights 

  // vector<lower = 0> [n_players] playerPlayWeights; // weighting the player effects to soft constraint sum 0

  vector [n_player_plays] wX; // values of sparse design matrix
  int <lower = 1> vX[n_player_plays]; // column indicators of sparse matrix
  int <lower = 1> uX[n_plays+1]; // row indicators of sparse matrix
  
  // matrix<lower =0, upper = 1>[n_players, n_position] posMat;//Matrix of fraction of time each player spends at each position
  
  }
  
  transformed data {
  }
  
  parameters {
  vector[n_players] b; //player intercepts
  
  real u; // effect for home field on a per play basis 
  real avg_effect; // intercept for average success rate
  
  vector<lower=0>[n_position] phi; // multiplicative adjustment to variance of player mean by position (different one for each main position - serves as different shrinkage parameters)
  // real<lower=0> sd_model; //model variance
  real<lower=0> sd_player; // player to player variance (before position adjustment)
  }
  
  transformed parameters {
  vector[n_plays] eta;  // linear predictor
  vector[n_plays] eta_phi;//probit translation of eta
  real avg_player_effect;
  // real sumWeightedPhi;
  // real sumWeightedPlayerEffect;
  // compute linear predictor
  eta = csr_matrix_times_vector(n_plays, n_players, wX, vX, uX, b) + home*u + avg_effect;
  eta_phi = Phi_approx(eta);
  //transformed phi
  // sumWeightedPhi = sum(posWeights .*phi );
  // sumWeightedPlayerEffect = sum(playerPlayWeights .*b );
  avg_player_effect = mean(b);
  }
  
  
  model{
  // vector[n_players] positionVarCoef; // variance multiplier for each player
  // 
  // //Priors
  // 
  // //priors and hyperpriors on players
  // positionVarCoef = posMat*phi;//
  // 
  for(j in 1:n_players){
    b[j] ~ student_t(15, player_prior_mean[j], sd_player*phi[positions[j]]) ;
  }
  sd_player ~ normal(0.213, 0.1); // z-scores have std dev of 1 with 22 players , 1 / sqrt(22) = 0.213
  avg_effect ~ normal(-0.15, 0.1);
  
  avg_player_effect ~ normal(0, 0.0001);

   // b ~ normal(player_prior_mean, sd_player.*phi[positions]) ;  

  // //b ~ normal(bpmMean, sqrt(positionVarCoef));
  // 
  // sumWeightedPhi ~ normal(1, sdSumWeights*n_position);
  // //sumWeightedPlayerEffect ~ normal(0, 0.001);
  // 
  phi[1] ~  gamma(41, 10)    ;//QB 0.1,25
  phi[2] ~  gamma(10, 10)    ;//RB (Runnning back)
  phi[3] ~  gamma( 6, 10)    ;//FB (fullback)
  phi[4] ~  gamma(30, 10)    ;//SLOT_WR (slot receivers)
  phi[5] ~  gamma(30, 10)    ;//WR (non-slot wide receivers)
  phi[6] ~  gamma(25, 10)    ;//TE not quite converged
  phi[7] ~  gamma(15, 10)    ;//T (tackle)
  phi[8] ~  gamma(14, 10)    ;//G (guard)
  phi[9] ~  gamma(14, 10)    ;//C (center)
  
  phi[10] ~ gamma( 5 , 10)    ;//EDGE (edge rusher)
  phi[11] ~ gamma( 6 , 10)    ;//INTERIOR_LINE (defensive tackle & nose)
  phi[12] ~ gamma( 5 , 10)    ;//OLB (outside linebacker)
  phi[13] ~ gamma( 5 , 10)    ;//MLB (inside linebacker)
  phi[14] ~ gamma(12 , 10)    ;//SLOT_CB (slot cornerback)
  phi[15] ~ gamma(18 , 10)    ;//CB (cornerback)
  phi[16] ~ gamma(17 , 10)    ;//SAFETY (safety)
  
  phi[17] ~ gamma(10 , 10)    ;//NA (No Position Data for this player)

  
  // DO I WANT ANOTHER PRIOR ON ALL PHI OR B? CAN PUT MULTIPLE
  
  // prior for Home Field Advantage (per play) 
  u ~ normal(0.2, 1); 
  // b ~ normal(pm_replacement, 2);
  // sd_model ~ gamma(40,30);
  // // likelihood contribution
  // y ~ normal(eta, sd_model);

  // PROBIT REGRESSION
  // Likelihood of observed data
  y ~ bernoulli(eta_phi);  // Phi is the cumulative distribution function for a standard normal


}


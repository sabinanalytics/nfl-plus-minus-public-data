data {
  int<lower=1> n_plays; //number of data points
  int<lower=0,upper=1> y_success[n_plays]; //response variable, y_success is a set of integers (1 if successful play, 0 otherwise)
  real z_epa[n_plays]; //response variable, the z-score of the epa per play
  
  real pm_replacement;//replacement level plus-minus for all players
  
  vector[n_plays] home;// variable denoting home/away/neutral (-1 for away, 0 for neutral, 1 for home)
  
  int<lower=1> n_players; //number of Players
  int<lower=1, upper=n_players> player_index[n_players]; //player id

  int<lower=1> n_position; //number of unique position groups
  int<lower=1, upper=n_position> positions[n_players]; //the main position for each player
  
  int<lower=1> n_player_plays; //total number of plays by all players (should be about n_players*n_plays)
  
  vector[n_players] player_plays;//number of plays total for each player
  vector[n_position] position_play_pct;// percent of all plays played at each position

  vector[n_players] player_prior_mean        ; //prior mean for each player
  // vector[n_players] player_prior_sd_multiple        ; // multiple of player variance based on position
  //draft_number^(-0.2) * rookie_deal + (1-rookie_deal)*apy_cap_pct_vs_avg  (the last variable is only within position)
  vector[n_players] player_rookie_deal;
  vector[n_players] player_draft_number;
  vector[n_players] player_undrafted;
  vector[n_players] player_apy_cap_pct_vs_avg;


  vector<lower = 0>[n_position] pos_shrinkage_prior_mean; // prior mean of the shrinkage of each position
  vector<lower = 0>[n_position] pos_shrinkage_prior_sd; // prior std. deviation of the shrinkage of each position
  
  real sd_sum_weighted_phi; //standard deviation of sum of weights

  // vector<lower = 0> [n_players] playerPlayWeights; // weighting the player effects to soft constraint sum 0

  vector [n_player_plays] wX; // values of sparse design matrix
  int <lower = 1> vX[n_player_plays]; // column indicators of sparse matrix
  int <lower = 1> uX[n_plays+1]; // row indicators of sparse matrix
  
  // matrix<lower =0, upper = 1>[n_players, n_position] posMat;//Matrix of fraction of time each player spends at each position
  
  }
  
  transformed data {
    vector[n_players] player_play_weight;//number of plays for the player divided by total number of player plays
    vector[n_position] position_shrinkage_a;// first parameter in position shrinkage gamma distribution
    vector[n_position] position_shrinkage_b;// first parameter in position shrinkage gamma distribution
    
    player_play_weight = player_plays / sum(player_plays);
    
    for(p in 1:n_position){
      position_shrinkage_a[p] = pow(pos_shrinkage_prior_mean[p] / pos_shrinkage_prior_sd[p], 2);
      position_shrinkage_b[p] = pos_shrinkage_prior_mean[p] / pow(pos_shrinkage_prior_sd[p], 2);      
    }

  }
  
  parameters {
  vector[n_players] b; //player intercepts
  vector[4] beta_player;//hyper prior on regression for player prior mean
  
  real u; // effect for home field on a per play basis 
  real avg_effect; // intercept for average success rate
  
  vector<lower=0>[n_position] phi; // multiplicative adjustment to variance of player mean by position (different one for each main position - serves as different shrinkage parameters)
  real<lower=0> sd_model; //model variance (for epa per play z-score)
  real<lower=0> sd_player; // player to player variance (before position adjustment)
  }
  
  transformed parameters {
  vector[n_plays] eta;  // linear predictor
  vector[n_plays] eta_phi;//probit translation of eta
  real sum_weighted_player_effect;
  real sum_weighted_phi;

  // compute linear predictor
  eta = csr_matrix_times_vector(n_plays, n_players, wX, vX, uX, b) + home*u;
  eta_phi = Phi(eta + avg_effect);//Phi_approx (supposedly faster)
  //transformed phi
  sum_weighted_phi = sum(position_play_pct .*phi );
  
  sum_weighted_player_effect = sum( player_play_weight .* b);

  }
  
  
  model{
  // vector[n_players] positionVarCoef; // variance multiplier for each player
  // 
  // //Priors
  // 
  // //priors and hyperpriors on players
  // positionVarCoef = posMat*phi;//
  // 
  vector[n_players] player_prior_mean_model;
  for(j in 1:n_players){
    //generic prior where every player starts at replacement
    // b[j] ~ normal(player_prior_mean[j], sd_player*phi[positions[j]]) ;
    
    //prior based on contract value if not on rookie deal or draft position if on rookie deal
    //draft_number^(-1/3) * rookie_deal + (1-rookie_deal)*apy_cap_pct_vs_avg  (the last variable is only within position)
    player_prior_mean_model[j] = player_rookie_deal[j]*(beta_player[1] + beta_player[2]*(1 - player_undrafted[j])*(1/cbrt(player_draft_number[j])) + beta_player[3]*player_undrafted[j]) + 
                                  beta_player[4]*(1 - player_rookie_deal[j])*player_apy_cap_pct_vs_avg[j];
    b[j] ~ normal(player_prior_mean_model[j], sd_player*phi[positions[j]]) ;

    
  }
  sd_player ~ normal(0.213, 0.1); // z-scores have std dev of 1 with 22 players , 1 / sqrt(22) = 0.213
  avg_effect ~ normal(-0.12, 0.1);
  
  //soft priors to get the average player effects to 0
  sum_weighted_player_effect ~ double_exponential(0, 0.0001);
  b ~ double_exponential(0, 1);//overall talent centered at 0 with thick tails

   // b ~ normal(player_prior_mean, sd_player.*phi[positions]) ;  

  // //b ~ normal(bpmMean, sqrt(positionVarCoef));
  // 
  sum_weighted_phi ~ double_exponential(1, sd_sum_weighted_phi*n_position);
   
  for(p in 1:n_position){
    phi[p] ~ gamma(position_shrinkage_a[p], position_shrinkage_b[p]);
  } 
   
  // phi[1] ~  gamma(1.9, 1.9)    ;//QB 0.1,25
  // phi[2] ~  gamma(1.9, 1.9)    ;//RB (Runnning back)
  // phi[3] ~  gamma(1.9, 1.9)    ;//FB (fullback)
  // phi[4] ~  gamma(1.9, 1.9)    ;//SLOT_WR (slot receivers)
  // phi[5] ~  gamma(1.9, 1.9)    ;//WR (non-slot wide receivers)
  // phi[6] ~  gamma(1.9, 1.9)    ;//TE not quite converged
  // phi[7] ~  gamma(1.9, 1.9)    ;//T (tackle)
  // phi[8] ~  gamma(1.9, 1.9)    ;//G (guard)
  // phi[9] ~  gamma(1.9, 1.9)    ;//C (center)
  // 
  // phi[10] ~ gamma(1.9, 1.9)    ;//EDGE (edge rusher)
  // phi[11] ~ gamma(1.9, 1.9)    ;//INTERIOR_LINE (defensive tackle & nose)
  // phi[12] ~ gamma(1.9, 1.9)    ;//OLB (outside linebacker)
  // phi[13] ~ gamma(1.9, 1.9)    ;//MLB (inside linebacker)
  // phi[14] ~ gamma(1.9, 1.9)    ;//SLOT_CB (slot cornerback)
  // phi[15] ~ gamma(1.9, 1.9)    ;//CB (cornerback)
  // phi[16] ~ gamma(1.9, 1.9)    ;//SAFETY (safety)
  // 
  // phi[17] ~ gamma(0.5 ,1.0)    ;//NA (No Position Data for this player)

  
  // DO I WANT ANOTHER PRIOR ON ALL PHI OR B? CAN PUT MULTIPLE
  
  // prior for Home Field Advantage (per play) 
  u ~ normal(0.2, 1); 
  // b ~ normal(pm_replacement, 2);
  sd_model ~ gamma(6,100);
  // // likelihood contribution
  // y ~ normal(eta, sd_model);

  // PROBIT REGRESSION
  // Likelihood of observed data
  y_success ~ bernoulli(eta_phi);  // Phi is the cumulative distribution function for a standard normal
  z_epa ~ normal(eta, sd_model); // do we want thicker tails like student_t(8)?

}


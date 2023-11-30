data {
  int<lower=1> n_plays; //number of data points
  int<lower=0, upper=1> y_success[n_plays]; //response variable, y_success is a set of integers (1 if successful play, 0 otherwise)
  
  real z_epa[n_plays]; //response variable, the z-score of the epa per play
  
  int<lower=1> n_seasons;//number of seasons in the data
  
  // variables surrounding game situation
  vector[n_plays] home;// variable denoting home/away/neutral (-1 for away, 0 for neutral, 1 for home)
  vector[n_plays] grass_ind;
  vector[n_plays] outdoors_ind;
  vector[n_plays] non_missing_temp_ind;
  vector[n_plays] non_missing_wind_ind;
  vector[n_plays] temp;
  vector[n_plays] wind;
  
  
  
  int<lower=1> n_players; //number of Players
  int<lower=1> n_player_seasons; //n_players TIMES n_seasons

  int<lower=1> n_position; //number of unique position groups
  int<lower=1, upper=n_position> positions[n_players]; //the main position for each player
  
  int<lower=1> n_player_plays; //total number of plays by all players (should be about n_players*n_plays)
  
  vector[n_players] player_plays;//number of plays total for each player
  vector[n_position] position_play_pct;// percent of all plays played at each position

  // variables to denote how many seasons and which ones a player was a part of
  vector<lower =0>[n_players] player_start_season_index;
  vector<lower =0>[n_players] player_end_season_index;
  vector<lower =0>[n_players] player_tot_seasons;

  vector<lower = 0, upper = 1>[n_players] player_rookie_deal;
  vector<lower = 0, upper = 1000>[n_players] player_draft_number;
  vector<lower = 0, upper = 1>[n_players] player_undrafted;
  vector<lower = 0, upper = 1>[n_players] player_already_in_league;

  // vector[n_players] player_apy_cap_;pct_vs_avg;
  //variables for a basis to estimate non-linear draft number affect
  int<lower = 0> player_draft_basis_col;
  matrix[n_players, player_draft_basis_col] player_draft_basis;


  vector<lower = 0>[n_position] pos_shrinkage_prior_mean; // prior mean of the shrinkage of each position
  vector<lower = 0>[n_position] pos_shrinkage_prior_sd; // prior std. deviation of the shrinkage of each position
  
  real sd_sum_weighted_phi; //standard deviation of sum of weights

  vector [n_player_plays] wX; // values of sparse design matrix
  int <lower = 1> vX[n_player_plays]; // column indicators of sparse matrix
  int <lower = 1> uX[n_plays+1]; // row indicators of sparse matrix

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
  matrix[n_players, n_seasons] b; //player intercepts
  vector[2] beta_player;//hyper prior on regression for player prior mean
  vector[player_draft_basis_col] beta_player_draft; // parameters for nonlinear splines on draft position
  
  vector[5] game_params_epa; // game effects on epa per play (no intercept here since we centered at 0)
  vector[6] game_params_success; // game effects on success rate

  vector<lower=0>[n_position] phi; // multiplicative adjustment to variance of player mean by position (different one for each main position - serves as different shrinkage parameters)
  real<lower=0> sd_model; //model variance (for epa per play z-score)
  real<lower=0> sd_player_yoy; // year to year player variance (before position adjustment)
  real<lower=0> sd_player_rookie; // rookie year player variance (before position adjustment)
  real<lower=0> sd_player_first; // non-rookie, first-year of model player variance (before position adjustment)

  }
  
  transformed parameters {
  vector[n_plays] eta;  // linear predictor

  // vector[n_plays] eta_phi;//probit translation of eta
  real sum_weighted_player_effect;
  real<lower=0> sum_weighted_phi;
  vector[(n_seasons*n_players)] b_long;//long format of player parameters (instead of matrix)
  vector[n_players] player_prior_mean_model;
  vector[n_players] player_prior_sd_model;
  
  //fill in b_long with each values of b
  for(j in 1:n_players){
    b_long[(n_seasons*(j - 1) + 1):(n_seasons*(j - 1) + n_seasons)] = to_vector(b[j]);
  }
  
  // compute linear predictor
  eta = csr_matrix_times_vector(n_plays, n_player_seasons, wX, vX, uX, b_long);
  

  // eta_phi = Phi_approx(eta + avg_effect);//Phi_approx (supposedly faster but tails don't match probit)
  //transformed phi
  sum_weighted_phi = sum(position_play_pct .*phi );
  
  sum_weighted_player_effect = sum( to_row_vector(player_play_weight) * b);
  
  for(j in 1:n_players){
      // player_prior_mean_model[j] = beta_player[1] +
      //   beta_player[2]*player_undrafted[j] + 
      //   beta_player[3]*(1 - player_undrafted[j])*(-log(player_draft_number[j]));
      if(player_already_in_league[j] == 1){
        player_prior_mean_model[j] = -0.05 ;
        player_prior_sd_model[j] = sd_player_first;

      }else{
        player_prior_mean_model[j] = beta_player[1] +
          beta_player[2]*player_undrafted[j] + 
          (1 - player_undrafted[j])*(player_draft_basis[j]*beta_player_draft);
          
        player_prior_sd_model[j] = sd_player_rookie;

      }
      
      
  }

  

  }
  
  
  model{
  // means for each likelihood defined
  vector[n_plays] mu_epa;  // mean vector for epa likelihood
  vector[n_plays] mu_success;  // mean vector for success rate likelihood
  
  mu_epa = eta + 
    game_params_epa[1]*home + 
    game_params_epa[2]*grass_ind + 
    game_params_epa[3]*outdoors_ind + 
    game_params_epa[4]*non_missing_temp_ind .* temp + 
    game_params_epa[5]*non_missing_wind_ind .* wind;
  mu_success = eta +
    game_params_success[1] + 
    game_params_success[2]*home + 
    game_params_success[3]*grass_ind + 
    game_params_success[4]*outdoors_ind + 
    game_params_success[5]*non_missing_temp_ind .* temp + 
    game_params_success[6]*non_missing_wind_ind .* wind;
    
    
    
    
  // //Priors
  //one prior for year to year change
  sd_player_yoy ~ gamma(0.0025, 0.5); // z-scores have std dev of 1 with 22 players , 1 / sqrt(22) = 0.213
  // one for rookie seasons
  sd_player_rookie ~ normal(0.1, 0.1); // z-scores have std dev of 1 with 22 players , 1 / sqrt(22) = 0.213
  // one for first year we observed (non-rookie)
  sd_player_first ~ normal(0.213, 0.1); // z-scores have std dev of 1 with 22 players , 1 / sqrt(22) = 0.213
  
  // avg_effect ~ normal(-0.12, 0.1);
  
  //soft priors to get the average player effects to 0
  sum_weighted_player_effect ~ double_exponential(0, 0.001);
  // mean comes from assuming average is -1/2 pts per game for this playtype (divided by 35 plays)
  // sd is from assuming best player is +8 pts passing per game (divided by 3 for QB value divded by 35 for plays)
  // sd = (8 / 3) / 35 (plays) / 3 (std. deviations)
  //skewness of 0.3 to have a decently skewed distribution
  beta_player ~ exp_mod_normal(-0.014, 0.025, 0.3);
  beta_player_draft ~ normal(0, 1);
  b_long ~ normal(0, 1);//overall talent centered at 0 
  
  // player effects are by season
  for(j in 1:n_players){

    for(s in 1:n_seasons){
      // if first season for player, you get a prior based on draft status
      if(player_start_season_index[j] == s){
        b[j, s] ~ normal(player_prior_mean_model[j], player_prior_sd_model[j]*phi[positions[j]]) ;
      }
      // if 2nd season or later, last season's value is your prior
      else if(player_start_season_index[j] < s){
        b[j, s] ~ normal(b[j, (s-1)], sd_player_yoy*phi[positions[j]]) ;
      }
      // otherwise you didn't start playing yet so I'll give you exactly 0 (doesn't count)
      else{
        b[j,s] ~ normal(0, 0.01);
      }
    }
  }
  

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
  // phi[12] ~ gamma(1.9, 1.9)    ;//MLB (inside linebacker)
  // phi[13] ~ gamma(1.9, 1.9)    ;//SLOT_CB (slot cornerback)
  // phi[14] ~ gamma(1.9, 1.9)    ;//CB (cornerback)
  // phi[15] ~ gamma(1.9, 1.9)    ;//SAFETY (safety)

  
  // DO I WANT ANOTHER PRIOR ON ALL PHI OR B? CAN PUT MULTIPLE
  
  // prior for game effect (per play) 
  game_params_success ~ normal(0, 1); 
  game_params_epa ~ normal(0, 1); 

  sd_model ~ gamma(6,100);
  // // likelihood contribution

  // PROBIT REGRESSION
  //   y_success ~ bernoulli(eta_phi);  // Phi is the cumulative distribution function for a standard normal
  // LOGISTIC REGRESSION
    y_success ~ bernoulli_logit(mu_success);

  // Likelihood of observed data
  z_epa ~ normal(mu_epa, sd_model); // do we want thicker tails like student_t(8)?

}

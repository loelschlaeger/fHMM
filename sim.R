### simulate data from HHMM
source("trans.R")
source("init.R")

simulateHHMM = function(controls){
  set.seed(controls[["seed"]])
  M = controls[["states"]][1] #coarse-scale states
  N = controls[["states"]][2] #fine-scale states
  df_cs = controls[["fix_df"]][1]
  df_fs = controls[["fix_df"]][2]
  T = controls[["timeHorizon"]][1]
  T_star = controls[["timeHorizon"]][2]

	thetaUncon = init_est(controls)
	thetaCon = thetaUncon2thetaCon(thetaUncon,controls)
	thetaList = thetaCon2thetaList(thetaCon,controls)

	simulateStates = function(delta,Gamma,T){
		N = length(delta)
		states = numeric(T)
		states[1] = sample(1:N,1,prob=delta)
		for(t in 2:T){
			states[t] = sample(1:N,1,prob=Gamma[states[t-1],])
		}
		return(states)
	}
	
	simulateObservations = function(states,mus,sigmas,dfs,T){
		obs = numeric(T)
		for(t in 1:T){
			obs[t] = rt(1,dfs[states[t]])*sigmas[states[t]]+mus[states[t]]
		}
		return(obs)
	}
	
	if(N==0){ ### simulate HMM
	  states = simulateStates(Gamma2delta(thetaList[["Gamma"]]),thetaList[["Gamma"]],T) 
	  observations = simulateObservations(states,thetaList[["mus"]],thetaList[["sigmas"]],thetaList[["dfs"]],T)
	}
	
	if(N>0){ ### simulate HHMM
  	states = matrix(0,T,T_star+1) 
  	observations = matrix(0,T,T_star+1)
  	states[,1] = simulateStates(Gamma2delta(thetaList[["Gamma"]]),thetaList[["Gamma"]],T) 
  	observations[,1] = simulateObservations(states,thetaList[["mus"]],thetaList[["sigmas"]],thetaList[["dfs"]],T)
  	for(t in 1:T){
  		S_t = states[t,1]
  		states[t,-1] = simulateStates(Gamma2delta(thetaList[["Gammas_star"]][[S_t]]),thetaList[["Gammas_star"]][[S_t]],T_star)
  		observations[t,-1] = simulateObservations(states[t,-1],thetaList[["mus_star"]][[S_t]],thetaList[["sigmas_star"]][[S_t]],thetaList[["dfs_star"]][[S_t]],T_star)
  	}
	}
	
	sim = list(
	  "observations" = observations,
	  "states"       = states,
	  "thetaUncon"   = thetaUncon,
	  "thetaCon"     = thetaCon,
	  "thetaList"    = thetaList
	  )
	return(sim)
}



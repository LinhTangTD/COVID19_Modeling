model = function(H.rate.nv, H.rate.v, p.fp.all, p.test.sym.nv, p.test.asym.nv, p.test.sym.v, p.test.asym.v,
                 I.rate.sym, I.rate.asym, p.sym.nv, p.sym.v, p.tp.sym.all, p.tp.asym.all){
  
  # Population is split into 2 sub-populations: Vulnerable (V) and Non-Vulnerable (NV)
  N = 100000
  p.vulnerable = .2
  N.v = N * p.vulnerable
  N.nv = N - N.v
  # Shared parameters
  day.q = 14 #quarantine period (days)
  day.i = 7 #incubation period (days)
  E.rate = 1 / day.i #exposure rate
  
  # Parameters Declaration for NV sub-population
  p.test.all.nv = p.test.asym.nv/2 #percentage of testing for all NV sub-population
  p.ns.nv = .25 #percentage of initial non-susceptible
  R.rate.sym.nv = .09 #recovery rate for symptomatic group
  R.rate.asym.nv = .1 #recovery rate for asymptomatic group
  R.rate.h.nv = .05333 #recovery rate for hospitalized group
  D.rate.sym.nv = .01 #death rate for symptomatic group
  D.rate.h.nv = 0.01333 #death rate for hospitalized group
  
  # Parameters Declaration for V sub-population
  p.test.all.v = p.test.asym.v/2 #percentage of testing for all V sub-population
  p.ns.v = 0 #percentage of initial non-susceptible
  R.rate.sym.v = .09 #recovery rate for symptomatic group
  R.rate.asym.v = .04 #recovery rate for asymptomatic group
  R.rate.h.v = .08 #recovery rate for hospitalized group
  D.rate.sym.v = .04 #death rate for symptomatic group
  D.rate.asym.v = .02 #death rate for asymptomatic group
  D.rate.h.v = .1 #death rate for hospitalized group
  
  ### Initial setting (day 0)
  Isym.nv = 5 #initial infected symptomatic
  Iasym.nv = 0 #initial infected asymptomatic
  R.nv = N.nv * p.ns.nv #initial "recovered" i.e non-susceptible
  S.nv = N.nv - Isym.nv - Iasym.nv - R.nv
  FQ.nv = 0
  E.nv = 0
  D.nv = 0
  H.nv = 0
  Qsym.nv = 0
  Qasym.nv = 0
  Isym.v = 5 #initial infected symptomatic
  Iasym.v = 0 #initial infected asymptomatic
  R.v = N.v * p.ns.v #initial "recovered" i.e non-susceptible
  S.v = N.v - Isym.v - Iasym.v - R.v
  FQ.v = 0
  E.v = 0
  D.v = 0
  H.v = 0
  Qsym.v = 0
  Qasym.v = 0
  Newly.Infected.All = 0
  Peak.Infected = Newly.Infected.All
  Total.Infected.So.Far = Isym.nv + Isym.v
  Current.Hospitalized.All = H.nv + H.v
  Peak.Hospitalized = Current.Hospitalized.All
  Total.Dead.So.Far  = D.nv + D.v
  Peak.Day.Infected = 0
  Possible.End.Days = 0
  day = 0
  checkpoints = c(100,200,300,400,500,600,700,800,900,1000,1100,1200,1300,1400)
  for(i in 1:1500){
    day = day + 1
    ### Immediate calculations (for next day statistics)
    new.exposed.nv = min(
      (rbinom(1, S.nv, I.rate.sym * Isym.v / N) +
         rbinom(1, S.nv, I.rate.asym * Iasym.v / N) +
         rbinom(1, S.nv, I.rate.sym * Isym.nv / N) + 
         rbinom(1, S.nv, I.rate.asym * Iasym.nv / N)), S.nv)
    quarantine.released.today.nv = rbinom(1, FQ.nv, 1/day.q)
    new.quarantined.nv = rbinom(1, S.nv, p.test.all.nv * p.fp.all)
    new.infected.all.nv = rbinom(1, E.nv, E.rate) #total infected
    # infected symptomatic
    new.infected.sym.nv = rbinom(1, new.infected.all.nv, p.sym.nv)
    confirmed.sym.nv = rbinom(1, Isym.nv, p.test.sym.nv * p.tp.sym.all) #tested & true positive
    nonconfirmed.sym.nv = Isym.nv - confirmed.sym.nv #not-tested & false negative
    # infected asymptomatic
    new.infected.asym.nv = new.infected.all.nv - new.infected.sym.nv
    confirmed.asym.nv = rbinom(1, Iasym.nv, p.test.asym.nv * p.tp.asym.all)
    nonconfirmed.asym.nv = Iasym.nv - confirmed.asym.nv
    # transition between compartments (I, Q, H, R, D)
    nonconfirmed.sym.to.H.nv = rbinom(1, nonconfirmed.sym.nv, H.rate.nv)
    nonconfirmed.sym.to.R.nv = rbinom(1, nonconfirmed.sym.nv, R.rate.sym.nv)
    nonconfirmed.sym.to.D.nv = rbinom(1, nonconfirmed.sym.nv, D.rate.sym.nv)
    while((nonconfirmed.sym.to.H.nv + nonconfirmed.sym.to.R.nv + nonconfirmed.sym.to.D.nv) > nonconfirmed.sym.nv){
      nonconfirmed.sym.to.H.nv = rbinom(1, nonconfirmed.sym.nv, H.rate.nv)
      nonconfirmed.sym.to.R.nv = rbinom(1, nonconfirmed.sym.nv, R.rate.sym.nv)
      nonconfirmed.sym.to.D.nv = rbinom(1, nonconfirmed.sym.nv, D.rate.sym.nv)
    }
    
    nonconfirmed.asym.to.R.nv = rbinom(1, nonconfirmed.asym.nv, R.rate.asym.nv)
    Qsym.to.H.nv = rbinom(1, Qsym.nv, H.rate.nv)
    Qsym.to.R.nv = rbinom(1, Qsym.nv, R.rate.sym.nv)
    Qsym.to.D.nv = rbinom(1, Qsym.nv, D.rate.sym.nv)
    while((Qsym.to.H.nv + Qsym.to.R.nv + Qsym.to.D.nv) > Qsym.nv){
      Qsym.to.H.nv = rbinom(1, Qsym.nv, H.rate.nv)
      Qsym.to.R.nv = rbinom(1, Qsym.nv, R.rate.sym.nv)
      Qsym.to.D.nv = rbinom(1, Qsym.nv, D.rate.sym.nv)
    }
    Qasym.to.R.nv = rbinom(1, Qasym.nv, R.rate.asym.nv)
    H.to.R.nv = rbinom(1, H.nv, R.rate.h.nv)
    H.to.D.nv = rbinom(1, H.nv, D.rate.h.nv)
    while((H.to.R.nv + H.to.D.nv) > H.nv){ # These while loops check for errors
      H.to.R.nv = rbinom(1, H.nv, R.rate.h.nv)
      H.to.D.nv = rbinom(1, H.nv, D.rate.h.nv)
    }
    
    ### Calculate changes in each compartment
    dS.nv = - new.exposed.nv - new.quarantined.nv + quarantine.released.today.nv
    dFQ.nv = new.quarantined.nv - quarantine.released.today.nv
    dE.nv = new.exposed.nv - new.infected.all.nv
    dIsym.nv = new.infected.sym.nv - confirmed.sym.nv - nonconfirmed.sym.to.H.nv - nonconfirmed.sym.to.R.nv - nonconfirmed.sym.to.D.nv
    dIasym.nv = new.infected.asym.nv - confirmed.asym.nv - nonconfirmed.asym.to.R.nv
    dQsym.nv = confirmed.sym.nv - Qsym.to.H.nv - Qsym.to.R.nv - Qsym.to.D.nv
    dQasym.nv = confirmed.asym.nv - Qasym.to.R.nv
    dH.nv = nonconfirmed.sym.to.H.nv + Qsym.to.H.nv - H.to.D.nv  - H.to.R.nv
    dR.nv = H.to.R.nv + nonconfirmed.sym.to.R.nv + Qsym.to.R.nv + Qasym.to.R.nv +  nonconfirmed.asym.to.R.nv
    dD.nv = H.to.D.nv + nonconfirmed.sym.to.D.nv + Qsym.to.D.nv
    
    ### Immediate calculations (for next day statistics)
    new.exposed.v = min(
      (rbinom(1, S.v, I.rate.sym * Isym.v / N) +
         rbinom(1, S.v, I.rate.asym * Iasym.v / N) +
         rbinom(1, S.v, I.rate.sym * Isym.nv / N) + 
         rbinom(1, S.v, I.rate.asym * Iasym.nv / N)), S.v)
    quarantine.released.today.v = rbinom(1, FQ.v, 1/day.q)
    new.quarantined.v = rbinom(1, S.v, p.test.all.v * p.fp.all)
    new.infected.all.v = rbinom(1, E.v, E.rate) #total infected
    # infected symptomatic
    new.infected.sym.v = rbinom(1, new.infected.all.v, p.sym.v)
    confirmed.sym.v = rbinom(1, Isym.v, p.test.sym.v * p.tp.sym.all) #tested & true positive
    nonconfirmed.sym.v = Isym.v - confirmed.sym.v #not-tested & false negative
    # infected asymptomatic
    new.infected.asym.v = new.infected.all.v - new.infected.sym.v
    confirmed.asym.v = rbinom(1, Iasym.v, p.test.asym.v * p.tp.asym.all)
    nonconfirmed.asym.v = Iasym.v - confirmed.asym.v
    # transition between compartments (I, Q, H, R, D)
    nonconfirmed.sym.to.H.v = rbinom(1, nonconfirmed.sym.v, H.rate.v)
    nonconfirmed.sym.to.R.v = rbinom(1, nonconfirmed.sym.v, R.rate.sym.v)
    nonconfirmed.sym.to.D.v = rbinom(1, nonconfirmed.sym.v, D.rate.sym.v)
    while((nonconfirmed.sym.to.H.v + nonconfirmed.sym.to.R.v + nonconfirmed.sym.to.D.v) > nonconfirmed.sym.v){
      nonconfirmed.sym.to.H.v = rbinom(1, nonconfirmed.sym.v, H.rate.v)
      nonconfirmed.sym.to.R.v = rbinom(1, nonconfirmed.sym.v, R.rate.sym.v)
      nonconfirmed.sym.to.D.v = rbinom(1, nonconfirmed.sym.v, D.rate.sym.v) 
    }
    nonconfirmed.asym.to.R.v = rbinom(1, nonconfirmed.asym.v, R.rate.asym.v)
    nonconfirmed.asym.to.D.v = rbinom(1, nonconfirmed.asym.v, D.rate.asym.v)
    while((nonconfirmed.asym.to.R.v + nonconfirmed.asym.to.D.v) > nonconfirmed.asym.v){
      nonconfirmed.asym.to.R.v = rbinom(1, nonconfirmed.asym.v, R.rate.asym.v)
      nonconfirmed.asym.to.D.v = rbinom(1, nonconfirmed.asym.v, D.rate.asym.v)  
    }
    Qsym.to.H.v = rbinom(1, Qsym.v, H.rate.v)
    Qsym.to.R.v = rbinom(1, Qsym.v, R.rate.sym.v)
    Qsym.to.D.v = rbinom(1, Qsym.v, D.rate.sym.v)
    while(Qsym.to.H.v + Qsym.to.R.v + Qsym.to.D.v > Qsym.v){
      Qsym.to.H.v = rbinom(1, Qsym.v, H.rate.v)
      Qsym.to.R.v = rbinom(1, Qsym.v, R.rate.sym.v)
      Qsym.to.D.v = rbinom(1, Qsym.v, D.rate.sym.v)
    }
    Qasym.to.R.v = rbinom(1, Qasym.v, R.rate.asym.v)
    Qasym.to.D.v = rbinom(1, Qasym.v, D.rate.asym.v)
    while((Qasym.to.R.v + Qasym.to.D.v) > Qasym.v){
      Qasym.to.R.v = rbinom(1, Qasym.v, R.rate.asym.v)
      Qasym.to.D.v = rbinom(1, Qasym.v, D.rate.asym.v)
    }
    H.to.R.v = rbinom(1, H.v, R.rate.h.v)
    H.to.D.v = rbinom(1, H.v, D.rate.h.v)
    while((H.to.R.v + H.to.D.v) > H.v){
      H.to.R.v = rbinom(1, H.v, R.rate.h.v)
      H.to.D.v = rbinom(1, H.v, D.rate.h.v)
    }
    
    ### Calculate changes in each compartment
    dS.v = - new.exposed.v - new.quarantined.v + quarantine.released.today.v
    dFQ.v = new.quarantined.v - quarantine.released.today.v
    dE.v = new.exposed.v - new.infected.all.v
    dIsym.v = new.infected.sym.v - confirmed.sym.v - nonconfirmed.sym.to.H.v - nonconfirmed.sym.to.R.v - nonconfirmed.sym.to.D.v
    dIasym.v = new.infected.asym.v - confirmed.asym.v - nonconfirmed.asym.to.R.v - nonconfirmed.asym.to.D.v
    dQsym.v = confirmed.sym.v - Qsym.to.H.v - Qsym.to.R.v - Qsym.to.D.v
    dQasym.v = confirmed.asym.v - Qasym.to.R.v - Qasym.to.D.v
    dH.v = nonconfirmed.sym.to.H.v + Qsym.to.H.v - H.to.D.v - H.to.R.v
    dR.v = H.to.R.v + nonconfirmed.sym.to.R.v + nonconfirmed.asym.to.R.v + Qsym.to.R.v + Qasym.to.R.v
    dD.v = H.to.D.v + nonconfirmed.sym.to.D.v + nonconfirmed.asym.to.D.v + Qsym.to.D.v + Qasym.to.D.v
    
    ### Update compartments
    S.nv = S.nv + dS.nv
    FQ.nv = FQ.nv + dFQ.nv
    E.nv = E.nv + dE.nv
    Isym.nv = Isym.nv + dIsym.nv
    Iasym.nv = Iasym.nv + dIasym.nv
    Qsym.nv = Qsym.nv + dQsym.nv
    Qasym.nv = Qasym.nv + dQasym.nv
    H.nv = H.nv + dH.nv
    R.nv = R.nv + dR.nv
    D.nv = D.nv + dD.nv
    S.v = S.v + dS.v
    FQ.v = FQ.v + dFQ.v
    E.v = E.v + dE.v
    Isym.v = Isym.v + dIsym.v
    Iasym.v = Iasym.v + dIasym.v
    Qsym.v = Qsym.v + dQsym.v
    Qasym.v = Qasym.v + dQasym.v
    H.v = H.v + dH.v
    R.v = R.v + dR.v
    D.v = D.v + dD.v
    
    # Summary Data (response variables)
    Current.Infected.All = Isym.v + Isym.nv + Iasym.v + Iasym.nv
    Newly.Infected.All = new.infected.all.nv + new.infected.all.v
    Total.Infected.So.Far = Total.Infected.So.Far + Newly.Infected.All
    Current.Hospitalized.All = H.nv + H.v
    Peak.Hospitalized = max(c(Current.Hospitalized.All, Peak.Hospitalized))
    Total.Dead.So.Far  = D.nv + D.v
    if(Newly.Infected.All > Peak.Infected){
      Peak.Infected = Newly.Infected.All
      Peak.Day.Infected = day
    }
    if(Current.Infected.All < 20){
      Possible.End.Days = c(Possible.End.Days, day)
    }
    if(i %in% checkpoints){
      End.Day = min(Possible.End.Days[Possible.End.Days > Peak.Day.Infected])
      if(End.Day != Inf){
        break
      }
    }
  }
  End.Day = min(Possible.End.Days[Possible.End.Days > Peak.Day.Infected])
  summary_data = c(H.rate.nv, H.rate.v, p.fp.all, p.test.sym.nv, p.test.asym.nv, p.test.sym.v, p.test.asym.v,
                      I.rate.sym, I.rate.asym, p.sym.nv, p.sym.v, p.tp.sym.all, p.tp.asym.all,
                      Peak.Infected, Total.Infected.So.Far, Peak.Hospitalized, Total.Dead.So.Far, Peak.Day.Infected, End.Day)
  # return(list(summary_data, Possible.End.Days))
  return(summary_data)
}
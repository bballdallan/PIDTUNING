#' PID Tuning ZN Closed Loop Tuning
#' 
#' 1. Ziegler Nichols Closed Loop Tuning
#' Step 1 -  Set integral (I) and derivative (D) to 0
#' Step 2 -  Increase proportinal gain (P) from 0 until it reaches
#'           ultimate gain (Ku), i.e. when output of the control loop has
#'           stable and consistent oscillation
#' Step 3 -  Ku and oscillation period are used to set PID gains depending
#'           on the type of controller used

#' Control Types
#' 1.P                     -   Kp=0.5Ku
#' 2.PI                    -   Kp=0.45Ku;  Ti=Tu/1.2
#' 3.PD                    -   Kp=0.8Ku;               ;Td=Tu/8
#' 4.Classic PID           -   Kp=0.6Ku;   Ti=Tu/2     ;Td=Tu/8
#' 5.Pessen Integral Rule  -   Kp=0.7Ku;   Ti=Tu/2.5   ;Td=3Tu/20
#' 6.Some OVershoot        -   Kp=0.33Ku;  Ti=Tu/2     ;Td=Tu/3
#' 7.No OVershoot          -   Kp=0.2Ku;   Ti=Tu/2     ;Td=Tu/3
#' u(t)=Kp(e(t) + (1/Ti)int(0,t)(e(T)dT)  + Td*de(t)/dt)
#' @param : Ku, Tu, controltype
#' @return: PID tuning parameters
#' @export

PIDZNclose=function(Ku,Tu=0,controltype=4){
  para=vector("numeric",3)
  if (controltype==1){
    para[1]=0.5*Ku
  } else if (controltype==2){
    para[1]=0.45*Ku
    para[2]=Tu/1.2
  } else if (controltype==3){
    para[1]=0.8*Ku
    para[3]=Tu/8
  } else if (controltype==4){
    para[1]=0.6*Ku
    para[2]=Tu/2
    para[3]=Tu/8
  } else if (controltype==5){
    para[1]=0.7*Ku
    para[2]=Tu/2.5
    para[3]=3*Tu/20
  } else if (controltype==6){
    para[1]=0.33*Ku
    para[2]=Tu/2
    para[3]=Tu/3
  } else if (controltype==7){
    para[1]=0.2*Ku
    para[2]=Tu/2
    para[3]=Tu/8
  }
  print(paste("Ku=",para[1],",Ti=",para[2],", Td=",para[3]))
  return(para)
}


#' PID Tuning ZN Open Loop Tuning
#' 
#' 2. Ziegler Nichols Open Loop Tuning
#' Step 1 - Set controller to open loop
#' Step 2 - Introduce step change in actuator
#' Step 3 - Determine Steady State Gain K=/(u2-u1) => tangent at point of estimated maximum slope
#' Step 4 - Determine time delay (Td) => duration from change of u to the point where tangent cuts y1
#' Step 5 - Determine Time Constant (Ts) => duration where tangent cuts y1 and y2

#' Control Types
#' 1.P                     -   Kp=Ts/(Td*K)
#' 2.PI                    -   Kp=0.9*Ts/(Td*K);   Ti=3.3*Td
#' 3.PID                   -   Kp=1.2*Ts/(Td*K);   Ti=2.0*Td   ;Td=0.5*Td
#' @param : K, Ts, Td, controltype
#' @return: PID tuning parameters
#' @export
PIDZNopen=function(K,Ts,Td,controltype=3){
  para=vector("numeric",3)
  if (controltype==1){
    para[1]=Ts/(Td*K)
  } else if (controltype==2){
    para[1]=0.9*Ts/(Td*K)
    para[2]=3.3*Td
  } else if (controltype==3){
    para[1]=1.2*Ts/(Td*K)
    para[2]=2.0*Td
    para[3]=0.5*Td
  }
  print(paste("Ku=",para[1],",Ti=",para[2],", Td=",para[3]))
  return(para)
}
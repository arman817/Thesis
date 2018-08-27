TMLE_Stage2_BothCorrect_Medication1 <- c()
TMLE_Stage2_CorrectG_Medication1 <- c()
TMLE_Stage2_CorrectQ_Medication1 <- c()
TMLE_Stage2_BothIncorrect_Medication1 <- c()
GLM_Estimate_Incorrect_Medication1 <- c()
TMLE_Estimate_Medication1 <- c()
GLM_Stage2_Incorrect_Medication1 <- c()
Avg_TMLE_Medication1 <- c()

Seed_Value <- read.csv(file="Seed.csv", header=FALSE, sep=",")[,1]
##################################################################################
Func1 <- function(Cov,A,Y){
  data1 <- data.frame(Cov,Y,A)
  data2 <- data1[which(data1$A==1),]
  data3 <- data2[,1:9]
  model <- glm(Y~.,data=data3,family="binomial")
  data4 <- data1[,1:9]
  prediction <- predict.glm(model,type="response",newdata=data4)
  return(prediction)
}

Func2 <- function(Cov,A){
  data <- data.frame(Cov,A)
  model <- glm(A~.,data=data,family="binomial")
  prediction <- predict.glm(model,type="response",newdata=data)
  return(prediction)
}

Func3 <- function(A,Y){
  data1 <- data.frame(Y,A)
  data2 <- data1[which(data1$A==1),]
  model <- glm(Y~1,data=data2,family="binomial")
  prediction <- predict.glm(model,type="response",newdata=data1)
  return(prediction)
}

Func4 <- function(Y,Q,pi,A){
  pi.tronc<-pi
  if((1/min(pi.tronc))>200){
    q<-quantile(pi,c(0.01,1))
    pi.tronc[pi<q[1]]<-q[1]
    pi.tronc[pi>q[2]]<-q[2]
  }
  weight <- 1/pi.tronc
  data <- data.frame(weight,Q)
  data1 <- data.frame(weight,Q,Y,A)
  data2 <- data1[which(data1$A == 1),]
  h <- data2$weight
  #Update Step in the regression
  model <- glm(Y ~ -1 + h ,offset = qlogis(Q),data = data2,family = "binomial")
  #Predict Step
  prediction <- plogis(qlogis(Q) + coef(model)["h"]/pi)
  #prediction <- predict.glm(model, type = "response",newdata=data) #Q_star
  return(prediction)
}

Func5 <- function(Cov,A){
  data <- data.frame(Cov,A)
  model <- glm(A~.,data=data,family="binomial")
  prediction <- predict.glm(model,type="response",newdata=data)
  return(prediction)
}

Func6 <- function(Cov,A,A_ind){
  data1 <- data.frame(Cov,A,A_ind)
  data2 <- data1[which(data1$A_ind==1),]
  data3 <- data2[,1:9]
  model <- glm(A~.,data=data3,family="binomial")
  data4 <- data1[,1:9]
  prediction <- predict.glm(model,type="response",newdata=data4)
  return(prediction)
}

Func7 <- function(Cov,A,A_ind){
  data1 <- data.frame(Cov,A,A_ind)
  data2 <- data1[which(data1$A_ind==1),]
  data3 <- data2[,1:9]
  model <- glm(A~1,data=data3,family="binomial")
  data4 <- data1[,1:9]
  prediction <- predict.glm(model,type="response",newdata=data4)
  return(prediction)
}


################################################################################

F1unc1 <- function(Cov,A,Y,Kao){
  data1 <- data.frame(Cov,Y,A,Kao)
  data2 <- data1[which(data1$A==1),]
  data3 <- data2[,1:9]
  model <- glm(Y~., data = data3, weights = data2$Kao,family="binomial")
  data4 <- data1[,1:9]
  prediction <- predict.glm(model,type="response",newdata=data4)
  return(prediction)
}


F1unc3 <- function(A,Y){
  data1 <- data.frame(Y,A)
  data2 <- data1[which(data1$A==1),]
  model <- glm(Y~1,data=data2,family="binomial")
  prediction <- predict.glm(model,type="response",newdata=data1)
  return(prediction)
}

F1unc4 <- function(Y,Q,pi,A,Kao){
  pi.tronc<-pi
  if((1/min(pi.tronc))>200){
    q<-quantile(pi,c(0.01,1))
    pi.tronc[pi<q[1]]<-q[1]
    pi.tronc[pi>q[2]]<-q[2]
  }
  weight <- 1/pi.tronc
  data <- data.frame(weight,Q,Kao)
  data1 <- data.frame(weight,Q,Y,A,Kao)
  data2 <- data1[which(data1$A == 1),]
  h <- data2$weight
  #Update Step in the regression
  model <- glm(Y ~ -1 + h ,offset = qlogis(Q),data = data2, weights = data2$Kao, family = "binomial")
  #Predict Step
  prediction <- plogis(qlogis(Q) + coef(model)["h"]/pi)
  #prediction <- predict.glm(model, type = "response",newdata=data) #Q_star
  return(prediction)
}

F1unc5 <- function(Cov,A,Kao){
  data1 <- data.frame(Cov,A)
  model <- bayesglm(A~.,data=data1,weights = Kao,family="binomial")
  prediction <- predict.glm(model,type="response",newdata=data1)
  return(prediction)
}


F1unc6 <- function(Cov,A,A_ind,Kao){
  data1 <- data.frame(Cov,A,A_ind,Kao)
  data2 <- data1[which(data1$A_ind==1),]
  data3 <- data2[,1:9]
  model <- glm(A~.,data=data3,weights = data2$Kao,family="binomial")
  data4 <- data1[,1:9]
  prediction <- predict.glm(model,type="response",newdata=data4)
  return(prediction)
}

F1unc7 <- function(Cov,A,A_ind){
  data1 <- data.frame(Cov,A,A_ind)
  data2 <- data1[which(data1$A_ind==1),]
  data3 <- data2[,1:9]
  model <- glm(A~1,data=data3,family="binomial")
  data4 <- data1[,1:9]
  prediction <- predict.glm(model,type="response",newdata=data4)
  return(prediction)
}

################################################################################


####################################
for(iter in 1:1000){
  
  #Create Data for the Simulation
  #################################################################################
  set.seed(Seed_Value[iter])
  #set.seed(2014)
  N_Sampled <- 67
  Sample_Size <- rep(200,N_Sampled)
  V1 <- rnorm(N_Sampled,0.45,1)
  V2 <- rnorm(N_Sampled,0.5,1)
  U1 <- rnorm(N_Sampled,0.55,0.7)
  #U2 <- rnorm(N_Sampled,0.6,0.9)
  D1 <- NA
  D2 <- NA
  D3 <- NA
  i <- 1
  while(i <= N_Sampled){
    D1[i] <- rbinom(1,1,plogis(0.33+1.1*V1[i]))
    D2[i] <- rbinom(1,1,plogis(0.76+0.5*V2[i]))
    D3[i] <- rbinom(1,1,plogis(0.55+0.6*V1[i]))
    if((D1[i]+D2[i]+D3[i])>0)
      i <- i+1
  }
  V1_ij <- c()
  V2_ij <- c()
  U_ij <- c()
  D1_ij <- c()
  D2_ij <- c()
  D3_ij <- c()
  Study_ID <- c()
  for(i in 1:N_Sampled){
    V1_ij <- append(V1_ij,rep(V1[i],Sample_Size[i]))
    V2_ij <- append(V2_ij,rep(V2[i],Sample_Size[i]))
    U_ij <- append(U_ij,rep(U1[i],Sample_Size[i]))
    #U2_ij <- append(U2_ij,rep(U2[i],Sample_Size[i]))
    D1_ij <- append(D1_ij,rep(D1[i],Sample_Size[i]))
    D2_ij <- append(D2_ij,rep(D2[i],Sample_Size[i]))
    D3_ij <- append(D3_ij,rep(D3[i],Sample_Size[i]))
    Study_ID <- append(Study_ID,rep(i,Sample_Size[i]))
  }
  N <- sum(Sample_Size)
  U1_ij <- rnorm(N,mean=0.4*U_ij,sd = 0.8)
  W1_ij <- rnorm(N,mean=0.1+0.35*V1_ij,sd=0.5)
  W2_ij <- rnorm(N,mean=0.25*V2_ij,sd=0.6)
  A1_ij <- rbinom(N,1,D1_ij*plogis(-0.45+V1_ij+0.4*W1_ij+1.8*W2_ij))
  A2_ij <- rbinom(N,1,D2_ij*plogis(-0.55+2*V1_ij+W1_ij+W2_ij))
  A3_ij <- rbinom(N,1,D3_ij*plogis(-0.1+1.4*V1_ij+0.35*W1_ij+1.6*W2_ij))
  Y_ij <- rbinom(N,1,plogis(0.7-3.2*W1_ij+1.1*U1_ij*A1_ij+0.15*A2_ij-0.45*A3_ij*W2_ij))

  Full_IPD <- data.frame(Study_ID,W1_ij,W2_ij,A1_ij,A2_ij,A3_ij,Y_ij,D1_ij,D2_ij,D3_ij,V1_ij,V2_ij)
  Full_AD <- c()
  for(i in 1:N_Sampled){
    IPD <- Full_IPD[which(Full_IPD$Study_ID==i),]
    Full_AD <- rbind(Full_AD,colMeans(IPD))
  }
  colnames(Full_AD) <- c("Study_ID","W1_bar","W2_bar","A1_bar","A2_bar","A3_bar","Y_bar","D1_bar","D2_bar","D3_bar","V1","V2")
  Full_AD <- data.frame(Full_AD)
  pInd <- plogis(0.25+1.3*Full_AD$W1_bar*Full_AD$D1 - 1.2*Full_AD$W2_bar-
                   0.5*Full_AD$A1_bar + 0.52*Full_AD$A2_bar-0.7*Full_AD$Y_bar)
  Indicator_bar <- rbinom(N_Sampled,1,pInd)
  Full_AD <- cbind(Sample_Size,Full_AD,Indicator_bar)
  Indicator <- c()
  Study_Indicator <- c()
  for(i in 1:N_Sampled){
    Indicator <- append(Indicator,rep(Indicator_bar[i],Sample_Size[i]))
  }
  Full_IPD <- cbind(Full_IPD,Indicator)
  Model_Data <- data.frame(Full_AD$W1_bar,Full_AD$W2_bar,Full_AD$A1_bar,Full_AD$A2_bar,Full_AD$A3,Full_AD$Y_bar,Full_AD$D1_bar,Full_AD$D2_bar,Full_AD$D3_bar,Full_AD$V1,Full_AD$V2)
  modelijk <- glm(Indicator_bar~.,data=Model_Data,family="binomial")
  Pi_n <- predict.glm(modelijk,type="response",newdata=Model_Data)
  for(i in 1:N_Sampled){
    Study_Indicator <- append(Study_Indicator,rep(Pi_n[i],Sample_Size[i]))
  }
  Full_IPD <- cbind(Full_IPD,Study_Indicator)
  Full_AD <- cbind(Full_AD,Pi_n)
  #################################################################################
  
  
  #################################################################################
  #For Estimating Simple TMLE for medication 1
  #################################################################################
  Interested_IPD <- Full_IPD[which(Full_IPD$Indicator==1 & Full_IPD$D1_ij==1),]
  Covariates <- cbind(Interested_IPD$W1_ij,Interested_IPD$W2_ij,Interested_IPD$A2_ij,Interested_IPD$A3_ij,Interested_IPD$D2_ij,Interested_IPD$D3_ij,Interested_IPD$V1_ij,Interested_IPD$V2_ij)
  Medication_Interest <- Interested_IPD$A1_ij
  Outcome <- Interested_IPD$Y_ij
  GLM_Estimate <- Func1(Covariates,Medication_Interest,Outcome)
  G <- Func2(Covariates,Medication_Interest)
  TMLE_Estimate <- Func4(Outcome,GLM_Estimate,G,Medication_Interest)
  GLM_Estimate_Incorrect <- Func3(Medication_Interest,Outcome)
  TMLE_Estimate_Incorrect <- Func4(Outcome,GLM_Estimate_Incorrect,G,Medication_Interest)
  
  
  #################################################################################
  #For Estimating Two Stage TMLE for medication 1
  #################################################################################
  #First Stage
  Interested_IPD <- Full_IPD[which(Full_IPD$Indicator==1),]
  Covariates <- cbind(Interested_IPD$W1_ij,Interested_IPD$W2_ij,Interested_IPD$A2_ij,Interested_IPD$A3_ij,Interested_IPD$D2_ij,Interested_IPD$D3_ij,Interested_IPD$V1_ij,Interested_IPD$V2_ij)
  Ind_Interest <- 1/Interested_IPD$Study_Indicator
  Medication_Interest <- Interested_IPD$A1_ij
  Indicator_1 <- Interested_IPD$D1_ij
  Outcome <- Interested_IPD$Y_ij
  Stage1_GLM_Estimate <- F1unc1(Covariates,Medication_Interest,Outcome,Ind_Interest)
  Stage1_GLM_Estimate_Incorrect <- F1unc3(Medication_Interest,Outcome)
  Interested_AD <- Full_AD[which(Full_AD$Indicator_bar==1),]
  Avg_Covariates <- cbind(Interested_AD$V1,Interested_AD$V2)
  Avg_Indicator <- Interested_AD$D1_bar
  Size <- Interested_AD$Sample_Size
  Blahblah <- 1/Interested_AD$Pi_n
  P1 <- F1unc5(Avg_Covariates,Avg_Indicator,Blahblah)
  G1 <- c()
  for(i in 1:length(Interested_AD[,1])){
    G1 <- append(G1,rep(P1[i],Size[i]))
  }
  G2_Correct <- F1unc6(Covariates,Medication_Interest,Indicator_1,Ind_Interest)
  G2_Incorrect <- F1unc7(Covariates,Medication_Interest,Indicator_1)
  G_Correct <- G1*G2_Correct
  G_Incorrect <- G1*G2_Incorrect
  
  #Every time
  TMLE_Stage1_Estimate_CorrectQandG <- F1unc4(Outcome,Stage1_GLM_Estimate,G_Correct,Medication_Interest,Ind_Interest)
  TMLE_Stage1_Estimate_IncorrectQandCorrectG <- F1unc4(Outcome,Stage1_GLM_Estimate_Incorrect,G_Correct,Medication_Interest,Ind_Interest)
  TMLE_Stage1_Estimate_CorrectQandIncorrectG <- F1unc4(Outcome,Stage1_GLM_Estimate,G_Incorrect,Medication_Interest,Ind_Interest)
  TMLE_Stage1_Estimate_IncorrectQandG <- F1unc4(Outcome,Stage1_GLM_Estimate_Incorrect,G_Incorrect,Medication_Interest,Ind_Interest)
  #TMLE_Stage1_Estimate_Incorrect <- Func4(Outcome,Stage1_GLM_Estimate_Incorrect,G,Medication_Interest)
  Size_TMLE <- data.frame(cbind(TMLE_Stage1_Estimate_CorrectQandG,TMLE_Stage1_Estimate_IncorrectQandCorrectG,
                                TMLE_Stage1_Estimate_CorrectQandIncorrectG,TMLE_Stage1_Estimate_IncorrectQandG,
                                Stage1_GLM_Estimate,Stage1_GLM_Estimate_Incorrect,Interested_IPD$Study_ID))
  colnames(Size_TMLE) <- c("TMLE_Est_BothCorrect","TMLE_Est_CorrectG","TMLE_Est_CorrectQ","TMLE_Est_BothIncorrectQ",
                           "GLM_Est","GLM_Est_Incorrect","Study_ID")
  TMLE_Both_Correct <- NA
  TMLE_Correct_G <- NA
  TMLE_Correct_Q <- NA
  TMLE_Both_Incorrect <- NA
  Avg_GLM <- NA
  Avg_GLM_Incorrect <- NA
  Just_F <- NA
  for(i in 1:length(Interested_AD[,1])){
    New_Data <- Size_TMLE[which(Size_TMLE$Study_ID == Interested_AD$Study_ID[i]),]
    TMLE_Both_Correct[i] <- mean(New_Data$TMLE_Est_BothCorrect)
    TMLE_Correct_G[i] <- mean(New_Data$TMLE_Est_CorrectG)
    TMLE_Correct_Q[i] <- mean(New_Data$TMLE_Est_CorrectQ)
    TMLE_Both_Incorrect[i] <- mean(New_Data$TMLE_Est_BothIncorrectQ)
    Avg_GLM[i] <- mean(New_Data$GLM_Est)
    Avg_GLM_Incorrect[i] <- mean(New_Data$GLM_Est_Incorrect)
  }
  TMLE_Stage1_CorrectQandG <- rep(0,N_Sampled)
  TMLE_Stage1_IncorrectQandCorrectG <- rep(0,N_Sampled)
  TMLE_Stage1_CorrectQandIncorrectG <- rep(0,N_Sampled)
  TMLE_Stage1_IncorrectQandG <- rep(0,N_Sampled)
  GLM_Stage1 <- rep(0,N_Sampled)
  GLM_Stage1_Incorrect <- rep(0,N_Sampled)
  j <- 1
  for(i in 1:N_Sampled){
    if(j<=length(Interested_AD[,1])){
      if(is.element(Interested_AD$Study_ID[j],Full_AD$Study_ID[i])){
        TMLE_Stage1_CorrectQandG[i] <- TMLE_Both_Correct[j]
        TMLE_Stage1_IncorrectQandCorrectG[i] <- TMLE_Correct_G[j]
        TMLE_Stage1_CorrectQandIncorrectG[i] <- TMLE_Correct_Q[j]
        TMLE_Stage1_IncorrectQandG[i] <- TMLE_Both_Incorrect[j]
        GLM_Stage1[i] <- Avg_GLM[j]
        GLM_Stage1_Incorrect[i] <- Avg_GLM_Incorrect[j]
        j <- j+1
      }
    }
  }
  #Second_Stage
  #Indicator bar vs others
  TMLE_Stage2_BothCorrect <- Indicator_bar*TMLE_Stage1_CorrectQandG/Pi_n
  TMLE_Stage2_Correct_G <- Indicator_bar*TMLE_Stage1_IncorrectQandCorrectG/Pi_n
  TMLE_Stage2_Correct_Q <- Indicator_bar*TMLE_Stage1_CorrectQandIncorrectG/Pi_n
  TMLE_Stage2_BothInorrect <- Indicator_bar*TMLE_Stage1_IncorrectQandG/Pi_n
  GLM_Stage2 <- Indicator_bar*GLM_Stage1/Pi_n
  GLM_Stage2_Incorrect <- Indicator_bar*GLM_Stage1_Incorrect/Pi_n
  #################################################################################
  
  #################################################################################
  #Save the results
  #################################################################################
  GLM_Estimate_Incorrect_Medication1[iter] <- mean(GLM_Estimate_Incorrect)
  TMLE_Estimate_Medication1[iter] <- mean(TMLE_Estimate)
  Avg_TMLE_Medication1[iter] <- mean(TMLE_Both_Correct)
  GLM_Stage2_Incorrect_Medication1[iter] <- mean(GLM_Stage2_Incorrect)
  
  TMLE_Stage2_BothCorrect_Medication1[iter] <- mean(TMLE_Stage2_BothCorrect)
  TMLE_Stage2_CorrectG_Medication1[iter] <- mean(TMLE_Stage2_Correct_G)
  TMLE_Stage2_CorrectQ_Medication1[iter] <- mean(TMLE_Stage2_Correct_Q)
  TMLE_Stage2_BothIncorrect_Medication1[iter] <- mean(TMLE_Stage2_BothInorrect)
  #################################################################################
  print(iter)
}


mean(TMLE_Stage2_BothCorrect_Medication1)
mean(TMLE_Stage2_CorrectG_Medication1)
mean(TMLE_Stage2_CorrectQ_Medication1)
mean(TMLE_Stage2_BothIncorrect_Medication1)
mean(Avg_TMLE_Medication1)  
mean(TMLE_Estimate_Medication1) 
mean(GLM_Estimate_Incorrect_Medication1)

sd(TMLE_Stage2_BothCorrect_Medication1)
mean(TMLE_Stage2_BothCorrect_Medication1)-1.96*sd(TMLE_Stage2_BothCorrect_Medication1)
mean(TMLE_Stage2_BothCorrect_Medication1)+1.96*sd(TMLE_Stage2_BothCorrect_Medication1)

sd(TMLE_Stage2_CorrectG_Medication1)
mean(TMLE_Stage2_CorrectG_Medication1)-1.96*sd(TMLE_Stage2_CorrectG_Medication1)
mean(TMLE_Stage2_CorrectG_Medication1)+1.96*sd(TMLE_Stage2_CorrectG_Medication1)

sd(TMLE_Stage2_CorrectQ_Medication1)
mean(TMLE_Stage2_CorrectQ_Medication1)-1.96*sd(TMLE_Stage2_CorrectQ_Medication1)
mean(TMLE_Stage2_CorrectQ_Medication1)+1.96*sd(TMLE_Stage2_CorrectQ_Medication1)

sd(TMLE_Stage2_BothIncorrect_Medication1)
mean(TMLE_Stage2_BothIncorrect_Medication1)-1.96*sd(TMLE_Stage2_BothIncorrect_Medication1)
mean(TMLE_Stage2_BothIncorrect_Medication1)+1.96*sd(TMLE_Stage2_BothIncorrect_Medication1)

sd(Avg_TMLE_Medication1)
mean(Avg_TMLE_Medication1)-1.96*sd(Avg_TMLE_Medication1)
mean(Avg_TMLE_Medication1)+1.96*sd(Avg_TMLE_Medication1)

sd(TMLE_Estimate_Medication1)
mean(TMLE_Estimate_Medication1)-1.96*sd(TMLE_Estimate_Medication1)
mean(TMLE_Estimate_Medication1)+1.96*sd(TMLE_Estimate_Medication1)

sd(GLM_Estimate_Incorrect_Medication1)
mean(GLM_Estimate_Incorrect_Medication1)-1.96*sd(GLM_Estimate_Incorrect_Medication1)
mean(GLM_Estimate_Incorrect_Medication1)+1.96*sd(GLM_Estimate_Incorrect_Medication1)
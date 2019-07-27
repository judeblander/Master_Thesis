#### Creation of minimum variance portfolio function ####
min.var.portfolio <- function(covmat, mean.returns, Rfdaily, short = "no", max.allocation, min.allocation, names)
{
  n <- ncol(covmat)
  zeros <- array(0, dim = c(n,1))
  
  # Fully investment constraint
  Amat <- cbind(rep(1,n))
  bvec <- 1
  meq <- 1
  
 if(short=="no"){
    Amat <- cbind(Amat, diag(n))
    bvec <- c(bvec, rep(0, n))
  }
  
  # If max allocation (concentration) is specified
  if(!is.null(max.allocation)){
    if(max.allocation > 1 | max.allocation <0){
      stop("max.allocation must be greater than 0 and less than 1")
    }
    if(max.allocation * n < 1){
      stop("Need to set max.allocation higher; not enough assets to add to 1")
    }
    Amat <- cbind(Amat, -diag(n))
    bvec <- c(bvec, rep(-max.allocation, n))
  }
  
  # If min allocation (concentration) is specified
  if(!is.null(min.allocation)){
    
    Amat <- cbind(Amat, diag(n))
    bvec <- c(bvec, rep(min.allocation, n))
  }
  
  sol <- solve.QP(covmat, dvec=zeros, Amat=Amat, bvec=bvec, meq=1)
  min.var <- matrix(nrow=1, ncol=n+3)
  colnames(min.var) <- c(colnames(covmat), "Std.Dev", "Exp.Return", "sharpe")
  min.var[1,1:n] <- sol$solution
  min.var[1,"Std.Dev"] <- sqrt(sol$solution%*%covmat%*%sol$solution)
  min.var[1,"Exp.Return"] <- as.numeric(sol$solution %*% mean.returns)
  min.var[1,"sharpe"] <- (min.var[1,"Exp.Return"]- Rfdaily) / min.var[1,"Std.Dev"]
  return(as.data.frame(min.var))
}

#### Creation of efficient portfolio function ####
Eff.portfolio <- function(covmat, mean.returns,Rfdaily, short ="no",target.return,max.allocation, min.allocation, names)
{
  n <- ncol(covmat)
  
  # Create initial Amat and bvec assuming only equality constraint
  # (short-selling is allowed, no allocation constraints)
  Amat <- cbind(rep(1,n),mean.returns)
  bvec <- c(1,target.return)
  meq <- 1
  
  # Then modify the Amat and bvec if short-selling is prohibited

if(short=="no"){
    Amat <- cbind(Amat, diag(n))
    bvec <- c(bvec, rep(0, n))
  }
  
  # And modify Amat and bvec if a max allocation (concentration) is specified
  if(!is.null(max.allocation)){
    if(max.allocation > 1 | max.allocation <0){
      stop("max.allocation must be greater than 0 and less than 1")
    }
    if(max.allocation * n < 1){
      stop("Need to set max.allocation higher; not enough assets to add to 1")
    }
    Amat <- cbind(Amat, -diag(n))
    bvec <- c(bvec, rep(-max.allocation, n))
  }
  
  # And modify Amat and bvec if a max allocation (concentration) is specified
  if(!is.null(min.allocation)){
    #if(min.allocation > -1 | min.allocation < 0){
      #stop("min.allocation must be greater than -1 and less than 0")
    #}
    #if(min.allocation * n < -1){
      #stop("Need to set min.allocation higher; not enough assets to add to 1")
    #}
    Amat <- cbind(Amat, diag(n))
    bvec <- c(bvec, rep(min.allocation, n))
  }

    sol <- solve.QP(covmat, dvec=mean.returns, Amat=Amat, bvec=bvec, meq=2, FALSE)
  # Initialize a matrix to contain allocation and statistics
  # This is not necessary, but speeds up processing and uses less memory
  eff <- matrix(nrow=1, ncol=n+3)
  # Now I need to give the matrix column names
  colnames(eff) <- c(colnames(covmat), "Std.Dev", "Exp.Return", "sharpe")
  eff[1,1:n] <- sol$solution
  eff[1,"Std.Dev"] <- sqrt(sol$solution%*%covmat%*%sol$solution)
  eff[1,"Exp.Return"] <- as.numeric(sol$solution %*% mean.returns)
  eff[1,"sharpe"] <- (eff[1,"Exp.Return"] - Rfdaily) / eff[1,"Std.Dev"]
  return(as.data.frame(eff))
}
  
#### Creation of efficient frontier function ####
eff.frontier <- function (covmat, mean.returns,Rfdaily, short="no", max.allocation=NULL, min.allocation = NULL,risk.premium.up=5, risk.increment=.005)
  {
  # return argument should be a m x n matrix with one column per security
  # short argument is whether short-selling is allowed; default is no (short
  # selling prohibited)max.allocation is the maximum % allowed for any one
  # security (reduces concentration) risk.premium.up is the upper limit of the
  # risk premium modeled (see for loop below) and risk.increment is the
  # increment (by) value used in the for loop
  
  n <- ncol(covmat)
  
  # Create initial Amat and bvec assuming only equality constraint
  # (short-selling is allowed, no allocation constraints)
  Amat <- matrix (1, nrow=n)
  bvec <- 1
  meq <- 1
  
  # Then modify the Amat and bvec if short-selling is prohibited
  if(short=="no"){
    Amat <- cbind(1, diag(n))
    bvec <- c(bvec, rep(0, n))
  }
  
  # And modify Amat and bvec if a max allocation (concentration) is specified
  if(!is.null(max.allocation)){
    if(max.allocation > 1 | max.allocation <0){
      stop("max.allocation must be greater than 0 and less than 1")
    }
    if(max.allocation * n < 1){
      stop("Need to set max.allocation higher; not enough assets to add to 1")
    }
    Amat <- cbind(Amat, -diag(n))
    bvec <- c(bvec, rep(-max.allocation, n))
  }
  
  # And modify Amat and bvec if a max allocation (concentration) is specified
  if(!is.null(min.allocation)){

    Amat <- cbind(Amat, diag(n))
    bvec <- c(bvec, rep(min.allocation, n))
  }
  
  # Calculate the number of loops
  loops <- risk.premium.up / risk.increment + 1
  loop <- 1
  
  # Initialize a matrix to contain allocation and statistics
  # This is not necessary, but speeds up processing and uses less memory
  eff <- matrix(nrow=loops, ncol=n+3)
  # Now I need to give the matrix column names
  colnames(eff) <- c(colnames(covmat), "Std.Dev", "Exp.Return", "sharpe")
  
  # Loop through the quadratic program solver
  for (i in seq(from=0, to=risk.premium.up, by=risk.increment)){
    dvec <- mean.returns * i # This moves the solution along the EF
    sol <- solve.QP(covmat, dvec=dvec, Amat=Amat, bvec=bvec, meq=meq)
    eff[loop,"Std.Dev"] <- sqrt(sum(sol$solution*colSums((covmat*sol$solution))))
    eff[loop,"Exp.Return"] <- as.numeric(sol$solution %*% mean.returns)
    eff[loop,"sharpe"] <- (eff[loop,"Exp.Return"]- Rfdaily) / eff[loop,"Std.Dev"]
    eff[loop,1:n] <- sol$solution
    loop <- loop+1
  }
  
  return(as.data.frame(eff))
}


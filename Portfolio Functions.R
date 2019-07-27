#### Creation of minimum variance portfolio function ####
min.var.portfolio <- function(covmat, mean.returns, Rfdaily, short = "no", max.allocation, min.allocation, names)
{
  n <- ncol(covmat)
  zeros <- array(0, dim = c(n,1))
  
  # Fully investment constraint
  Amat <- cbind(rep(1,n))
  bvec <- 1
  meq <- 1
  
  # Modify the Amat and bvec if short-selling is prohibited
  if(short=="no"){
    Amat <- cbind(Amat, diag(n))
    bvec <- c(bvec, rep(0, n))
  }
  
  # If max allocation is specified
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
  
  # If min allocation is specified
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
  
  # Fully investment constraint
  Amat <- cbind(rep(1,n),mean.returns)
  bvec <- c(1,target.return)
  meq <- 1
  
  # Modify the Amat and bvec if short-selling is prohibited
  if(short=="no"){
    Amat <- cbind(Amat, diag(n))
    bvec <- c(bvec, rep(0, n))
  }
  
  # Modify Amat and bvec if a max allocation is specified
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
  
  # Modify Amat and bvec if a max allocation (concentration) is specified
  if(!is.null(min.allocation)){

    Amat <- cbind(Amat, diag(n))
    bvec <- c(bvec, rep(min.allocation, n))
  }

  sol <- solve.QP(covmat, dvec=mean.returns, Amat=Amat, bvec=bvec, meq=2, FALSE)

  eff <- matrix(nrow=1, ncol=n+3)
  
  # Give the matrix column names
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
  
  n <- ncol(covmat)
  
  # Fully investment constraint
  Amat <- matrix (1, nrow=n)
  bvec <- 1
  meq <- 1
  
  # Modify the Amat and bvec if short-selling is prohibited
  if(short=="no"){
    Amat <- cbind(1, diag(n))
    bvec <- c(bvec, rep(0, n))
  }
  
  # Modify Amat and bvec if a max allocation (concentration) is specified
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
  
  # Modify Amat and bvec if a max allocation (concentration) is specified
  if(!is.null(min.allocation)){

    Amat <- cbind(Amat, diag(n))
    bvec <- c(bvec, rep(min.allocation, n))
  }
  
  # Calculate the number of loops
  loops <- risk.premium.up / risk.increment + 1
  loop <- 1
  
  eff <- matrix(nrow=loops, ncol=n+3)
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


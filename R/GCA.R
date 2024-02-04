Stats <- function( dat ){

  Marg.Dist <- CDF <- Score <- Reg.Grad <- Cor.Grad <- Min <- Max <- Avg <- Std <- vector( "list", 2 )

  Marg.Dist[[ 1 ]] <- rowSums( dat )
  Marg.Dist[[ 2 ]] <- colSums( dat )

  CDF[[ 1 ]] <- cumsum( Marg.Dist[[ 1 ]] )
  CDF[[ 2 ]] <- cumsum( Marg.Dist[[ 2 ]] )

  Score[[ 1 ]] <- c( 0, head( CDF[[ 1 ]], -1 ) ) + 0.5 * Marg.Dist[[ 1 ]]
  Score[[ 2 ]] <- c( 0, head( CDF[[ 2 ]], -1 ) ) + 0.5 * Marg.Dist[[ 2 ]]

  names( Score[[ 1 ]] ) <- names( Marg.Dist[[ 1 ]] )
  names( Score[[ 2 ]] ) <- names( Marg.Dist[[ 2 ]] )

  Reg.Grad[[ 1 ]] <- rowSums( sweep( dat, 2, Score[[ 2 ]], "*" ) ) / Marg.Dist[[ 1 ]]
  Reg.Grad[[ 2 ]] <- colSums( sweep( dat, 1, Score[[ 1 ]], "*" ) ) / Marg.Dist[[ 2 ]]

  Cor.Grad[[ 1 ]] <-  cumsum( ( Marg.Dist[[ 1 ]] * Reg.Grad[[ 1 ]] ) / sum( Marg.Dist[[ 1 ]] * Reg.Grad[[ 1 ]] ) )
  Cor.Grad[[ 2 ]] <-  cumsum( ( Marg.Dist[[ 2 ]] * Reg.Grad[[ 2 ]] ) / sum( Marg.Dist[[ 2 ]] * Reg.Grad[[ 2 ]] ) )

  Min[[ 1 ]]  <- apply( dat, 1, function(x){ min(x) } )
  Min[[ 2 ]]  <- apply( dat, 2, function(x){ min(x) } )

  Max[[ 1 ]]  <- apply( dat, 1, function(x){ max(x) } )
  Max[[ 2 ]]  <- apply( dat, 2, function(x){ max(x) } )

  Avg[[ 1 ]]  <- apply( dat, 1, function(x){ mean(x) } )
  Avg[[ 2 ]]  <- apply( dat, 2, function(x){ mean(x) } )

  Std[[ 1 ]]  <- apply( dat, 1, function(x){ ifelse( length(x) == 1, 0, sd(x) ) } )
  Std[[ 2 ]]  <- apply( dat, 2, function(x){ ifelse( length(x) == 1, 0, sd(x) ) } )

  return( list( Marg.Dist = Marg.Dist, CDF = CDF, Score = Score, Reg.Grad = Reg.Grad, Cor.Grad = Cor.Grad,
                Min = Min, Max = Max, Avg = Avg, Std = Std ) )

}

Ar <- function( cdf, corgrad, cdfMin = 0 , corMin = 0 ){

  cdf_lag <- c( cdfMin, head( cdf, -1 ) )
  corgrad_lag <- c( corMin, head( corgrad, -1 ) )
  ar <- sum( ( corgrad + corgrad_lag ) * ( cdf - cdf_lag ) )

  return( ar )

}

OptimGrad <- function( param, dat ){

  return( Ar( dat[[1]][param], dat[[2]][param] ) )

}

Rho <- function( dat, score ){

  rho <- 3 * sum( sweep( sweep( dat, 2, 2 * score[[ 2 ]] - 1, "*" ), 1, 2 * score[[ 1 ]] - 1, "*" ) )

  return( rho )

}

Gini <- function( dat ){

  n <- length( dat )
  avg <- mean( dat )
  gini <- sum( ( 2 * 1:n - n - 1) * dat ) / ( n^2 * avg )
  return( gini )

}

AutoRevert <- function( dat, res ){

  rIndx <- grep( "row[[:digit:]]", colnames(res) )
  cIndx <- grep( "col[[:digit:]]", colnames(res) )

  n <- nrow( res )
  Res <- list()

  repeat{

    P <- cbind( res[ 1, rIndx ], res[ 1, cIndx ] )
    P_ <- cbind( rev( res[ 1, rIndx ] ), rev( res[ 1, cIndx ] ) )

    whichP <- apply( res[ , -ncol(res) ], 1, function( x, r ){ all( x == r ) }, r = P )
    whichP_ <- apply( res[ , -ncol(res) ], 1, function( x, r ){ all( x == r ) }, r = P_ )

    tabReorder <- TableReorder( AddClWe( dat ), P[, rIndx ], P[, cIndx ] )

    giniR <- Gini( attr( tabReorder, "Stats" )$Marg.Dist[[ 1 ]] )
    giniC <- Gini( attr( tabReorder, "Stats" )$Marg.Dist[[ 2 ]] )
    ArL <- Ar( attr( tabReorder, "Stats" )$Marg.Dist[[ 1 ]], RemoveClWe( tabReorder )[, 1 ] )
    ArR <- Ar( attr( tabReorder, "Stats" )$Marg.Dist[[ 1 ]], RemoveClWe( tabReorder )[, ncol(tabReorder)-2 ] )
# print("P");
# print(whichP);
# print(whichP_);
# print("Gini");
# print(giniR);
# print(giniC);
# print("Ar");
# print(ArL);
# print(ArR);
    Res[[ length(Res) + 1 ]] <- ChooseRevert( res, whichP, whichP_, giniR, giniC, ArL, ArR )
    res <- res[ !(whichP | whichP_), , drop = F ]

    if( nrow(res) == 0 ) break

  }

  Res <- do.call( "rbind", Res )
  Res <- Res[ order( -Res[, ncol(Res) ] ), ]
  rownames( Res ) <- NULL

  return( Res )

}

ChooseRevert <- function( res, whichP, whichP_, giniR, giniC, ArL, ArR ){

  Res <- if( giniR < 0 ){

    res[ whichP_, ]

  }else if( giniR > 0 ){

    res[ whichP, ]

  }else if( giniR == 0 & giniC < 0 ){

    res[ whichP_, ]

  }else if( giniR == 0 & giniC > 0 ){

    res[ whichP, ]

  }else if( giniR == 0 & giniC == 0 & ArL < ArR ){

    res[ whichP_, ]

  }else{

    res[ whichP, ]

  }

  return( Res )

}

#' GCA
#' @title GCA
#'
#' @description GCA algorithm based on Spearman’s rho.
#'
#' @param dat Data frame or matrix based on the XGrade() and RemoveClWe() functions.
#' @param iter Number of iterations.
#'
#' @return Data frame with columns and rows ordering and Spearman's rho.
#'
#' @export GCA
#'
#' @examples
#' \dontrun{
#' X <- matrix( c(0.25,0.1,0.1,0.05,0.2,0.05,0.04,0.03,0.03,0.06,0.02,0.07), 3, 4 )
#' colnames(X) <- c("j1","j2","j3","j4")
#' rownames(X) <- c("i1","i2","i3")
#' X <- AddClWe( X )
#' XGrade <- TableGCA( X )
#' GCA( RemoveClWe( XGrade ), 100 )
#' }
#'

GCA <- function( dat, iter ){

  Stop <- NonZeroRowsCols( dat )
  if( Stop ){

    return( invisible() )

  }

  if( !any( class( dat ) %in% "TabGCA" ) ){

    dat <- TableGCA( AddClWe( dat ) )

  }

  rowPerm <- do.call( "rbind", lapply( 1:iter, function(x){ sample( 1:nrow(dat), nrow(dat) ) } ) )
  colPerm <- do.call( "rbind", lapply( 1:iter, function(x){ sample( 1:ncol(dat), ncol(dat) ) } ) )

  rowRes <- matrix( 0, iter, nrow(dat) )
  colRes <- matrix( 0, iter, ncol(dat) )

  rhoResults <- double( iter )

  for( i in 1:iter ){

    Row1 <- rowPerm[i,]
    Col1 <- colPerm[i,]

    repeat{

      Changed <- F

      stats <- Stats( dat[ Row1, Col1 ] )
      Row2 <- Row1[ order( stats$Reg.Grad[[ 1 ]] ) ]

      if( any( Row1 != Row2 ) ){ Row1 <- Row2; Changed <- T }

      stats <- Stats( dat[ Row1, Col1 ] )
      Col2 <- Col1[ order( stats$Reg.Grad[[ 2 ]] ) ]

      if( any( Col1 != Col2 ) ){ Col1 <- Col2; Changed <- T }

      if( Changed == F ) break

    }

    rowRes[i,] <- Row1
    colRes[i,] <- Col1

    rhoResults[i] <- Rho( dat[ Row1, Col1 ], stats$Score )

  }

  res <- data.frame( rowRes, colRes, rhoResults )
  res <- res[ order(-res[,ncol(res)]), ]
  res <- unique( res )

  colnames(res)[-ncol(res)] <- c( paste0( "row", 1:nrow(dat) ), paste0( "col", 1:ncol(dat) ) )
  # print( res )
  Res <- AutoRevert( dat, res )

  return( Res )

}

RhoAggTest <- function( dat ){

  nRow <- nrow( dat )-2
  nCol <- ncol( dat )-2

  itRow <- min( nRow, 30 )
  itCol <- min( nCol, 30 )

  rhoRow <- double( itRow )
  rhoCol <- double( itCol )

  for( i in 2:itRow ){
    dat <- Cluster( dat, N = i, byRow = TRUE )
  }
  for( i in 2:itCol ){
    dat <- Cluster( dat, N = i, byRow = FALSE )
  }

  for( i in 2:itRow ){

    temp <- AggTable( dat, TRUE, NULL, i-1 )
    temp <- TableGCA( temp )
    rhoRow[ i ] <- Rho( RemoveClWe( temp ), attr( temp, "Stats" )$Score )

  }
  for( i in 2:itCol ){

    temp <- AggTable( dat, FALSE, NULL, i-1 )
    temp <- TableGCA( temp )
    rhoCol[ i ] <- Rho( RemoveClWe( temp ), attr( temp, "Stats" )$Score )

  }

  out <- list( Row = rhoRow, Col = rhoCol )
  return( out )

}

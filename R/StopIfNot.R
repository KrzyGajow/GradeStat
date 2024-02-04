NonZeroRowsCols <- function( dat ){

  if( is.null( attr(dat,"Stats")$Marg.Dist ) ){

    Row <- rowSums( dat )
    Col <- colSums( dat )

  }else{

    Row <- attr(dat,"Stats")$Marg.Dist[[1]]
    Col <- attr(dat,"Stats")$Marg.Dist[[2]]

  }

  if( any( Col == 0 ) | any ( Row == 0 ) ){

    cat("\n\n********** PROGRAM TERMINATED **********\n\n")
    cat( "The GradeStat requires a non-zero sum of every row and column in a table. \n\n" )
    cat( sprintf( "The following rows have zero sum: %s \n\n",
                  paste( rownames( dat )[ which( Row == 0 ) ], collapse = ", " ) ) )
    cat( sprintf( "The following columns have zero sum: %s",
                  paste( colnames( dat )[ which( Col == 0 ) ], collapse = ", " ) ) )
    cat("\n\n********** PROGRAM TERMINATED **********\n\n")

    return( T )

  }

  return( F )

}

NonZeroRowsColsShiny <- function( dat ){

  Row <- rowSums( dat )
  Col <- colSums( dat )

  if( any( Col == 0 ) | any ( Row == 0 ) ){

    Info <- list( Info = "The GradeStat requires a non-zero sum of every row and column in a table.",
                  Rows = sprintf( "The following rows have zero sum: %s.", paste( rownames( dat )[ which( Row == 0 ) ], collapse = ", " ) ),
                  Cols = sprintf( "The following columns have zero sum: %s.", paste( colnames( dat )[ which( Col == 0 ) ], collapse = ", " ) ) )

    return( Info )

  }

  return( F )

}

CharCols <- function( dat ){

  if( !any( class( dat ) %in% c( "data.frame", "data.table" ) ) ){

    dat <- as.data.frame( dat )

  }

  ColType <- unlist( lapply( dat, typeof ) )

  if( any( ColType == "character" ) ){

    cat("\n\n********** PROGRAM TERMINATED **********\n\n")
    cat( sprintf( "The following columns have non-numerical type: %s.",
                  paste( colnames( dat )[ which( ColType == "character" ) ], collapse = ", " ) ) )
    cat("\n\n********** PROGRAM TERMINATED **********\n\n")
    return( T )

  }

  return( F )

}

CharColsShiny <- function( dat ){

  ColType <- unlist( lapply( dat, typeof ) )

  if( any( ColType == "character" ) ){

    Info <- sprintf( "The following columns have non-numerical type: %s.",
                     paste( colnames( dat )[ which( ColType == "character" ) ], collapse = ", " ) )
    return( Info )

  }

  return( F )

}

UndefinedRecords <- function( dat ){

  if( !any( class( dat ) %in% c( "data.frame", "data.table" ) ) ){

    dat <- as.data.frame( dat )

  }

  UndefRec <- unlist( lapply( dat, function(x){ any( !is.finite(x) ) } ) )

  if( any( UndefRec ) ){

    cat("\n\n********** PROGRAM TERMINATED **********\n\n")
    cat( sprintf( "The following columns have undefined values (i.e. Inf, NA, NaN, char): %s.",
                  paste( colnames( dat )[ which( UndefRec ) ], collapse = ", " ) ) )
    cat("\n\n********** PROGRAM TERMINATED **********\n\n")

    return( T )

  }

  return( F )

}

UndefinedRecordsShiny <- function( dat ){

  UndefRec <- unlist( lapply( dat, function(x){ any( !is.finite(x) ) } ) )

  if( any( UndefRec ) ){

    Info <- sprintf( "The following columns have undefined values (i.e. Inf, NA, NaN, char): %s.",
                     paste( colnames( dat )[ which( UndefRec ) ], collapse = ", " ) )
    return( Info )

  }

  return( F )

}

TableRawCheckShiny <- function( dat ){

  dat <- dat[ -c( (nrow(dat)-1):nrow(dat) ), -c( (ncol(dat)-1):ncol(dat) ) ]

  Stop1 <- CharColsShiny( dat )
  Stop2 <- UndefinedRecordsShiny( dat )

  if( is.character( Stop1 ) & is.character( Stop2 ) ){

    Cond <- sprintf( "<font color=\"#FF0000\"> %s <br> %s </font>", Stop1, Stop2 )

    attr( Cond, "isTabOk" ) <- F

  }else if( is.character( Stop1 ) & !is.character( Stop2 ) ){

    Cond <- sprintf( "<font color=\"#FF0000\"> %s </font>", Stop1 )

    attr( Cond, "isTabOk" ) <- F

  }else if( !is.character( Stop1 ) & is.character( Stop2 ) ){

    Cond <- sprintf( "<font color=\"#FF0000\"> %s </font>", Stop2 )

    attr( Cond, "isTabOk" ) <- F

  }else{

    Stop3 <- NonZeroRowsColsShiny( dat )

    if( is.list( Stop3 ) ){

      Cond <- sprintf( "<font color=\"#FF0000\"> %s <br> %s <br> %s </font>", Stop3$Info, Stop3$Rows, Stop3$Cols )
      attr( Cond, "isTabOk" ) <- F

    }else{

      Cond <- "<font color=\"#008B00\"> The table is ready for the Grade Transformation. </font>"
      attr( Cond, "isTabOk" ) <- T

    }

  }

  return( Cond )

}

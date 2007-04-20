BRTEXT <- 1
BRCODE <- 2
BRCOMMENT <- 3
BRCATCODE <- 4
BRTEMPLATE <- 5

`brew` <-
function(file=stdin(),output=stdout(),text=NULL,envir=parent.frame(),run=TRUE){

	# Error check input
	closeIcon <- FALSE
	if (is.character(text) && nchar(text[1]) > 0){
		closeIcon <- TRUE
		icon <- textConnection(text[1])
	} else if (inherits(file,'connection') && summary(file)$"can read" == 'yes') {
		icon <- file
	} else if (is.character(file) && file.exists(file)){
		closeIcon <- TRUE
		icon <- file(file,open="rt")
	} else {
		stop('No valid input')
		return(invisible(NULL))
	}

	# Error check output
	if (inherits(output,'connection')){
		if (summary(output)$"can write" != 'yes')
			stop('output connection is not writeable')
	} else if ( !is.character(output) )
		stop('No valid output')

	# Error check env
	if (!is.environment(envir)){
		warning('envir is not a valid environment')
		return(invisible(NULL))
	}

	newline <- FALSE
	state <- BRTEXT
	buf <- code <- character()
	line <- ''
	
	while(TRUE){
		if (!nchar(line)){
			line <- readLines(icon,1)
		   	if (length(line) != 1) break
			line <- paste(line,"\n",sep='')
			newline <- TRUE
		} else newline <- FALSE
		if (state == BRTEXT){
			if (newline && regexpr("^%",line,perl=TRUE) > 0){
				code[length(code)+1] <- paste("cat(",deparse(paste(buf,collapse='')),")")
				spl <- strsplit(line,"^%",perl=TRUE)[[1]]
				code[length(code)+1] <- spl[2]
				line <- ''
				buf <- character()
				next
			}
			if (regexpr("<%=",line,perl=TRUE) > 0){
				state <- BRCATCODE
				delim <- "<%="
			} else if (regexpr("<%#",line,perl=TRUE) > 0){
				state <- BRCOMMENT
				delim <- "<%#"
			} else if (regexpr('<%%',line,perl=TRUE) > 0){
				# Template generator, strip a %
				spl <- strsplit(line,'<%%',fixed=TRUE)[[1]]
				buf[length(buf)+1] <- paste(spl[1],'<%',sep='')
				line <- paste(spl[-1],collapse='<%%')
				state <- BRTEMPLATE
				next
			} else if (regexpr("<%",line,perl=TRUE) > 0){
				state <- BRCODE
				delim <- "<%"
			}
			if (state != BRTEXT){ # something changed
				spl <- strsplit(line,delim,fixed=TRUE)[[1]]
				if (nchar(spl[1])) buf[length(buf)+1] <- spl[1]
				line <- paste(spl[-1],collapse=delim)

				if (length(buf)) code[length(code)+1] <- paste("cat(",deparse(paste(buf,collapse='')),")")
				buf  <- character()
			} else {
				buf[length(buf)+1] <- line
				line <- ''
			}
		} else {
			if (regexpr("%%>",line,perl=TRUE) > 0){
				spl <- strsplit(line,"%%>",fixed=TRUE)[[1]]
				buf[length(buf)+1] <- paste(spl[1],'%>',sep='')
				line <- paste(spl[-1],collapse='%%>')
				state <- BRTEXT
				next
			}
			if (regexpr("%>",line,perl=TRUE) > 0){
				spl <- strsplit(line,"%>",fixed=TRUE)[[1]]
				line <- paste(spl[-1],collapse='%>')

				n <- nchar(spl[1])
				# test  for '-' immediately preceding %> will strip trailing newline from line
				if (n > 0) {
					if (substr(spl[1],n,n) == '-') {
						line <- substr(line,1,nchar(line)-1)
						spl[1] <- substr(spl[1],1,n-1)
					}
					buf[length(buf)+1] <- spl[1]
				}
				if (state == BRCODE){
					code[length(code)+1] <- paste(buf,collapse='')
				} else if (state == BRCATCODE){
					code[length(code)+1] <- paste("cat(",paste(buf,collapse=''),")")
				}
				buf <- character()
				state <- BRTEXT
			} else {
				buf[length(buf)+1] <- line
				line <- ''
			}
		}
	}
	if (state == BRTEXT){
		if (length(buf)) {
			code[length(code)+1] <- paste("cat(",deparse(paste(buf,collapse='')),")")
			# cat(buf,sep='')
		}
	} else {
		warning("Unclosed tag")
	}

	if (closeIcon) close(icon)

	if (run){

		sunk <- FALSE
		if (!exists('.brew.output',where=envir) || !is.null(match.call()$output)) {
			sink(output)
			sunk <- ifelse(output!=stdout() && output!=stderr(), TRUE, FALSE)
			assign('.brew.output',output,pos=envir)
		}

		ret <- try(eval(parse(text=code),envir=envir))

		if (sunk) sink()
		invisible(ret)
	} else {
		invisible(parse(text=code))
	}
}

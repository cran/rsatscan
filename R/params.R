utils::globalVariables("ssenv")
#' @title Substitute new values into the input object
#' 
#' @description
#' Replaces existing values found in one object with new values
#' 
#' @param x A character vector of the form "name=value"
#' @param ssparams A character vector with arbitrary lines, 
#' currently imagined to be .ss.params
#' 
#' @details 
#' For each line of x, the function: 1) finds the "name" and the "value"
#' 2) checks to see whether the "name" exists in ssparams; if not, prints a warning
#' but if so, replaces the existing line of ssparams with that line of x.
#' 
#' Not expected to be used directly.
#' 
#' @return The modified ssparams.
subin = function (x,ssparams) {
  for (i in 1:length(x)) {
    inprm = substr(x[i],1,regexpr("=",x[i]))
    indef = substr(x[i],regexpr("=",x[i])+1, nchar(x[i]))
    if (length(which(substr(ssparams,1,regexpr("=",ssparams)) == inprm)) == 0)
       warning('Trouble! There is no parameter "', substr(inprm,1,regexpr("=",inprm)-1),
                                                          '"', call.=FALSE)
    else {ssparams[which(substr(ssparams,1,regexpr("=",ssparams)) == inprm)]=paste0(inprm,indef)}
  }
  return(ssparams)
}

# test whether this works appropriately when there is no = in a an input line
# test whether it works if "name = value", as well as "name=value".
# most likely I should re-do to extract the = from inorm and remove trailing blanks


#' @title Change list version of paramaters into char vector
#' 
#' @description
#' Turns a list of options into a charvar of options
#' 
#' @details 
#' The resulting charvar has values such as "name=value" where "name" was the named item
#' of the list.
#' 
#' @return
#' A character vector
#' 
#' Not expected to be used directly.
#'
#' @param x A list.
#' 
charlistopts = function (x) {
  paste0(names(x),"=",unlist(x))
}

#Huge ups to http://digitheadslabnotebook.blogspot.com/2011/06/environments-in-r.html
#which helped me get the scoping to play out correctly.
#ss.options will: 1) return the current values of .ss.params, if no invals
#                 2) Reset the values of .ss.params, if reset==TRUE
#                 3) change the values of the listed parameters, if a) invals = 
#                      c("param=value","param=value") or list(param="value")
#' @title Set or reset parameters to be used by SaTScan
#' 
#' @description Set or reset parameters to be used by SaTScan
#' 
#' @details \code{ss.options()} is intended to function like \code{par()} or 
#' \code{options()}.  There is a default set of parameter settings that resembles 
#' the one used by SaTScan, except that it produces all possible output files and
#' makes them as .dbf files instead of text.
#' 
#' @param invals A list with entries of the form name=value, where value should be 
#' in quotes unless it is a number. Alternatively, may be a character vector whose
#' entries are of the form "name=value".  The "name" in either case should be a 
#' valid SaTScan parameter name; unrecognized names will generate a warning and will 
#' do nothing.
#' @param reset If TRUE, will restore the default parameter values described in 
#' the "Details" section.
#' @param version A string of the form "#.#" or "#.#.#" specifying a version of 
#' SaTScan to take parameters from. If this parameter is NULL or not specified,
#' then parameters are reset based on the latest version of SaTScan.
#' @return If \code{invals == NULL}, returns the current parameter set, 
#' as altered by previous 
#' calls to \code{ss.options()} since the last call with \code{reset=TRUE}.  Otherwise 
#' returns modified parameter set invisibly.  The side effect, if \code{invals != NULL}, is to 
#' set the current values of the parameters per the value of \code{invals} 
#' and \code{reset}.
#' 
#' @export
#' 
#' @examples 
#' \dontrun{
#' head(ss.options(),3)
#' ss.options(list(CaseFile="NYCfever.cas"))
#' head(ss.options(),3)
#' 
#' # reset; shows whole parameter file without invisible()
#' invisible(ss.options(reset=TRUE))
#' head(ss.options(),3)
#' }
#' 
ss.options = function (invals=NULL, reset=FALSE, version=NULL) {

  inparms = ssenv$.ss.params
  if (reset == TRUE && is.null(version)) ssenv$.ss.params = ssenv$.ss.params.defaults
  else if (reset == TRUE) {
    
    if(!is.character(version)) version = as.character(version)
    
    version.regex = "^\\d+[.]\\d+([.]\\d+)?$"
    if(!grepl(version.regex, version)) stop("Invalid version of SaTScan - versions should be formatted as '#.#' or '#.#.#'")
    else {
      version.components = strsplit(version, ".", fixed=TRUE)[[1]]
      major = as.numeric(version.components[1])
      minor = as.numeric(version.components[2])
      
      if (major < 9 || (major == 9 && minor < 2)) {
        ssenv$.ss.params = ssenv$.ss.params.v9_2
        warning("The minimum defined parameters version of SaTScan is 9.2")
      }
      else if (major == 9 && minor == 2) ssenv$.ss.params = ssenv$.ss.params.v9_2
      else if (major == 9 && minor == 3) ssenv$.ss.params = ssenv$.ss.params.v9_3
      else if (major == 9 && minor == 4) ssenv$.ss.params = ssenv$.ss.params.v9_4
      else if (major == 9 && minor == 5) ssenv$.ss.params = ssenv$.ss.params.v9_5
      else if (major == 9 && minor == 6) ssenv$.ss.params = ssenv$.ss.params.v9_6
      else if (major == 9 && minor == 7) ssenv$.ss.params = ssenv$.ss.params.v9_7
      else if (major == 10 && minor == 0) ssenv$.ss.params = ssenv$.ss.params.v10_0
      else if (major == 10 && minor == 1) ssenv$.ss.params = ssenv$.ss.params.v10_1
      else {
        ssenv$.ss.params = ssenv$.ss.params.v10_1
        print("The specified parameters version is not known, defaulting to version 10.1")
      }
    }
  }
  if (is.null(invals)) {return(ssenv$.ss.params)}
  else {
    if (inherits(invals, "list")) invals = charlistopts(invals)
    ssenv$.ss.params =  subin(invals, inparms)
    invisible(ssenv$.ss.params)
  }
}
# review the help text for logic-- matches function??

#I need to think about how this will work when called by another function.

#  Do I need to re-think this?  There is 
#   a [Multiple Data Sets] line already...

#' @title Add lines to the current SaTScan parameter list
#' 
#' @description Allows you to add arbitrary lines to the current set
#' of SaTScan parameters
#' 
#' @details For certain SaTScan models or inputs (multiple data sets, 
#' Polygon),
#' SaTScan allows a variable number of parameters; these 
#' parameters are not used/allowed for other models or inputs.
#' This function allows the user to add 
#' arbitray lines to the current list of 
#' parameters.  In addition to the options mentioned, it could also be
#' used to add comments to the parameter file.
#' 
#' @param invals A character vector, which will be added to the end of the 
#' current paramter list.
#' 
#' @return Nothing.  
ss.options.extra = function(invals=NULL) {
  if (is.null(invals)) stop("This function doesn't do anything when there is no input")
  if (!inherits(invals, "character")) stop("Please input a character vector")
  else {
    ssenv$.ss.params =  c(ssenv$.ss.params, invals)
    invisible()
  }
}
# for help page: examples of [Polygon] and Multiple Data Sets






# Functions to write out the param file
# Probably a really bad idea to make matchout = FALSE-- only useful to write file
# from R but examine output manually
#' @title Write the SaTScan parameter file
#' 
#' @description Writes the current set of SaTScan parameters to a
#' specified location in the OS.
#' 
#' @details The current SaTScan options can be reset or modified
#' \code{ss.options()} and/or \code{ss.options.extra()}.  Once 
#' they are set as desired, they can be written to the OS 
#' using this function.
#' 
#' @param location A directory location, excluding the trailing "/".
#' @param filename The name of the file to be written to the OS;
#' The extension ".prm" will be appended.
#' @param matchout If false, the ResultsFile parameter will not
#' be touched; note that this will likely result in undesirable
#' performance from calls to \code{satcan()} using the parameter file.
#' If true, the ResultsFile is reset to share the filename given here.
#' 
#' @return Nothing. (Invisibly.)  Side effect is to write a file 
#' in the OS.
#' 
#' 
#' @examples 
#' \dontrun{
#' ## Would write the current ss.options() to c:/temp/NYCfever.prm
#' write.ss.prm("c:/tmp","NYCfever")
#' }
#' 
#' 
#' 
#' @export
#' @seealso \code{\link{ss.options}}, \code{\link{ss.options.extra}}
#' 
#
#  I should change this to detect and deal with the trailing /.
# change docs to cross-link.

write.ss.prm = function(location, filename, matchout = TRUE)  {
  if (matchout) ss.options(list(ResultsFile=paste0(filename,".txt")))
  fileconn<-file(paste0(location,"/",filename,".prm"))
  writeLines(ssenv$.ss.params, fileconn)
  close(fileconn)
  invisible()
}



#Testing
#ss.options(c("CaseFile=blue","ControlFile=red"))
#ss.options("CaseFile=orange")
#head(.ss.params)
#check = ss.options(reset=TRUE)
#head(check)







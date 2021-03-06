% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/MODIStsp_addindex.R
\name{MODIStsp_addindex}
\alias{MODIStsp_addindex}
\title{MODIStsp_addindex}
\usage{
MODIStsp_addindex(option_jsfile = NA, prodopts_file = NA, selprod = NA,
  selvers = NA, gui = TRUE, new_indexbandname = "",
  new_indexfullname = "", new_indexformula = "",
  new_indexnodata_out = "32767", MODIStsp_dir = NA)
}
\arguments{
\item{option_jsfile}{settings (optional): full path of the JSON file containing the
processing options in which the new indexes
are saved (default: MODIStsp_Previous.JSON in subfolder Previous).}

\item{prodopts_file}{settings (optional): full path of the RData file containing
products description (default: MODIStsp_ProdOpts.RData in subfolder Previous)}

\item{selprod}{settings (optional): Name of the product to which the new index should
be added. (Note: the index will be added to all other products allowing its
computation !)}

\item{selvers}{settings (optional): Version of the product to which the new index
should be added.}

\item{gui}{logical value (default: TRUE): if TRUE, the GUI is opened to define the new
index; otherwise use the "new_indexbandname", "new_indexfullname" and
"new_indexformula" parameters to define it non-interactively.}

\item{new_indexbandname}{(optional if gui=TRUE): short name (acronym) of the new
spectral index.}

\item{new_indexfullname}{(optional if gui=TRUE): extended name of the new spectral index.}

\item{new_indexformula}{(optional if gui=TRUE): string containing the formula of
the new spectral indexes. Variables accepted to compute it are the names of the bands:
b1_Red, b2_NIR, b3_Blue, b4_Green, b5_SWIR, b6_SWIR and b7_SWIR.}

\item{new_indexnodata_out}{(optional): NoData value to assign to the rasters
containing the new index}

\item{MODIStsp_dir}{(optional): main folder containing MODIStsp R files (used only to
launch MODSItsp from outside the package using MODIStsp_std.R)}
}
\value{
NULL - the MODIStsp_Previous.RData file is modified so to allow computation of
the additional index
}
\description{
Function used to add a user-defined Spectral Index to the default list of
computable spectral indexes. Execution without the GUI (i.e., to add a new index from
a script) is also possible (see examples)
}
\details{
\itemize{
\item The function asks the user to provide the info related to the new desired
Spectral Index using a GUI interface, checks for correctness of provided information
(e.g., correct bandnames, computable formula, etc...). If the index is legit, it
modifies the MODIStsp_Previous.RData file so to allow computation of the additional
index within MODIStsp./n
\item To remove all custom-added spectral indexes, simply delete the MODIStsp_Previous.RData
file within the /Previous subfolder of the folder in which the package was installed,
or the alternative JSON specified by the parameter "option_jsfile".
\item The function can be run either from within the main MODIStsp GUI, or a standalone
script. In the latter case, it modifies either the MODIStsp_Previous.RData options
file, or the options_file specified by the user, to add the new index.
}
}
\note{
License: GPL 3.0
}
\examples{
# Run the GUI to interactively define the function
 \dontrun{
 MODIStsp_addindex()}

# Run the GUI and save the new index in a custom RData file
\dontrun{
MODIStsp_addindex(option_jsfile = "X:/yourpath/youroptions.json")}

# Define the new index non-interactively
\dontrun{
MODIStsp_addindex(gui = FALSE, new_indexbandname = "SSD",
  new_indexfullname = "Simple Stupid Difference",
  new_indexformula = "b2_NIR-b1_Red")}


}
\author{
Lorenzo Busetto, phD (2014-2015) \email{busetto.l@irea.cnr.it}

Luigi Ranghetti, phD (2015) \email{ranghetti.l@irea.cnr.it}
}

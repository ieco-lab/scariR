% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/create_MaxEnt_suitability_maps_CV.R
\name{create_MaxEnt_suitability_maps_CV}
\alias{create_MaxEnt_suitability_maps_CV}
\title{Predicts maps of establishment suitability based on MaxEnt model ('SDMmodelCV' object)}
\usage{
create_MaxEnt_suitability_maps_CV(
  model.obj,
  model.name,
  mypath,
  create.dir = FALSE,
  env.covar.obj,
  describe.proj = NA,
  predict.fun = "mean",
  predict.type = "cloglog",
  clamp.pred = TRUE,
  thresh = NA,
  map.thresh = FALSE,
  map.thresh.extra = NA,
  map.style = NA,
  summary.file = NA
)
}
\arguments{
\item{model.obj}{A model object created by the package 'SDMtune', should be of
class 'SDMmodelCV'.}

\item{model.name}{Character. A string matching the name of the object set for
\code{model.obj}. Exclude unnecessary phrases, such as the "_model" ending.}

\item{mypath}{Character.A file path to the sub directory where the model
output will be stored. Should be used with the \code{\link[=file.path]{file.path()}} function
(i.e. with '/' instead of '\\'). If this sub directory does not already exist
and should be created by the function, set \code{create.dir} = TRUE. This will
create a folder from the last part of the filepath in \code{mypath}.}

\item{create.dir}{Logical. Should the last element of \code{mypath} create a sub
directory for the model output? If TRUE, the main folder will be created for
the model output. If FALSE (ie, the sub directory already exists), only the
"plots" folder within the model output sub directory will be created.}

\item{env.covar.obj}{A stack of rasters of environmental covariates. These
covariates may be the same covariates used to train the model, or they may be
temporally or spatially projected (ex, for climate change). Suitability
predictions will be made based on these rasters. See details for additional
formatting information.}

\item{describe.proj}{Character. Is the environmental covariate raster stack in
\code{env.covar.obj} projected to a different geographical space, time period, etc
than the model was trained on? If yes, enter the name of the projected period
or region. This ensures that plot titles and file names reflect the projected
region or time period. If using multiple words, separate with an underscore.}

\item{predict.fun}{Character. The default is "mean". This is the function to
be applied to combine the iterations of the model when predicting a raster
output. Can be one of: \code{min}, \code{mean}, \code{median}, \code{max}, or \code{sd}
(standard deviation). If multiple are desired, must be in the concatenated
form: \code{c("mean", "sd")}. Should be all lowercase.}

\item{predict.type}{Character. Default is "cloglog". The type of raster output
to be created from the trained model. Can be either \code{cloglog} or
\code{c("cloglog", "logistic")}. Cannot only be \code{logistic}.}

\item{clamp.pred}{Logical. Default is TRUE. Should clamping be performed?}

\item{thresh}{Numeric or Character. Does not need to be defined if
\code{map.thresh = FALSE} This may be imported manually (numeric), or may be
selected from one of the thresholds for the model (character). If a preset,
the specified mean threshold value for all iterations of the model is chosen.
See details for a list of preset options and other usages.}

\item{map.thresh}{Logical, TRUE by default. This function determines if a
thresholded suitability map will be created. If not, output will only consist
of suitability maps of the type specified in \code{predict.fun}. \strong{Note} threshold
maps can only be created for the cloglog output from the model. If multiple
values for \code{predict.type} are specified, the function will still only produce
a threshold map based on the cloglog output}

\item{map.thresh.extra}{Character, default is NA. This argument will plot an
extra threshold underneath the main threshold layer. It must be one of the
thresholds specified in \code{thresh} and must be less than or equal to the value
of the other values of thresh. For example, I use this to plot the MTP
threshold under the other thresholds being plotted (because MTP is usually the
smallest thresh value). However, plotting the 10_percentile thresh under the
MTP threshold would not function properly and the map would not render.}

\item{map.style}{List, default is NA. This is used to apply
ggplot aesthetics to the plot outputs. If specified, the given value should be
a list of ggplot aesthetic options. If not, the built-in default list will be
used (see details). See examples for usage.}

\item{summary.file}{Data import. Does not need to be defined if
\code{map.thresh = FALSE}. Should be a .csv file or data frame that contains the
summary statistics output created by
\code{\link[=compute_MaxEnt_summary_statistics_CV]{compute_MaxEnt_summary_statistics_CV()}} (filename ending in
"summary_all_iterations.csv"). If an import, file path should be in the format
produced by the \code{\link[=file.path]{file.path()}} function (i.e. with '/' instead of '\\').}
}
\value{
A raster of suitability values projected to the same spatial extent as the
input \code{env.covar.obj} and a corresponding .jpg figure are created. If multiple
values are given for \code{predict.fun}, then one raster and jpg image
will be created for each value. If \code{map.thresh = TRUE}, then the output will
also include a binary raster of suitability and a .jpg image of unsuitable
areas layered on top of suitability raster. This threshold of suitability is
determined by the value of \code{thresh}.

Use caution: will overwrite previous files by default.
}
\description{
This function will create a map of establishment suitability based on a MaxEnt
model trained using the \code{SDMtune} R package. It will optionally create
thresholded versions of these maps using thresholds given by the MaxEnt
algorithm.
}
\details{
The function requires the packages 'cli', 'ggnewscale', 'tidyverse', 'devtools', 'SDMtune', and 'terra'

The thresholding feature of this function will limit what is considered
suitable using one of a list of commonly applied thresholds. These threshold
values are determined by the MaxEnt algorithm. (ie, a non-thresholded map on
the cloglog scale has a lower color scale limit of 0, while a thresholded map
has a lower limit determined by the algorithm.)

\strong{NOTE} This function will create a thresholded suitability map for a raster
output using the SD function, but this map would not be meaningful because
it does not illustrate cloglog suitability (while thresholds are created
using the cloglog suitability metrics).
\subsection{thresh:}{

This can be a single numeric or preset character value. It may also be a
concatenated set of numerics or presets, as in  \code{c(0.2, 0.3)} or
\code{c("MTSS", "BTO")}. Note that this function only uses the cloglog version of
these thresholds.

Thresh presets list:
\itemize{
\item \code{BTO} = Balance training omission predicted area and threshold value
\item \code{EE} = Equate entropy of thresholded and original distributions
\item \code{ETSS} = Equal training sensitivity and specificity
\item \code{MTP} = Minimum Training Presence
\item \code{MTSS} = Maximum training sensitivity plus specificity
\item \code{ten_percentile} or \verb{10_percentile} = Ten percentile training presence
}
}

\subsection{env.covar.obj:}{

This must a \code{SpatRaster} raster stack created using \code{\link[terra:rast]{terra::rast()}}. The stack
should include the same covariates (as raster layers) that you used to train
the model and the names of the variables in these layers must be the same.
You can check for naming consistency using \code{\link[=names]{names()}}.
}

\subsection{map.style:}{

Map style default options:
\itemize{
\item \code{xlab("UTM Easting")}
\item \code{ylab("UTM Northing")}
\item \code{labs(fill = "Suitability for SLF")}
\item \code{theme_classic()}
\item \code{theme(legend_position = "bottom")}
\item \code{theme(panel.background = element_rect(fill = "lightblue2", color = "lightblue2"), legend.title = element_text(face = "bold"))}
\item \code{scale_x_continuous(expand = c(0, 0))}
\item \code{scale_y_continuous(expand = c(0, 0))}
\item \code{coord_equal()}
\item \code{scale_fill_viridis_c(option = "D", limits = c(0, 1.00), breaks = c(0, 0.25, 0.5, 0.75, 1.00), guide = guide_colorbar(frame.colour = "black", ticks.colour = "black", barwidth = 20))}
}
}
}
\examples{

# ARGUMENT USAGE:

```R
mypath <- file.path(here::here() \%>\%
                    dirname(),
                     "maxent/models/slf_global_v3")

map_style <- list(
xlab("UTM Easting"),
ylab("UTM Northing"),
theme_classic()
)

# EXAMPLE USAGE:
scari::create_MaxEnt_suitability_maps_CV(
model.obj = global_model,
model.name = "global",
mypath = mypath,
create.dir = FALSE,
env.covar.obj = x_global_126_env_covariates,
describe.proj = "globe_2041-2070_GFDL_ssp126", # name of area or time period projected to
clamp.pred = TRUE,
thresh = c("MTP", "MTSS"),
map.thresh = TRUE, # whether thresholded versions of these maps should be created
map.thresh.extra = "MTP",
summary.file = file.path(mypath, "global_summary_all_iterations.csv")
)

```

}

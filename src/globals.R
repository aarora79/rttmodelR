library(dplyr)
library(ggplot2)
library(tidyverse)
library(tidyr)
library(stringi)
library(lubridate)
library(futile.logger)
library(psych)
library(tibbletime)
library(plotly)
library(viridis)
library(scales)
library(tidyquant)
library(VGAM)
library(MCMCpack)
library(mcmcplots)
library(knitr)
library(kableExtra)
library(gridExtra)
library(grid)
library(png)

# high watermark for congestion i.e. any value more than this means serious congestion
RTT_CONG_HIGH_WATERMARK = 5000 

# high watermark for detecting erroneous values in the data, have seen extremely large
# values reported by ping so those need to be removed
RTT_ERROR_THRESHOLD = 100000

LAG_THRESHOLD = 60
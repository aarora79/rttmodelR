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

RTT_HIGH_WATERMARK = 5000
LAG_THRESHOLD = 60
library(zoo)
library(ggplot2)
library(dplyr)

readTracking <- function(filename) {
  x <- read.csv(filename)
  x$Length <- x$Y1 - x$Y0
  x
}

# Find splits as zones with high min/max differences
# Width should be the approximate width (in number of frames) of the pause between samples
findSections <- function(x, width, threshold=NULL) {
  xz <- as.zoo(x)
  n <- length(xz)
  # make sure width is odd
  width <- if(width %% 2 == 1) width else width+1
  mid <- width %/% 2
  delta <- rollapply(xz, width, max, partial=TRUE) - 
    rollapply(xz, width, min, partial=TRUE)
  locMin <- rollapply(delta, width, function(x) which.min(x) == mid, fill=c(FALSE,NA,FALSE))
  mins <- delta[locMin]
  threshold <- if(is.null(threshold)) mean(mins)/3 else threshold
  index(mins[mins < threshold])
}

sectionIds <- function(x, sections.limits) {
  n <- length(sections.limits)+1
  sections.limits <- c(0, sections.limits, nrow(x))
  genIds <- function (i) {
    rep(i, sections.limits[i+1]-sections.limits[i])
  }
  unlist(lapply(1:n, genIds))
}

medianFilter <- function(x, width) {
  rollapply(x, width, median, partial=TRUE)
}
reccursiveMedianFilter <- function(x, width) {
  Reduce(medianFilter, seq(3,width,2), as.zoo(x))
}

findOscillations <- function(x, noise_width=3) {
  idx <- c(1:length(x))
  df <- data.frame(x=idx, y=x)
  mod <- loess(y~x, df, span=0.8, degree=1)
  est <- predict(mod)
  shifted <- x - est
  filtered <- if(noise_width < 3) shifted else reccursiveMedianFilter(shifted, noise_width)
  filtered > 0
}

groupOscillations <- function(osc) {
  n <- length(osc)
  transitions <- as.numeric(osc[2:n] != osc[1:n-1])
  as.factor(Reduce(function(x,y) x+y, transitions, 1, accumulate=TRUE))
}

findSectionExtrema <- function(data) {
  maxs <- data %>% filter(upper) %>%
    group_by(oscillations, add=TRUE) %>%
    summarise(Time=Time[which.max(Length)], Length=max(Length))
  mins <- data %>% filter(!upper) %>%
    group_by(oscillations, add=TRUE) %>%
    summarise(Time=Time[which.min(Length)], Length=min(Length))
  mins$kind <- factor(rep("min", nrow(mins)), c("min", "max"))
  maxs$kind <- factor(rep("max", nrow(maxs)), c("min", "max"))
  merge(mins, maxs, all=TRUE)
}

findLengthExtrema <- function(data, pauseLength=NULL, noiseWidth=5) {
  if(!is.null(pauseLength)) {
    sections.limits <- findSections(data$Length, pauseLength)
    data$sections <- as.factor(sectionIds(data, sections.limits))
  } else
    data$sections <- as.factor(rep(1, nrow(data)))
  data %>% group_by(sections) %>%
    mutate(upper=findOscillations(Length, noiseWidth)) %>%
    mutate(oscillations=groupOscillations(upper)) %>%
    findSectionExtrema()
}

plotCurve <- function(x) {
  ggplot(data=x) + geom_line(aes(x=Time, y=Length))
}

plotExtrema <- function(x, extrema) {
  extrema$section_kind <- interaction(list(extrema$sections, extrema$kind))
  ggplot() +
    geom_line(data=x, aes(x=Time, y=Length)) + 
    geom_point(data=extrema, aes(x=Time, y=Length, colour=section_kind))
}

sectionStrain <- function(section) {
  
}

strainLoading <- function(extrema) {
  extrema %>%
    group_by(sections) %>%
    summarise()
}

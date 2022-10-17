
library(readxl)
library(dplyr)
library(grid)
library(lattice)
library(survival)



# EXPERIMENTAL INFECTIONS ----

# Meta-analysis data cleaning ----

### Read raw data.
#outputs data.set
prepare.data.MA <- function(datainput){

  dd.raw <- as_tibble(datainput)

  data.set <- dd.raw %>%
    select(refID, groupID,
           uniqueID, sampUnitSize,
           minIncub, maxIncub, minDetect, maxDetect, durationPI,
           agent, targetSpecies,
           labTestUsed, targetLabTest, matrix, agentSubtype, ageCat, route
    ) %>%
    mutate(
      agent = as.factor(agent),
      targetSpecies = as.factor(targetSpecies),
      targetLabTest = as.factor(targetLabTest),
      matrix = as.factor(matrix),
      agentSubtype = as.factor(agentSubtype),
      ageCat = as.factor(ageCat),
      route = as.factor(route)
    )

  ### If durationPI is missing, we don't censor.
  ### Handle this by replacing NA with a large number.
  ix <- which(is.na(data.set$durationPI))
  data.set$durationPI[ix] <- 1e6 # Large enough!

  ### Replace durationPI by two variables:
  ### duration.incub:  For incubation, study duration is the max of durationPI.
  ### duration.detect: For detection, it is simply durationPI (per row).
  data.set <- data.set %>%
    group_by(uniqueID) %>%
    mutate(
      duration.incub = max.na(durationPI),
      duration.detect = durationPI) %>%
    ungroup

  ### If minXXX = duration and maxXXX = NA (XXX = Incub or Detect), then we
  ### set maxXXX = duration as well.
  # Incub.
  ix <- which((data.set$minIncub == data.set$duration.incub) &
                is.na(data.set$maxIncub))
  data.set$maxIncub[ix] <- data.set$duration.incub[ix]
  # Detect.
  ix <- which((data.set$minDetect == data.set$duration.detect) &
                is.na(data.set$maxDetect))
  data.set$maxDetect[ix] <- data.set$duration.detect[ix]


  # Remove entries with missing sampUnitSize.
  data.set <- data.set %>%
    filter(!is.na(sampUnitSize))


  ### Sanity checks.
  # Species is always constant within uniqueID.
  a <- data.set %>%
    group_by(uniqueID) %>%
    summarise(ok = all.same(targetSpecies)) %>%
    ungroup
  stopifnot(all(a$ok))
  # Incubation/detection times <= study period (for the proper variable!).
  stopifnot(all(data.set$minIncub <= data.set$duration.incub, na.rm=T))
  stopifnot(all(data.set$maxIncub <= data.set$duration.incub, na.rm=T))
  stopifnot(all(data.set$minDetect <= data.set$duration.detect, na.rm=T))
  stopifnot(all(data.set$maxDetect <= data.set$duration.detect, na.rm=T))

  return(data.set)
}



## INCUBATION----

#outputs dd.plot
prepare.data.INCUB <- function(datainput=data.set, agent){

  dd <- data.set %>%
    filter(agent == agent) %>%
    filter(!is.na(minIncub) | !is.na(maxIncub)) %>%
    mutate(
      cns.min = (minIncub == duration.incub), # minIncub censored?
      cns.max = (maxIncub == duration.incub)  # maxIncub censored?
    )


  dd.incub <- dd %>%
    group_by(uniqueID) %>%
    # Summarise.
    # For min[max]Incub, we pick the row with the smallest [largest] value.
    # Censoring information taken from the corresponding row.
    summarise(
      ix.min = which.min.na(minIncub),
      ix.max = which.max.na(maxIncub),
      species = head(targetSpecies, 1), # All equal.
      n.animals = max(sampUnitSize), # Actually corresponds to ix.min/ix.max.
      duration = head(duration.incub, 1), # All equal (def of duration.incub)!
      min.incub = minIncub[ix.min],
      max.incub = maxIncub[ix.max],
      cns.min = cns.min[ix.min],
      cns.max = cns.max[ix.max]
    ) %>%
    select(-ix.min, -ix.max) %>%
    ungroup


  # Plot the data. One observation is a group of animals.
  # Create a long data set with a single variable 'incubation' together
  # with a min.max variable.
  dd.plot.min <- dd.incub %>%
    select(-max.incub, -cns.max) %>%
    rename(incub = min.incub, cns = cns.min) %>%
    mutate(min.max = "START Clinical Signs")
  dd.plot.max <- dd.incub %>%
    select(-min.incub, -cns.min) %>%
    rename(incub = max.incub, cns = cns.max) %>%
    mutate(min.max="END Clinical Signs")
  dd.plot <- rbind(dd.plot.min, dd.plot.max) %>%
    mutate(min.max=relevel(factor(min.max), ref="START Clinical Signs")) %>%
    mutate(species = factor(species))

  # Remove species with too few groups.
  dd.plot <- dd.plot %>%
    group_by(species) %>%
    mutate(n.groups = n()/2) %>% # 2 rows (min and max) for each group.
    ungroup %>%
    filter(n.groups >= min.groups) %>% # Require 4 groups.
    mutate(species = factor(species)) # Remove unused factor levels.

  return(dd.plot)
}


### Produce Kaplan-Meier INCUB ----

km.incub.ci <- function(){
  grid <- c(1,2)
  if(length(levels(dd.plot$species))>2)(grid <- c(2,2))

  xyplot(!cns ~ incub | species, groups=min.max, data=dd.plot,
         panel=panel.superpose,
         # The main job is done by this panel function. It computes the KM curve
         # as well as median/CI/IQR, and plots these quantities.
         panel.groups=function(x, y, col.line, group.value,...) {

           # For book-keeping.
           cur.species <- levels(dd.plot$species)[panel.number()]
           # Results of this analysis will be stored in this list, and then written
           # back to the global variable 'result.list'.
           cur.res <- list()

           # y values of polygons.
           pol.y <- c(0, 0, 1, 1)
           ## Kaplan-Meier plot.
           so <- Surv(x, y) # x = Time (incub), y = event indicator (!cns).
           sf <- survfit(so ~ 1) # Fit Kaplan-Meier.
           # Make the curve increase from (0, 0).
           xx <- c(0, sf$time)
           yy <- c(0, 1 - sf$surv)
           # Draw Kaplan-Meier curve.
           panel.xyplot(xx, yy, col.line=col.line, ...)
           # Add text on number of groups.
           grid.text(sprintf("%d groups", length(x)),
                     x = unit(0.9, "npc"), y = unit(0.1, "npc"))
           cur.res$n.rows <- length(x)


           # Plot median survival time with CI from survfit.
           qsf <- quantile(sf, probs=0.5)
           m <- qsf$quantile # Median.
           lcl <- qsf$lower # Lower confidence limit for median.
           ucl <- qsf$upper # Upper.
           panel.abline(v=m, col=col.line, lwd=2, lty="dashed")
           # Left part of CI.
           pol.x <- c(lcl, m, m, lcl)
           panel.polygon(x=pol.x, y=pol.y, col=col.line, alpha=1/8)
           # Right part of CI.
           pol.x <- c(ucl, m, m, ucl)
           panel.polygon(x=pol.x, y=pol.y, col=col.line, alpha=1/8)
           cur.res$median <- m
           cur.res$lcl <- lcl
           cur.res$ucl <- ucl
           result.list.incub.ci[[cur.species]][[group.value]] <<- cur.res

         },
         main=paste0(as.character(agent)," - Clinical signs period, with CI"), # Main title.
         type="s", # Step function.
         lwd=2, # Thick line.
         layout= grid,#c(2, 2), # 2 x 2 grid of panels.
         as.table=T, # Correct panel order.
         xlim=c(-3, 53), # Limit of x axis.
         xlab="Days", ylab="Groups showing clinical signs",
         auto.key=list(points=F, lines=T, columns=2) # Simple legend.
  )
}


km.incub.iq <- function(){
  grid <- c(1,2)
  if(length(levels(dd.plot$species))>2)(grid <- c(2,2))

  xyplot(!cns ~ incub | species, groups=min.max, data=dd.plot,
         panel=panel.superpose,
         # The main job is done by this panel function. It computes the KM curve
         # as well as median/CI/IQR, and plots these quantities.
         panel.groups=function(x, y, col.line, group.value,...) {

           # For book-keeping.
           cur.species <- levels(dd.plot$species)[panel.number()]
           # Results of this analysis will be stored in this list, and then written
           # back to the global variable 'result.list'.
           cur.res <- list()

           # y values of polygons.
           pol.y <- c(0, 0, 1, 1)
           ## Kaplan-Meier plot.
           so <- Surv(x, y) # x = Time (incub), y = event indicator (!cns).
           sf <- survfit(so ~ 1) # Fit Kaplan-Meier.
           # Make the curve increase from (0, 0).
           xx <- c(0, sf$time)
           yy <- c(0, 1 - sf$surv)
           # Draw Kaplan-Meier curve.
           panel.xyplot(xx, yy, col.line=col.line, ...)
           # Add text on number of groups.
           grid.text(sprintf("%d groups", length(x)),
                     x = unit(0.9, "npc"), y = unit(0.1, "npc"))
           cur.res$n.rows <- length(x)


           # Plot median survival time with IQ from survfit.
           qsf <- quantile(sf, probs=c(0.25, 0.5, 0.75))
           m <- qsf$quantile # [Q1, median, Q3].
           pol.x <- c(m[1], m[3], m[3], m[1])
           panel.abline(v=m[2], col=col.line, lwd=2, lty="dashed")
           panel.polygon(x=pol.x, y=pol.y, col=col.line, alpha=1/8)

           cur.res$median <- m[2]
           cur.res$iq1 <-  m[1]
           cur.res$iq3 <-  m[3]

           #browser()
           result.list.incub.iq[[cur.species]][[group.value]] <<- cur.res

         },
         main=paste0(as.character(agent)," - Clinical signs period, with IQ"), # Main title.
         type="s", # Step function.
         lwd=2, # Thick line.
         layout= grid,#c(2, 2), # 2 x 2 grid of panels.
         as.table=T, # Correct panel order.
         xlim=c(-3, 53), # Limit of x axis.
         xlab="Days", ylab="Groups showing clinical signs",
         auto.key=list(points=F, lines=T, columns=2) # Simple legend.
  )
}



# tables ----

produce.tables.INCUB <- function(
  list1 = result.list.incub.ci,
  list2 = result.list.incub.iq
){

  species <- names(list1)
  parameter <- names(list1[[1]])
  nrows <- length(species)*2
  fill <- rep(NA,nrows)

  output.incub <- data.frame(species=fill,
                             parameter=fill,
                             n.groups=fill,
                             median=fill,
                             LCL=fill,
                             UCL=fill,
                             IQ1=fill,
                             IQ3=fill)

  r = 1
  for( s in 1:length(species)){
    for (p in 1:length(list1[[s]])){
      output.incub$species[r] <- species[s]
      output.incub$parameter[r] <- parameter[p]
      output.incub$n.groups[r] <- list1[[s]][[p]][[1]]
      output.incub$median[r] <- list1[[s]][[p]][[2]][[1]]
      output.incub$LCL[r] <- list1[[s]][[p]][[3]][[1]]
      output.incub$UCL[r] <- list1[[s]][[p]][[4]][[1]]
      output.incub$IQ1[r] <- list2[[s]][[p]][[3]][[1]]
      output.incub$IQ3[r] <- list2[[s]][[p]][[4]][[1]]
      r=r+1
    }
  }

  output.incub <- output.incub[!apply(output.incub, 1, function(x) all(is.na(x))),]


  return(output.incub)

}




# DETECTION ----

#outputs dd.plot
prepare.data.DETECT <- function(datainput=data.set){

  dd <- data.set %>%
    filter(agent == agent) %>%
    filter(targetLabTest == targetLabTest.selection) %>%
    filter(!is.na(minDetect) | !is.na(maxDetect)) %>%
    filter(matrix != "NotReported") %>%
    mutate(
      cns.min = (minDetect == duration.detect), # minDetect censored?
      cns.max = (maxDetect == duration.detect)  # maxDetect censored?
    )


  dd.detect <- dd %>%
    select(species = targetSpecies,
           matrix,
           min.detect = minDetect,
           max.detect = maxDetect,
           cns.min, cns.max)



  # Plot the data. One observation is a row.
  # Create a long data set with a single variable 'detection' together
  # with a min.max variable.
  dd.plot.min <- dd.detect %>%
    select(-max.detect, -cns.max) %>%
    rename(detect = min.detect, cns = cns.min) %>%
    mutate(min.max = "Min_detection")
  dd.plot.max <- dd.detect %>%
    select(-min.detect, -cns.min) %>%
    rename(detect = max.detect, cns = cns.max) %>%
    mutate(min.max="Max_detection")
  dd.plot <- rbind(dd.plot.min, dd.plot.max)

  if(dim(dd.plot)[1]>0){
    dd.plot <- dd.plot%>%
    mutate(min.max=relevel(factor(min.max), ref="Min_detection")) %>%
    mutate(species = factor(species))
  }

  # Remove species/matrix combinations with too few rows (in original data).
  dd.plot <- dd.plot %>%
    group_by(species, matrix) %>%
    mutate(n.rows = n()/2) %>% # 2 rows (min and max) for each group.
    ungroup %>%
    filter(n.rows >= min.groups) %>% # Require 4 groups.
    mutate(species = factor(species), # Remove unused factor levels.
           matrix = factor(matrix))

  # Shorten a few long matrix names.
  levels(dd.plot$matrix) <- gsub("FoetusStillbirthNeonatus", "Foetus",
                                 levels(dd.plot$matrix))
  levels(dd.plot$matrix) <- gsub("Limphonode", "Limph",
                                 levels(dd.plot$matrix))
  return(dd.plot)
}


### Produce Kaplan-Meier. ----


km.detect.ci <- function(){

  grid <- c(1,2)
  if(length(levels(dd.plot$species))>2)(grid <- c(2,2))

  xyplot(!cns ~ detect | species + matrix, groups=min.max, data=dd.plot,
         panel=panel.superpose,
         # The main job is done by this panel function. It computes the KM curve
         # as well as median/CI/IQR, and plots these quantities.
         panel.groups=function(x, y, col.line, group.value, ...) {
           # For book-keeping.
           cur.species <- levels(dd.plot$species)[current.column()]
           cur.matrix <- levels(dd.plot$matrix)[current.row()]
           # Results of this analysis will be stored in this list, and then written
           # back to the global variable 'result.list'.
           cur.res <- list()

           # y values of polygons.
           pol.y <- c(0, 0, 1, 1)
           ## Kaplan-Meier plot.
           so <- Surv(x, y) # x = Time (incub), y = event indicator (!cns).
           sf <- survfit(so ~ 1) # Fit Kaplan-Meier.
           # Make the curve increase from (0, 0).
           xx <- c(0, sf$time)
           yy <- c(0, 1 - sf$surv)
           # Draw Kaplan-Meier curve.
           panel.xyplot(xx, yy, col.line=col.line, ...)
           # Add text on number of groups.
           grid.text(sprintf("%d groups", length(x)),
                     x = unit(0.9, "npc"), y = unit(0.15, "npc"),
                     gp = gpar(cex = 0.7))
           cur.res$n.rows <- length(x)

           # Plot median survival time with CI from survfit.
           qsf <- quantile(sf, probs=0.5)
           m <- qsf$quantile # Median.
           lcl <- qsf$lower # Lower confidence limit for median.
           ucl <- qsf$upper # Upper.
           panel.abline(v=m, col=col.line, lwd=2, lty="dashed")
           # Left part of CI.
           pol.x <- c(lcl, m, m, lcl)
           panel.polygon(x=pol.x, y=pol.y, col=col.line, alpha=1/8)
           # Right part of CI.
           pol.x <- c(ucl, m, m, ucl)
           panel.polygon(x=pol.x, y=pol.y, col=col.line, alpha=1/8)
           cur.res$median <- m
           cur.res$lcl <- lcl
           cur.res$ucl <- ucl


           result.list.detect.ci[[cur.species]][[cur.matrix]][[group.value]] <<- cur.res
         },
         main=paste0(as.character(agent),", Target of lab test: ", as.character(targetLabTest.selection), "(MEAN+CI)"), # Main title.
         type="s", # Step function.
         lwd=2, # Thick line.
         layout= grid,#c(2, 2), # 2 x 2 grid of panels.
         as.table=T, # Correct panel order.
         xlim=c(-3, 53), # Limit of x axis.
         xlab="Days", ylab="Detection, Median and CI",
         par.strip.text=list(cex=0.75),
         auto.key=list(points=F, lines=T, columns=2) # Simple legend.
  ) %>% useOuterStrips
}

km.detect.iq <- function(){

  grid <- c(1,2)
  if(length(levels(dd.plot$species))>2)(grid <- c(2,2))

  xyplot(!cns ~ detect | species + matrix, groups=min.max, data=dd.plot,
         panel=panel.superpose,
         # The main job is done by this panel function. It computes the KM curve
         # as well as median/CI/IQR, and plots these quantities.
         panel.groups=function(x, y, col.line, group.value, ...) {
           # For book-keeping.
           cur.species <- levels(dd.plot$species)[current.column()]
           cur.matrix <- levels(dd.plot$matrix)[current.row()]
           # Results of this analysis will be stored in this list, and then written
           # back to the global variable 'result.list'.
           cur.res <- list()

           # y values of polygons.
           pol.y <- c(0, 0, 1, 1)
           ## Kaplan-Meier plot.
           so <- Surv(x, y) # x = Time (incub), y = event indicator (!cns).
           sf <- survfit(so ~ 1) # Fit Kaplan-Meier.
           # Make the curve increase from (0, 0).
           xx <- c(0, sf$time)
           yy <- c(0, 1 - sf$surv)
           # Draw Kaplan-Meier curve.
           panel.xyplot(xx, yy, col.line=col.line, ...)
           # Add text on number of groups.
           grid.text(sprintf("%d groups", length(x)),
                     x = unit(0.9, "npc"), y = unit(0.15, "npc"),
                     gp = gpar(cex = 0.7))
           cur.res$n.rows <- length(x)

           # Plot median survival time with IQ from survfit.
           qsf <- quantile(sf, probs=c(0.25, 0.5, 0.75))
           m <- qsf$quantile # [Q1, median, Q3].
           pol.x <- c(m[1], m[3], m[3], m[1])
           panel.abline(v=m[2], col=col.line, lwd=2, lty="dashed")
           panel.polygon(x=pol.x, y=pol.y, col=col.line, alpha=1/8)
           cur.res$median <- m[2]
           cur.res$q1 <- m[1]
           cur.res$q3 <- m[3]

           result.list.detect.iq[[cur.species]][[cur.matrix]][[group.value]] <<- cur.res
         },
         main=paste0(as.character(agent),", Target of lab test: ", as.character(targetLabTest.selection), "(MEDIAN+IQ)"), # Main title.
         type="s", # Step function.
         lwd=2, # Thick line.
         layout= grid,#c(2, 2), # 2 x 2 grid of panels.
         as.table=T, # Correct panel order.
         xlim=c(-3, 53), # Limit of x axis.
         xlab="Days", ylab="Detection, Median and IQ",
         par.strip.text=list(cex=0.75),
         auto.key=list(points=F, lines=T, columns=2) # Simple legend.
  ) %>% useOuterStrips
}


# tables ----

produce.tables.DETECT <- function(
  list1 = result.list.detect.ci,
  list2 = result.list.detect.iq
){

  output.detect <- data.frame(species=NA,
                              matrix=NA,
                              parameter=NA,
                              n.groups=NA,
                              median=NA,
                              LCL=NA,
                              UCL=NA,
                              IQ1=NA,
                              IQ3=NA)

  for (s in 1:length(list1)){
    for (m in 1:length(list1[[s]])){
      for (p in 1:2){

        row.vector <- vector()

        row.vector[1] <- names(list1)[s]
        row.vector[2] <- names(list1[[s]])[m]
        row.vector[3] <- names(list1[[s]][[m]])[p]
        row.vector[4] <- list1[[s]][[m]][[p]][[1]]
        row.vector[5] <- list1[[s]][[m]][[p]][[2]]
        row.vector[6] <- list1[[s]][[m]][[p]][[3]]
        row.vector[7] <- list1[[s]][[m]][[p]][[4]]
        row.vector[8] <- list2[[s]][[m]][[p]][[3]]
        row.vector[9] <- list2[[s]][[m]][[p]][[4]]

        output.detect <- rbind(output.detect,row.vector)
      }
    }
  }
  output.detect <- output.detect[-1,]
  return(output.detect)
}


# PATHOGEN SURVIVAL -----


make.KM.curves <- function(data.set, agent.name, CI=TRUE, table=TRUE)
{
  data.plot <- data.set %>% filter(agent==agent.name) %>%
    group_by(targetLab, matrix) %>%
    mutate(n.rows = n()) %>%
    ungroup %>%
    filter(n.rows >= min.groups) %>% # Require 4 groups.
    mutate(matrix=as.factor(matrix),
           targetLab = as.factor(targetLab))

  #stopifnot(dim(data.plot)[1]>0)
  if(dim(data.plot)[1]>0) {

    if (!table){
      ### Produce Kaplan-Meier.
      km <- xyplot(!cns ~ maxDetect | targetLab+matrix, data=data.plot,
                   prepanel = function(x, ...) {
                     list(xlim = c(-0.005*max(x, na.rm = TRUE), 1.005*max(x, na.rm = TRUE)),
                          ylim=c(0,1))
                   },
                   # The main job is done by this panel function. It computes the KM curve
                   # as well as median/CI/IQR, and plots these quantities.
                   panel = function(x, y) {
                     # y values of polygons.
                     pol.y <- c(0, 0, 1, 1)
                     ## Kaplan-Meier plot.
                     so <- Surv(x, y) # x = Time (incub), y = event indicator (!cns).
                     sf <- survfit(so ~ 1) # Fit Kaplan-Meier.
                     # Make the curve decrease from (0, 1).
                     xx <- c(0, sf$time)
                     yy <- c(1, sf$surv)

                     # Draw Kaplan-Meier curve.
                     panel.xyplot(xx, yy, type="s", col="blue")
                     # add censored points
                     cens.time <-  sort(unique(so[so[,2]==0,1]))
                     cens.surv <- yy[is.element(xx, cens.time)]
                     panel.xyplot(cens.time, cens.surv, pch=4)
                     # Add text on number of groups.
                     grid.text(sprintf("%d groups", length(x)),
                               x = unit(0.9, "npc"), y = unit(0.15, "npc"),
                               gp = gpar(cex = 0.7))


                     # Plot median survival time with CI from survfit if CI==TRUE
                     if (CI) {
                       qsf <- quantile(sf, probs=0.5)
                       m <- qsf$quantile # Median.
                       lcl <- qsf$lower # Lower confidence limit for median.
                       ucl <- qsf$upper # Upper.
                       panel.abline(v=m, lwd=2, col="blue", lty="dashed")
                       # Left part of CI.
                       pol.x <- c(lcl, m, m, lcl)
                       panel.polygon(x=pol.x, y=pol.y, col="blue", alpha=1/8)
                       # Right part of CI.
                       pol.x <- c(ucl, m, m, ucl)
                       panel.polygon(x=pol.x, y=pol.y, col="blue", alpha=1/8)
                     }

                     # Plot median survival time and IQR if CI=FALSE
                     if (!CI) {
                       qsf <- quantile(sf, probs=c(0.25, 0.5, 0.75))
                       m <- qsf$quantile # [Q1, median, Q3].
                       ifelse(!is.na(m[3]), pol.x <- c(m[1], m[3], m[3], m[1]), pol.x <- c(m[1], m[2], m[2], m[1]))
                       panel.abline(v=m[2], lwd=2, lty="dashed")
                       panel.polygon(x=pol.x, y=pol.y, col="blue", alpha=1/8)
                     }
                   },
                   main=as.character(agent.name), # Main title.
                   scales = list(x = "free"),
                   xlab="Duration survival tile (Days)", ylab="Probability of Detection",
                   par.strip.text=list(cex=0.75),
                   #par.strip = list(lines = 5, cex = 0.5),
                   key = list(lines = list(pch = c(15,4), type="p", col = "blue", alpha=c(1/8,1)),
                              text = list(c(ifelse(CI, "Confidence intervals for median survival time", "IQR of survival time"),
                                            "Censored groups")),
                              columns=2
                   )
      ) %>% useOuterStrips
      return(km)

    } else { # table=T
      df <- data.plot %>% select(agent, matrix, targetLab, n.rows) %>% arrange(matrix) %>%
        distinct()
      df$median <- round(as.double(NA),1)
      df$lower.CI <- round(as.double(NA),1)
      df$upper.CI <- round(as.double(NA),1)
      df$q1 <- round(as.double(NA),1)
      df$q3 <- round(as.double(NA),1)

      for (i in 1:nrow(df))
      {
        cur.data <- left_join(slice(df,i), data.plot, by=c("matrix", "targetLab", "agent"))

        so <- Surv(cur.data$maxDetect, !cur.data$cns)
        sf <- survfit(so ~ 1) # Fit Kaplan-Meier.
        qsf <- quantile(sf, probs=c(0.25, 0.5, 0.75))
        df[i, "median"] <- round(qsf$quantile[2],1)
        df[i, "lower.CI"] <- round(qsf$lower[2],1)
        df[i, "upper.CI"] <- round(qsf$upper[2],1)
        df[i, "q1"] <- round(qsf$quantile[1],1)
        df[i, "q3"] <- round(qsf$quantile[3],1)
      }
      return(df)
    }
  }else{
    return(cat("not enough eligible studies\n"))
  }

}


##############################################
# functions needed for the meta analysis script ----

### Misc utility functions.


# All NA's in x?
all.na <- function(x) all(is.na(x))

# Are all the values in x the same (including NA's)?
all.same <- function(x) {
  attributes(x) <- NULL # Don't look for name differences.
  return (identical(rep(x[1], times=length(x)), x))
}

# Min/max functions, returning NA if all elements are NA.
min.na <- function(x) {
  if (all.na(x)) {
    ret <- NA
  } else {
    ret <- min(x, na.rm=T)
  }
  return (ret)
}
max.na <- function(x) {
  if (all.na(x)) {
    ret <- NA
  } else {
    ret <- max(x, na.rm=T)
  }
  return (ret)
}
which.min.na <- function(x) {
  if (all.na(x)) {
    ret <- NA
  } else {
    ret <- which.min(x)
  }
  return (ret)
}
which.max.na <- function(x) {
  if (all.na(x)) {
    ret <- NA
  } else {
    ret <- which.max(x)
  }
  return (ret)
}

# Create a 95% CI from estimate b and standard error se.
create.ci <- function(b, se) {
  lcl <- b - lambda95() * se
  ucl <- b + lambda95() * se
  ci <- c(lcl, ucl)
  return (ci)
}


useOuterStrips <- function (x, strip = strip.default, strip.left = strip.custom(horizontal = FALSE),
                            strip.lines = 1, strip.left.lines = strip.lines)
{
  dimx <- dim(x)
  stopifnot(inherits(x, "trellis"))
  stopifnot(length(dimx) == 2)
  as.table <- x$as.table
  opar <- if (is.null(x$par.settings))
    list()
  else x$par.settings
  par.settings <- modifyList(opar, list(layout.heights = if (as.table) list(strip = c(strip.lines,
                                                                                      rep(0, dimx[2] - 1))) else list(strip = c(rep(0, dimx[2] -
                                                                                                                                      1), strip.lines)), layout.widths = list(strip.left = c(strip.left.lines,
                                                                                                                                                                                             rep(0, dimx[1] - 1)))))
  if (is.character(strip))
    strip <- get(strip)
  if (is.logical(strip) && strip)
    strip <- strip.default
  new.strip <- if (is.function(strip)) {
    function(which.given, which.panel, var.name, ...) {
      row.to.keep <- if (as.table)
        1
      else nrow(trellis.currentLayout())
      if (which.given == 1 && current.row() == row.to.keep)
        strip(which.given = 1, which.panel = which.panel[1],
              var.name = var.name[1], ...)
    }
  }
  else strip
  if (is.character(strip.left))
    strip.left <- get(strip.left)
  if (is.logical(strip.left) && strip.left)
    strip.left <- strip.custom(horizontal = FALSE)
  new.strip.left <- if (is.function(strip.left)) {
    function(which.given, which.panel, var.name, ...) {
      if (which.given == 2 && current.column() == 1)
        strip.left(which.given = 1, which.panel = which.panel[2],
                   var.name = var.name[2], ...)
    }
  }
  else strip.left
  update(x, par.settings = par.settings, strip = new.strip,
         strip.left = new.strip.left, par.strip.text = list(lines = 0.5),
         layout = dimx)}

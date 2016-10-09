# NightLights: VIIRS DMSP Comparison
Study of NOAA' s DMSP and VIIRS data for Department of Economic Affairs India

---
title: "DMSP VIIRS Syncronisation {WIP}"
output: html_document
---

```{r setup, include=FALSE, echo=FALSE, results='hide'}
knitr::opts_chunk$set(echo = TRUE)
library(raster)
library(plotly)
library(rgdal)
library(maptools)
library(maps)
library(mapproj)
```


## Basic Difference:
<span style="color:blue">DMSP-OLS</span>|  <span style="color:blue">VIIRS DNB</span> 
---------------------------            | -------------
Reflectance (Ratio of radiance)        | Radiance
Ratio (DN)                             | Absolute Value (DNB)
1000m resolution                       | 750 m resolution
Cloud & Light Noise Adjusted           | Non adjusted raw values
6 bit quantization [0 - 63 **(2^6)**]  | 13/14 bit [range:0 - 8192+ **(2^13)**]


## Background: 
Running DMSP series on nightlights from 1992-2013 has limitations, such as low spatial resolution (2.7 km ground sample distance), low radiometric resolution (six bit), a saturation effect in bright regions, lack of on-board calibration, lack of systematic recording of in-flight gain changes and lack of multiple spectral bands for discriminating lighting types lack of spectral channels suitable for discrimination of thermal sources of lighting and lack of low light imaging spectral bands suitable for discriminating lighting types. <http://www.star.nesdis.noaa.gov/smcd/spb/nsun/snpp/VIIRS/VIIRS_SDR_Users_guide.pdf>.

#### Distribution of DMSP DN
```{r df, echo =FALSE}
dm13 <- raster("C:/Parth/Personal/Data Mining/GoI DEA/Data/Raw/Night Lights/DMSP India/NL_2013_F18.cmp.st.tif")  
x <- dm13$NL_2013_F18.cmp.st
p <- plot_ly(x = x, type = "histogram", name = "Histogram")
p <- p %>% add_trace(x = density(x)$x, y = density(x)$y, mode = "lines", fill = "tozeroy", yaxis = "y2", name = "Density")
p <- p %>% layout(yaxis2 = list(overlaying = "y", side = "right"), title = "India DMSP:2013")
p
```
* latter is an interactive plot, zoom into sections to avoid scaling distortions*

VIIRS offers a substantial number of improvements over the OLS in terms of spatial resolution, dynamic range, quantization, calibrations and the availability of spectral bands suitable for discrimination of thermal sources of light emissions.

#### Distribution of VIIRS DNB
```{r , echo =FALSE}
vr14 <- raster("C:/Parth/Personal/Data Mining/GoI DEA/Data/Raw/Night Lights/VIIRS/India 14-15/SVDNB_20140301-20140331.avg_rade9_India.tif")  
y <- vr14$SVDNB_20140301.20140331.avg_rade9_India
plot_ly(x = x, type = "histogram", name = "Histogram", title ="A") %>% 
  add_trace(x = density(y)$x, y = density(y)$y, mode = "lines", fill = "blue", yaxis = "y2", name = "Density") %>% 
  layout(yaxis2 = list(overlaying = "y", side = "right"), title = "India VIIRS:2014")
```
* latter is an interactive plot, zoom into sections to avoid scaling distortions*


## Analysis Plan
* Clip raster title by Indian boundaries
+ Reproject the data by Albers Projection
+ Re sample the 15 arc second (750m) VIIRS data to 30 arc second (1km) resolution
+ Quantization from 13 to 6 bits

## Clip raster tile by Indian boundaries
More times than often when any satellite imagery data (DMSP or VIIRS) is clipped for
India, there is a possibility of associated noise. Also clipped data's NA value are wrongly taken as 0 radiance/reflectance. Tile clipping to the specified geography helps
in resolving this. 
```{r , echo =FALSE, results='hide'}
# India Boundaries
# ---------------------
adm <- readShapeSpatial("C:/Parth/Personal/Data Mining/GoI DEA/Data/Raw/Boundaries/State GoI/2011_State.shp")

# VIIRS 2014     
# ---------------------
vr14 <- raster("C:/Parth/Personal/Data Mining/GoI DEA/Data/Raw/Night Lights/VIIRS/India 14-15/SVDNB_20140301-20140331.avg_rade9_India.tif")  

# Clipping Tile by Indian Extent 
# ---------------------
#c1 <- crop(vr14, extent(adm))
#ras.ind <- mask(x=c1, mask=adm) 

# Clipping Tile by Indian Extent: Plot of Clipped Sample DMSP Data  
# ---------------------
ip <- "C:/Parth/Personal/Data Mining/GoI DEA/Data/Raw/Night Lights/In Process/"
clr <- raster(paste0(ip,"DMSP13sample.tif")) 
plot(clr)
```

## Reproject the data by Albers Projection
The Albers equal-area conic projection two standard parallels. The scale and shape are not preserved, distortion is minimal between the standard parallels. Albers prjection
application helps in defining uniform pixel geometry before implementing resampling
```{r vr14, echo =T}
# Albers Projection
# ---------------------
aea <- '+proj=aea +lat_1=20 +lat_2=60 +lat_0=40 +lon_0=-96 +x_0=0 
+y_0=0 +datum=NAD83' 
#ra <- projectRaster(vr14, crs=aea) 

# Resultant GRID
# ---------------------
m <- map(database= "world", regions  = "India", plot=F)
map(database= "world", regions  = "India", project="albers", par=c(39, 45))
map.grid(m)
```
<https://www.nceas.ucsb.edu/~frazier/RSpatialGuides/OverviewCoordinateReferenceSystems.pdf>

### Resampling: from 15 arc seconds to 30 arc second:
The following chunk converts the image pixel from 750m to 1 sq km, by creating an
empty raster grid of specified geometry and then imposing night lights data onto it
```{r , echo =T}
ras <- raster("C:/Parth/Personal/Data Mining/GoI DEA/Data/Raw/Night Lights/VIIRS/India 14-15/SVDNB_20140301-20140331.avg_rade9_India.tif")  

# Extent Computations
# ---------------------
x_min <- extent(ras)[1]
x_max <- extent(ras)[2]
y_min <- extent(ras)[3]
y_max <- extent(ras)[4]
cell_res <- 0.008333333  # 10 km GRID
cell_res <- 0.0008333333  # 10 km GRID

# Number of Columns & Rows : Computations
# ---------------------
x_extent <-as.integer((x_max-x_min)/cell_res)
x_extent.1 <- (((x_max-x_min)/cell_res)-as.integer((x_max-x_min)/cell_res))  #Long
y_extent <- as.integer((y_max-y_min)/cell_res)
y_extent.1 <- (((y_max-y_min)/cell_res)-as.integer((y_max-y_min)/cell_res))  #Lat
n_row <- ifelse(y_extent.1>0.5,(y_extent+2),(y_extent+1))    #lat
n_col <- ifelse(x_extent.1>0.5,(x_extent+2),(x_extent+1))    #long

# Empty Raster
# ---------------------
ras1 <- raster(nrow=n_row,ncol=n_col)
extent(ras1) <- extent(ras)

# Resampling from 750m to 10km
#ras2 <- resample(ras,ras1,method='bilinear')
#extent(ras2) <- extent(ras)
#writeRaster(ras2,filename = "Mar2014.tif",format="GTiff",overwrite=TRUE) 
```

## Quantization
Unfortunately there is no deinfed package in R for bit transformation.
The results for the same will be shared in here shortly

## Appendix: Techincal Notes
* Digital Number (DMSP): is a ratio of Upwelling and Downwelling Radiance
therefore it is a unitless ratio measuring net reflectance
+ DNB Radaiance (VIIRS): is captures radiance measured in watts/cm2/sterdians
+ multiplying the pixel value by 10^9 gives radiance in units of W/cm2-sr
<http://www.crisp.nus.edu.sg/~research/tutorial/image.htm>
### FIN

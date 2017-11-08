# Pre-outbreak forest conditions mediate the effects of spruce beetle (Dendroctonus rufipennis (Kirby)) outbreaks on fuels in subalpine forests of Colorado

Nathan Mietkiewicz, Dominik Kulakowski, Thomas T. Veblen

## Abstract
Over the past 30 years, disturbances have increased in size and frequency across the western United States, and are predicted to continue increasing due to climate change. Spruce beetles (Dendroctonus rufipennis Kirby; SB) are among the most important forest insects causing widespread tree mortality, shaping stand and landscape spatial heterogeneity in the southern Rocky Mountains, USA.  Bark beetles preferentially attack older trees and stands in latter stages of development.  However, as climate intensifies outbreaks by promoting growth of beetle populations and compromising tree defenses, younger trees and stands in early stages of development also are being affected by outbreaks.  To date, no study has considered the consequences of this novel phenomenon on surface and aerial fuel arrangements, nor more generally how pre-outbreak forest conditions mediate the effects of outbreaks.  We collected fuels data across a chronosequence of post-outbreak sites affected by spruce beetle between the 1940s and the 2010s, stratified by young (<130yrs) and old (>130yrs) post-fire stands.  Canopy and surface fuel loads were calculated for each tree and stand, and available crown fuel load, crown bulk density, and canopy bulk densities were estimated.  Canopy bulk density and density of live canopy individuals were reduced in all stands affected by spruce beetle, though foliage loss was proportionally greater in old stands as compared to young stands.  Fine surface fuel loads in young stands were three times greater shortly (< 30yrs) following outbreak as compared to young stands not affected by outbreak, after which the abundance of fine surface fuels decreased to below endemic (i.e., non-outbreak) levels. In both young and old stands, the net effect of spruce beetle outbreaks during the 20th and 21st century reduced total canopy fuels and increased stand-scale spatial heterogeneity of canopy fuels following outbreak. Importantly, the decrease in canopy fuels following outbreaks was greater in young post-fire stands than in older stands, suggesting that spruce beetle outbreaks may more substantially reduce risk of active crown fire when they affect stands in earlier stages of development. This study shows pre-outbreak disturbance legacies influence overall stand structure and can mitigate effects of subsequent disturbance interactions.

# Metadata

**canopybulkdensity.csv** - Vertical canopy bulk denisty estimates for each age/outbreak class aggregated into 0.25m bins.


**canopycover.csv** - Canopy openness measurements for each site across all age/outbreak classes.
-  year = The decade of epidemic spruce beetle attack
-  age = The relative age of spruce-fir stand. 
-  site = The unique id for the site
-  canopyopen = The percent of canopy openness


**canopyfuelload.csv** - All canopy fuel load measurements for each plot, site, age, and outbreak class.  The abbreviations to cell headers are:
-  year = The decade of epidemic spruce beetle attack
-  age = The relative age of spruce-fir stand. 
-  site = The unique id for the site
-  plot = The plot contained within the each site.
-  plot_size = The plot size in m2.
-  spp = The species of tree.  PIEN = englemnann spruce; ABLA = subalpine fir; PICO = lodgepole pine.
-  status = A qualitative assessment of the relative stage of spruce betle attack.  See table 1 for more detail.
-  sf = Whether the tree was standing (S) or fallen (F)
-  lifol = Canopy bulk density of live foliage (Mg/ha)
-  defol = Canopy bulk density of dead foliage (Mg/ha)
-  li1hr = Canopy bulk density of live 1-hour fuels (Mg/ha)
-  de1hr = Canopy bulk density of dead 1-hour fuels (Mg/ha)
-  li10hr = Canopy bulk density of live 10-hour fuels (Mg/ha)
-  de10hr = Canopy bulk density of dead 10-hour fuels (Mg/ha)
-  li100hr = Canopy bulk density of live 100-hour fuels (Mg/ha)
-  de100hr = Canopy bulk density of dead 100-hour fuels (Mg/ha)
-  cbh = Canopy base height (m)
-  lcw = Live crown weight (kg/m2)
-  dcw = Dead crown weight (kg/m2)
-  acfl = Available crown fuel load (Mg/ha)
-  ba_li = Basal area of live class
-  ba_gr = Basal area of green class
-  ba_ye = Basal area of yellow class
-  ba_nd = Basal area of needle class
-  ba_tw = Basal area of twig class
-  ba_br = Basal area of branches class
-  ba_sn = Basal area of snag class
-  ba_de = Basal area of dead class


**coarsesurfacefuelload.csv** - All downed, coarse fuel load measurements for each plot, site, age, and outbreak class.  The abbreviations to cell headers are:
-  year = The decade of epidemic spruce beetle attack
-  age = The relative age of spruce-fir stand. 
-  site = The unique id for the site
-  plot = The plot contained within the each site.
-  n1 = The number of 1-hour fuels
-  n10 = The number of 10-hour fuels
-  n100 = The number of 100-hour fuels
-  dsq100 = The diameter squared of 100-hour fuels (m)
-  fl1 = The fuel load of 1-hourfuels (Mg/ha)
-  fl10 = The fuel load of 10-hourfuels (Mg/ha)
-  fl100 = The fuel load of 100-hour fuels (Mg/ha)
-  nS1000 = The number of sound 1000-hour fuels
-  dsqS1000 = The diameter squared of sound 1000-hour fuels (m)
-  flS1000 = The fuel load of sound 1000-hour fuels (Mg/ha)
-  nR1000 = The number of rotten 1000-hour fuels
-  dsqR1000 = The diameter squared of rotten 1000-hour fuels (m)
-  flR1000 = The fuel load of rotten 1000-hour fuels (Mg/ha)


**finesurfacefuelload.csv** - All downed, fine fuel load measurements for each plot, site, age, and outbreak class.  The abbreviations to cell headers are:
-  year = The decade of epidemic spruce beetle attack
-  age = The relative age of spruce-fir stand. 
-  site = The unique id for the site
-  plot = The plot contained within the each site.
-  duff = The duff layer depth (m)
-  litter = The litter layer depth (m)
-  fuelbed = The fuelbed depth (duff+litter; m)
-  flduff = The duff layer fuel load (Mg/ha)
-  fllitter = The litter layer fuel load (Mg/ha)
-  flfuelbed = The fuelbed fuel load (Mg/ha)


**microplot.csv** - Vegetation and bulk denisty from the microsite measurements for each plot, site, age, and outbreak class.  The abbreviations to cell headers are:
-  year = The decade of epidemic spruce beetle attack
-  age = The relative age of spruce-fir stand. 
-  site = The unique id for the site
-  plot = The plot contained within the each site.
-  line = Each line from the constructed Brown's line
-  bulkden = The bulk denisty of each respective vegetation type (Mg/ha)
-  vegtype = The specific live/dead vegetation class.
    -  LS = Live shrub
    -  DS = Dead shrub
    -  LH = Live herb
    -  DH = Dead herb
    -  LG = Live grass
    -  DG = Dead grass
    -  Li = Litter
 - vegcat = Generalized class based on vegtype


**saplings.csv** - Vegetation and bulk denisty from the microsite measurements for each plot, site, age, and outbreak class.  The abbreviations to cell headers are:
-  year = The decade of epidemic spruce beetle attack
-  age = The relative age of spruce-fir stand. 
-  site = The unique id for the site
-  plot = The plot contained within the each site.
-  subplot = The subplots within the plot.
-  spp = The species of tree.  PIEN = englemnann spruce; ABLA = subalpine fir; PICO = lodgepole pine.
-  foliage = The foliage fuel load (Mg/ha)
-  f1hr = The 1-hour fuel load (Mg/ha)
-  f10hr = The 10-hour fuel load (Mg/ha)
-  fl100hr = The 100-hr fuel load (Mg/ha)
-  fl1000hr = The 1000-hour fuel load (Mg/ha)
-  height = Height of tree (cm)

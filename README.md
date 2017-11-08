# Pre-outbreak forest conditions mediate the effects of spruce beetle (Dendroctonus rufipennis (Kirby)) outbreaks on fuels in subalpine forests of Colorado

Nathan Mietkiewicz, Dominik Kulakowski, Thomas T. Veblen

## Abstract
Over the past 30 years, disturbances have increased in size and frequency across the western United States, and are predicted to continue increasing due to climate change. Spruce beetles (Dendroctonus rufipennis Kirby; SB) are among the most important forest insects causing widespread tree mortality, shaping stand and landscape spatial heterogeneity in the southern Rocky Mountains, USA.  Bark beetles preferentially attack older trees and stands in latter stages of development.  However, as climate intensifies outbreaks by promoting growth of beetle populations and compromising tree defenses, younger trees and stands in early stages of development also are being affected by outbreaks.  To date, no study has considered the consequences of this novel phenomenon on surface and aerial fuel arrangements, nor more generally how pre-outbreak forest conditions mediate the effects of outbreaks.  We collected fuels data across a chronosequence of post-outbreak sites affected by spruce beetle between the 1940s and the 2010s, stratified by young (<130yrs) and old (>130yrs) post-fire stands.  Canopy and surface fuel loads were calculated for each tree and stand, and available crown fuel load, crown bulk density, and canopy bulk densities were estimated.  Canopy bulk density and density of live canopy individuals were reduced in all stands affected by spruce beetle, though foliage loss was proportionally greater in old stands as compared to young stands.  Fine surface fuel loads in young stands were three times greater shortly (< 30yrs) following outbreak as compared to young stands not affected by outbreak, after which the abundance of fine surface fuels decreased to below endemic (i.e., non-outbreak) levels. In both young and old stands, the net effect of spruce beetle outbreaks during the 20th and 21st century reduced total canopy fuels and increased stand-scale spatial heterogeneity of canopy fuels following outbreak. Importantly, the decrease in canopy fuels following outbreaks was greater in young post-fire stands than in older stands, suggesting that spruce beetle outbreaks may more substantially reduce risk of active crown fire when they affect stands in earlier stages of development. This study shows pre-outbreak disturbance legacies influence overall stand structure and can mitigate effects of subsequent disturbance interactions.

### Data

**canopybulkdensity.csv** - Vertical canopy bulk denisty estimates for each age/outbreak class aggregated into 0.25m bins.

**canopycover.csv** - Canopy openness measurements for each site across all age/outbreak classes.

**canopyfuelload.csv** - All canopy fuel load measurements for each plot, site, age, and outbreak class.  The abbreviations to cell headers are:
-  Year = The decade of epidemic spruce beetle attack
-  Age = The relative age of spruce-fir stand. 
-  Site = The unique id for the site
-  Plot = The subplot contained within the each site.
-  Plot.Size = The plot size in m2.
-  Species = The species of tree.  PIEN = englemnann spruce; ABLA = subalpine fir; PICO = lodgepole pine.
-  Status = A qualitative assessment of the relative stage of spruce betle attack.  See table 1 for more detail.
-  S.F = Whether the tree was standing (S) or fallen (F)
-  LiFol = Canopy bulk density of live foliage (Mg/ha)
-  DeFol = Canopy bulk density of dead foliage (Mg/ha)
-  Li.1hr = Canopy bulk density of live 1-hour fuels (Mg/ha)
-  De.1hr = Canopy bulk density of dead 1-hour fuels (Mg/ha)
-  Li.10hr = Canopy bulk density of live 10-hour fuels (Mg/ha)
-  De.10hr = Canopy bulk density of dead 10-hour fuels (Mg/ha)
-  Li.100hr = Canopy bulk density of live 100-hour fuels (Mg/ha)
-  De.100hr = Canopy bulk density of dead 100-hour fuels (Mg/ha)
-  CBH = Canopy base height (m)
-  LCW = Live crown weight (kg/m2)
-  DCW = Dead crown weight (kg/m2)
-  ACFL = Available crown fuel load (Mg/ha)
-  BA_Li = Basal area of live class
-  BA_Gr = Basal area of green class
-  BA_Ye = Basal area of yellow class
-  BA_Nd = Basal area of needle class
-  BA_Tw = Basal area of twig class
-  BA_Br = Basal area of branches class
-  BA_Sn = Basal area of snag class
-  BA_De = Basal area of dead class

**coarsesurfacefuelload.csv** - All downed, coarse fuel load measurements for each plot, site, age, and outbreak class.  The abbreviations to cell headers are:
-  Year = The decade of epidemic spruce beetle attack
-  Age = The relative age of spruce-fir stand. 
-  Site = The unique id for the site
-  Plot = The subplot contained within the each site.
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
-  Year = The decade of epidemic spruce beetle attack
-  Age = The relative age of spruce-fir stand. 
-  Site = The unique id for the site
-  Plot = The subplot contained within the each site.
-  Duff = The duff layer depth (m)
-  Litter = The litter layer depth (m)
-  Fuelbed = The fuelbed depth (duff+litter; m)


**microplot.csv** - Vegetation and bulk denisty from the microsite measurements for each plot, site, age, and outbreak class.  The abbreviations to cell headers are:
-  Year = The decade of epidemic spruce beetle attack
-  Age = The relative age of spruce-fir stand. 
-  Site = The unique id for the site
-  Plot = The subplot contained within the each site.
-  Line = Each line from the constructed Brown's line
-  BulkDensity = The bulk denisty of each respective vegetation type (Mg/ha)
-  Veg_Type = The specific live/dead vegetation class.
    -  LS = Live shrub
    -  DS = Dead shrub
    -  LH = Live herb
    -  DH = Dead herb
    -  LG = Live grass
    -  DG = Dead grass
    -  Li = Litter
 - Veg.Cat = Generalized class based on Veg_Type





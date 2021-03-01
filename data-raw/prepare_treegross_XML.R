#==============================================================================
#
#  Preprocessing of ForestSimulatorNWGermany6.xml
#
#  Could also be done at package load. It's only done in advance to keep the
#  number of dependencies low. This makes sense because the XML does not change
#  often (last change 2020-02-29 and previous version 5 is from 2012-10-12).
#
#  Taken from anstaltspaket from Robert Nuske (2021-03-01).
#==============================================================================

if(!requireNamespace('xml2', quietly=TRUE))
  stop('The package xml2 is needed to parse the TreeGrOSS XML-file.')


# read xml and select all relevant nodes
xml <- xml2::read_xml("data-raw/ForestSimulatorNWGermany6.xml")
nodes <- xml2::xml_find_all(xml, ".//SpeciesDefinition")

# for every node (table row): get children/cells, fetch their contents
data <- t(sapply(nodes,
                 function(x){
                   kids <- xml2::xml_children(x)
                   xml2::xml_text(kids)
                   }))

# convert to data.frame and slab names on it
spf <- as.data.frame(data, stringsAsFactors=FALSE)
colnames(spf) <- xml2::xml_name(xml2::xml_children(nodes[1]))


# keep only formulas one can use w/o additional info (eg. C66, harvested trees)
spf <- spf[, c("Code", "ShortName", "LongName", "LatinName", "HandledLikeCode",
               "UniformHeightCurveXML", "VolumeFunctionXML", "Crownwidth",
               "Crownbase", "SiteIndex", "PotentialHeightIncrement",
               "HeightIncrement")]

# clean up --------------------------------------------------------------------
# kick out grass & neu
spf <- spf[!(spf$Code == '999' | spf$Code == '-999'), ]
# kill wikipedia links
spf$LatinName <- gsub('\\s+http.*|\\s+\\(http.*', '', spf$LatinName)
spf[6:12] <- vapply(spf[6:12],
                    FUN=function(x){
                      # ln -> log
                      x <- gsub('log', 'log10', x)
                      x <- gsub('ln',  'log', x)
                      # rename variables
                      x <- gsub('t.d',     'bhd', x)
                      x <- gsub('t.h',     'h', x)
                      x <- gsub('t.cb',    'ka', x)
                      x <- gsub('t.age',   'alter', x)
                      x <- gsub('t.si',    'bon100', x)
                      x <- gsub('sp.dg',   'dg', x)
                      x <- gsub('sp.hg',   'hg', x)
                      x <- gsub('sp.h100', 'h100', x)
                      },
                    FUN.VALUE=rep('character', nrow(spf)))
# codes to integer
spf$Code <- as.integer(spf$Code)
spf$HandledLikeCode <- as.integer(spf$HandledLikeCode)

# save the data.frame as internal data
save(spf, file="R/sysdata.rda", version=3)

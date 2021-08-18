# From the fringes to the core
### An analysis of right-wing populists’ linking practices in seven EU parliaments and Switzerland


This repository provides some data and scripts related to the paper:

* [von Nordheim, G., Rieger, J. & Kleinen, K. (2021).](https://doi.org/10.1080/21670811.2021.1970602) From the fringes to the core – An analysis of right-wing populists’ linking practices in seven EU parliaments and Switzerland. *Digital Journalism*.

For bug reports, comments and questions please use the [issue tracker](https://github.com/JonasRieger/fringes/issues).

## Related Software
* [tosca](https://github.com/Docma-TU/tosca) is used for managing and manipulating the text data to a structure requested by ``ldaPrototype``.
* [ldaPrototype](https://github.com/JonasRieger/ldaPrototype) is used to determine a prototype from a number of runs of Latent Dirichlet Allocation.
* [ineq](https://cran.r-project.org/package=ineq) is used to calcaulate Gini coefficients.
* [longurl](https://github.com/hrbrmstr/longurl) is used for expanding short urls and [urltools](https://github.com/Ironholds/urltools) is useful for extracting url cores from urls.
* [RCurl](https://cran.r-project.org/package=RCurl) and [RJSONIO](https://cran.r-project.org/package=RJSONIO) are used for scraping.
* [batchtools](https://github.com/mllg/batchtools) is used for calculating (prototypes of) LDAs on the High Performace Compute Cluster [LiDO3](https://www.lido.tu-dortmund.de/cms/en/LiDO3/index.html).
* [data.table](https://github.com/Rdatatable/data.table) is used for managing and storing tabular data, e.g. texts and meta information.
* [tm](https://cran.r-project.org/package=tm), [lubridate](https://lubridate.tidyverse.org/) and [utf8](https://github.com/patperry/r-utf8) are useful packages for preprocessing and managing text data.
* [ggplot2](https://ggplot2.tidyverse.org/), [ggpubr](https://github.com/kassambara/ggpubr/), [ggrepel](https://github.com/slowkow/ggrepel) and [cividis](https://github.com/marcosci/cividis) are used to create the plots.
* [beanplot](https://cran.r-project.org/package=beanplot) is used to visualize advanced boxplots.

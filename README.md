# From the fringes to the core
### An analysis of right-wing populists’ linking practices in seven EU parliaments and Switzerland


This repository provides some data and scripts related to the paper:

* [von Nordheim, G., Rieger, J. & Kleinen-von Königslöw, K. (2021).](https://doi.org/10.1080/21670811.2021.1970602) From the fringes to the core – An analysis of right-wing populists’ linking practices in seven EU parliaments and Switzerland. *Digital Journalism*.

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

## Usage
Please note: For legal reasons the repository cannot provide all data. Please [let us know](https://github.com/JonasRieger/fringes/issues) if you feel that there is anything missing that we could add. 

In the ``code`` folder you can view and trace the chronology of the R code used.

The folder ``countries`` contains the following structure for all examined countries:
* the subfolder ``docs`` contains the used preprocessed texts (bag of words with indices of vocabulary - see ``vocab.txt/.rds``),
* the subfolder ``proto`` contains for all considered values K=20,25, ..., 75 the LDAPrototype models,
* the subfolder ``tables`` contains some descriptive statistics in tabular form,
* ``docs.rds`` contains the preprocessed texts in the form that can be used with the [ldaPrototype](https://github.com/JonasRieger/ldaPrototype) package,
* ``onepercent.txt`` specifies the parties of the country that would pass an artificial 1% hurdle,
* ``parties.csv`` and ``parties_col.csv`` contain information about party abbreviations and used colors for the party,
* ``topwords30.csv`` contains the 50 topwords for all K=30 topics of the corresponding LDAPrototype model,
* ``vocab.txt`` contains the corresponding vocabulary, ``vocab.rds`` as RDS file (see ``docs.rds``).

The folder ``countries/incl_UK`` also contains the (differing) results including the results of United Kingdom, which was omitted from the paper for interpretational reasons.

The ``misc`` folder contains various summary tables, e.g. in ``party_names.csv`` all party abbreviations are given - sorted by country - and in ``party_names2.csv`` sorted by abbreviation.

At last the ``pdf`` folder contains three PDFs:
* ``statistics_general.pdf`` gives an insight into descriptive statistics concerning the parliament and the raw data set (see also ``code/3statistics_general.Rmd``),
* ``create_textmeta.pdf`` gives the process and statistics of the preprocessing (see also ``code/4create_textmeta.Rmd``),
* ``plots.pdf`` contains some descriptive plots and further analytical plots based on the LDA results.

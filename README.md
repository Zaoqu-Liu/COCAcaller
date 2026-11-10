# COCAcaller

Identify COCA subtypes (COCA1, COCA2, COCA3) from miRNA expression in chronic pancreatitis, or classify a sample as Normal, with a pre-trained multi-layer perceptron classifier (`mlpcla`, trained with the `neuralnet` package).

## Installation

```r
# install.packages("devtools")
devtools::install_github("Zaoqu-Liu/COCAcaller")
```

The classifier needs the `neuralnet` package (`install.packages("neuralnet")`).

## Usage

`COCAcaller(data)` takes a data frame or matrix with samples in rows and the 12 miRNA features of the model in columns (`miR_20a_5p`, `miR_24_3p`, `miR_6134`, `miR_1285_3p`, `miR_1910_5p`, `miR_652_3p`, `let_7b_3p`, `miR_486_3p`, `miR_640`, `miR_7845_5p`, `miR_4420`, `miR_6511a_5p`). It returns the probability of each class (`Normal_prob`, `COCA1_prob`, `COCA2_prob`, `COCA3_prob`) and the predicted `Group`, the class with the highest probability.

```r
library(COCAcaller)

# Example input shipped with the package: one sample, 12 features
ex <- read.csv(system.file("www", "InputExample.csv", package = "COCAcaller"))
x  <- as.data.frame(t(setNames(ex$Value, ex$Feature)))

COCAcaller(x)
#>   Normal_prob COCA1_prob   COCA2_prob COCA3_prob  Group
#> 1    0.896249  0.1246225 0.0001008888 0.01057174 Normal
```

`COCA_app()` launches a Shiny app that predicts COCA subtypes from an uploaded miRNA feature file. On first use it checks for the packages it needs (shiny, shinyjs, bslib, readr, stringr, slickR, markdown, DT, shinyWidgets, shinyFeedback, neuralnet) and installs missing ones.

```r
COCA_app()
```

See `?COCAcaller` and `?COCA_app` for details.

## License

MIT, see `LICENSE` and `LICENSE.md`.

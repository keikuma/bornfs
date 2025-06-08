# BornFS

![Scala](https://img.shields.io/badge/Scala-3.x-red.svg)

## Overview

**BornFS** is a **feature selection algorithm** implemented in **Scala**.  
It efficiently selects a subset of relevant features from a large dataset while maintaining high accuracy.

## Features

- **Scalability** - The algorithm is highly efficient and scales well to large datasets.
- **High accuracy** - The relevance loss between selected features and labels can be controlled using the `-t` argument.
- **High concentration** - It selects only a small subset of relevant features from a large feature set.

## Prerequisites

Ensure you have the following installed before running the project:
- Scala `3.3.x`
- Java `JDK 11`
- [SBT](https://www.scala-sbt.org/) (Scala Build Tool)

## Data Requirements

BornFS assumes that feature values are discrete. In particular, best results are
obtained when each feature takes binary values (0 or 1).

## Installation

### Clone the Repository

```sh
git clone https://github.com/yourusername/bornfs.git
cd bornfs
sbt run -i xxx
```

## Tutorial

To learn how the algorithm works, use the following command:

```sh
sbt> run -T true -i test
```

## Reference

Kilho Shin, Chris Liu, Katsuyuki Maeda, Hiroaki Ohshima:
BornFS: Feature Selection with Balanced Relevance and Nuisance and Its Application to Very Large Datasets. ICAART (3) 2024: 1100-1107

## Questions and Issues

If you have any questions, suggestions, or bug reports, please open an issue on GitHub:  

➡ [Submit an Issue](https://github.com/yourusername/repository-name/issues)  

We appreciate your feedback!

# IBM ExaBounds: An Analytical Exascale System Model

## Introduction

_IBM ExaBounds_ models the performance and power of a large-scale high-performance computer (HPC). It is an analytical model (as opposed to a simulator), i.e., the interactions between system components are described mathematically rather than functionally. IBM ExaBounds comprises detailed performance models of multi-core processors, memory hierarchies and interconnect networks as well as the _IBM Memory-Scheduler-Agnostic Power Model_ for DRAMs. To model processor power it invokes [McPAT](https://code.google.com/archive/p/mcpat/).

The input to the system model are a set of JSON files describing the [hardware configuration](https://github.com/exabounds/IBM-ExaBounds/tree/master/Architectures) and a [software profile](https://github.com/exabounds/IBM-ExaBounds/tree/master/DataInOut). Software profiles can be generated by the [_IBM Platform-Independent Software Analysis_](https://github.com/exabounds/ibm-pisa) tool and extrapolated to the target scale with the **IBM Exascale Extrapolator**. (The latter is included in this _IBM ExaBounds_ repository.) The [_IBM Appresentor_](https://github.com/exabounds/IBM-Appresentor) automates the design of experiments and profiling required for extrapolation.

The complete tool flow is this:
1. _IBM Appresentor_, kicking off _IBM Platform-Independent Software Analysis_
2. _IBM Exascale Extrapolator_
3. _IBM ExaBounds_

## Getting started

Setting up and using the tools is described in the documentation for [_IBM ExaBounds_](https://github.com/exabounds/IBM-ExaBounds/blob/master/Documentation/ExaBounds/Manual.pdf) and the [_IBM Exascale Extrapolator_](https://github.com/exabounds/IBM-ExaBounds/blob/master/Documentation/ExtrAx/ExtrAx.pdf).

## Background

This tool chain was developed by the [_Algorithms & Machines_](http://researcher.watson.ibm.com/researcher/view_group.php?id=6395) team at _IBM Research – Zurich_ in the context of [_Dome_](http://www.dome-exascale.nl/), a joint program between _IBM Research – Zurich_ and [_Astron_](http://www.astron.nl/), the Netherlands Institute for Radio Astronomy.

The following main publications present the science behind _IBM ExaBounds_ and the _IBM Exascale Extrapolator_:
* Rik Jongerius, Andreea Anghel, Gero Dittmann, Giovanni Mariani, Erik Vermij, Henk Corporaal: [_"Analytic multi-core processor model for fast design-space exploration."_](http://ieeexplore.ieee.org/document/8168422/) Transactions on Computers, IEEE, 2018.
* Andreea Anghel: [_"On Large-Scale System Performance Analysis and Software Characterization."_](https://www.research-collection.ethz.ch/bitstream/handle/20.500.11850/212482/1/thesis_research_collection_no_CV.pdf) Ph.D. Thesis, ETH Zurich, 2017. _(For interconnect and branch-prediction modeling. Conference publications in preparation.)_
* Sandeep Poddar, Rik Jongerius, Leandro Fiorin, Giovanni Mariani, Gero Dittmann, Andreea Anghel, and Henk Corporaal: [_"MeSAP: A fast analytic power model for DRAM memories."_](http://ieeexplore.ieee.org/document/7926957/) Design, Automation Test in Europe (DATE), 2017, pp. 49-54.
* Giovanni Mariani, Andreea Anghel, Rik Jongerius, Gero Dittmann: [_"Scaling Properties of Parallel Applications to Exascale."_](http://rd.springer.com/article/10.1007/s10766-016-0412-y) International Journal of Parallel Programming 44(5), 975--1002, Springer, 2016.
* Giovanni Mariani, Andreea Anghel, Rik Jongerius, Gero Dittmann: [_"Classification of Thread Profiles for Scaling Application Behavior."_](http://www.sciencedirect.com/science/article/pii/S0167819117300418) Parallel Computing, Elsevier, 2017.

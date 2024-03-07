# Replication package for: "A Monetary Measure of the Strength and Robustness of the Attraction Effect" by Crosetto and Gaudeul, Economics Letters 2016.

Here we will put all the replication files for this paper.

## Contents

In particular, you can find

-   the paper, in the `/Paper and Appendices` folder
-   the experimental instructions and the appendices, in the `/Paper and Appendices` folder
-   a presentation given in Rome in 2016 in the `/Presentation` folder
-   the raw data in the `/Data` folder
-   the analysis scripts to generate the paper's results, in `/Analysis`
-   the information needed to recreate the stimuli for replication, in `/Stimuli`

## Recreating the stimuli: howto

The exact stimuli used in the original experiment hare provided in the `/Stimuli` folder. Subjects had to choose one item out of menus of 3 items and of 6 items (only the 3-menus are used in the Economics Letters paper). You get:

-   a `Menus.csv` that contains python lists in which each element is a menu option.

-   a `Menu_Inspector` pdf that shows visually the menus, the best option, and the profit of such best option.

-   a `Shapes` folder that contains all the images of all the shapes. Those were in turn generated with python

-   a `RegularShapeCreator.py` file that takes the `Menus.csv` and generates the shapes. **This does not work,** it has been written with python2.7 that is no more supported. But it gives you at least *an idea* of how we created the shapes. Other options are possible and actually potentially easier, using `ggplot` and `R` or `seaborn/matplotlib` and `python3`.

## A note on the experimental software

The experimental software was written in `python2.7` using `wxPython`. It does not run any more on `python3`. It never will run again, unless a lot of effort is devoted to that – and this is at the moment and in the foreseeable future not going to happen.

In the end it's a simple environment in which you choose among 3 options. We document all the options in the Stimuli folder. It should be straightforward to create a software to replicate our results, without using the original experimental software.

We are sorry about this – but to be completely honest, this was the first software that Paolo Crosetto ever wrote, and it was an amateurish effort. It did work and created reliable data; it was not easy to maintain, and did not follow *any* best software engineering practices whatsoever. Sorry.

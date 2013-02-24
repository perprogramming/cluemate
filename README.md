Cluemate
========

Cluemate is an advisory tool for the deduction board game [Clue](http://en.wikipedia.org/wiki/Cluedo).  
As such it helps the player to make his next move based on the
game play information it collects continuosly.

Cluemate is the result of my participation in the course
"Functional and Declarative Programming" during my studies in
Computer Science at the [Bonn-Rhein-Sieg University of Applied Science](http://www.hochschule-bonn-rhein-sieg.de/en/Home.html).

Cluemate is written in [SWI-Prolog](http://www.swi-prolog.org/), an open source implementation of the declarative programming language Prolog.


Installation
============

First make sure you have SWI-Prolog installed in Version 6.2 or higher and it's binary swipl
is in the current PATH. On Mac OSX SWI-Prolog is available via [Macports](http://www.macports.org/):
```bash
sudo port install swi-prolog
```

Clone the source code:
```bash
git clone https://github.com/perprogramming/cluemate.git
```

After cloning the repository, you can compile the cluemate and start it right away:
```bash
./compile
./cluemate
```
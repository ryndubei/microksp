microksp
========

Gravity turn assistant for KSP.

Build
-----

Debian-based Linux distributions:

```
sudo apt install freeglut3-dev haskell-stack
cd mksp
stack setup
stack build
```
Then use `stack run` to run the program.

Setup
-----

$XDG_DATA_HOME/microksp (.local/share/microksp on Linux) should contain atmosphere data files <planet (lowercase)>_atmosphere.txt for air resistance on the planet to be taken into account. An example file already exists for Kerbin: see the atmosphere directory. In the file, each line should contain "<altitude>m <temperature>K <pressure>P".

Controls
--------

- up/down : increase/decrease gravity kick velocity
- right/left : increase/decrease gravity kick angle
- shift/ctrl : increase/decrease engine thrust
- p : cycle through planets

License
-------
GPL-3.0-or-later

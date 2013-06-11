status-tools
============

Generates system status info, intended for use with xmobar. This is a reimplementation of some shell scripts I used to use, written in Haskell for speed and happiness.

The file you want is `status-tools.hs`. Everything else here is cruft.

The source contains some references which are pretty specific to my system. If you compile and run without modification it'll probably just throw an exception.

To-do
-----

- Implement mocp parser
- Switch to ifconfig
- Be smarter about parsing - parsec?

Licence
-------

On the off-chance that anyone wants to use this code, the licence is BSD 3-clause.

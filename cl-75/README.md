

These directories contain the Caml-Light programs contained in the book:

        The Functional Approach to Programming with Caml
        by Guy Cousineau and Michel Mauny
        Cambridge University Press
        ISBN: 0 521 57183 9 (Hardback), SBN: 0 521 57681 4 (Paperback)

and

        Approche Fonctionnelle de la Programmation
        by Guy Cousineau and Michel Mauny
        Ediscience International, Collection Informatique
        ISBN 0989-392-X

You may notice a few changes in these programs, compared to those that
are published in the book. Some of these changes had to be made to
correct a few errors, but, for the vast majority, they were performed
for the programs to follow the evolution of the Caml-Light system.

This distribution (0.3) is complete. Each directory gives access to
programs contained into a particular chapter, the Util directory
containing programs common to all chapters.

To restore the environment provided by a specific chapter, go to the
corresponding directory, run Caml-Light, and evaluate:

        include "load";;

which will include the contents of the `load.ml' file in your
Caml-Light session.


# INSTALLATION

The following installation will compile all the ML files in this tree.

Simply execute `make all` in the root directory of this distribution
(probably the directory where this file is), on a machine with
Caml-Light (version 0.6 or 0.7) installed.

If you want to rebuild the pictures occurring in the book, execute
`make pictures`, on a machine with Caml-Light and the MLgraph
library. You shouldn't need to do this, and, currently, we
don't provide any support for that.


                                        Guy Cousineau
                                        Michel Mauny

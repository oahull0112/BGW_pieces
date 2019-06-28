# BGW_pieces
my_main.f90 contains all subroutines. Takes an inclusion array that describes which ranges of rows in a data matrix to read in.
My_main splits the inclusion array into valence and conduction parts, then each MPI task generates its own local inclusion array.
Using the local inclusion array, the MPI task creates a union of hdf5 hyperslabs, then reads in the hyperslab.

The other .f90 files are each subroutine "unit" written separately for testing purposes.

# Ephemeral Trees
The `ephemeral_trees` project implements a treap data structure using
timestamps for the heap which allows efficient removal of expired entries.

The implementation is derived from the pseudo-code presented in the paper:

	Efficient Management of Short-Lived Data
	Albrecht Schmidt and Christian S. Jensen
	February 1, 2008
	http://arxiv.org/abs/cs/0505038

The included test suite graphs the performance:
![graph](https://github.com/sigscale/ephemeral_trees/blob/master/test/ephemeral_trees_load_SUITE_data/example-graph.png)

## Build
Port the `ephemeral_trees` application to your environment:

	$ aclocal; autoheader; autoconf; automake --add-missing
	$ ./configure

Build the `ephemeral_trees` application and documentation:

	$ make

## Test
Test the `ephemeral_trees` application:

	$ make check

## Install
Install the `ephemeral_trees` application:

	$ make install

## License
Everything here is distributed under a BSD 2-clause license.

Building
--------

This really should be built in a sandbox -- and we'll need c2hs.

    cd <directory where this was cloned>
    cabal sandbox init
    cabal install c2hs

Get the source for the leveldb with zlib alterations and correlated
haskell bindings.

    git clone git@github.com:greyson/leveldb-haskell.git leveldb-haskell
	git clone git@github.com:greyson/leveldb-mcpe.git leveldb-haskell/leveldb-mcpe

First, we need to build the leveldb specific to MCPE (with zlib
compression). The modified library is included (git subtree).

    cd leveldb-haskell/leveldb-mcpe
    CXXFLAGS="-fPIC" make libleveldb.a

Then we need to build the haskell bindings to use the new leveldb
instance (remember to note that our sandbox is above us):

    cd ..
    cabal sandbox --sandbox=../.cabal-sandbox init
    cabal install --extra-lib-dirs $PWD/leveldb-mcpe \
       --extra-include-dirs $PWD/leveldb-mcpe/include

Now we are ready to build the pocketmine library and programs.

    cd ..
    cabal install

And finally, run `ICanHasMap` on a world pulled from a device:

    .cabal-sandbox/bin/ICanHasMap <world directory>

This world must have both the full `db` directory, and the `level.dat`
file. The console-based map viewer will start at the spawnpoint for
the world.

#/usr/bin/env bash
 
error() {
        what=$1
        echo "Error in $what" >&2
        exit 1
}
 
branch=$1
echo "*** Will build branch $branch ***"
 
cd ~/Barbarossa
git fetch origin || error "git fetch"
git checkout -b $branch origin/$branch \
    || git checkout $branch \
    || error "git checkout"
 
ver=$(grep Version: Barbarossa.cabal|awk '{print $2}'|sed 's///')
echo "*** Version is $ver ***"

rm dist/build/Barbarossa/Barbarossa-tmp/*/*.o
rm dist/build/Barbarossa/Barbarossa-tmp/*/*.hi
 
cabal configure || error "configure"
cabal build || error "build"
 
mv dist/build/Barbarossa/Barbarossa ~/Engines/Barbarossa-${ver}-$branch || error "move"

echo "*** Done ***"

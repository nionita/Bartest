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

stack clean
 
stack build || error "build"

key="local-install-root"

sir=$(stack path | grep "$key" | sed 's/'$key': //')
echo "*** Move binary from: $sir/bin"

mv $sir/bin/Barbarossa ~/Engines/Barbarossa-${ver}-$branch || error "move"

echo "*** Done ***"

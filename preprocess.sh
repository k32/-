#!/bin/sh
# Preprocess the grammar

directory=$1
main=$2
if [[ ! -f $main ]]
then
    echo "Usage: $(basename $0) directory main_file"
    exit 1
fi

# Prepare input files for processing by GNU CPP
for file in $(find $directory -name '*.dict' -or -name '4.0.*' -and -not -name '*.bak')
do
    sed  -i.bak                                       \
         -e 's/^\/\([a-z0-9\/.]*\)/#include "\1"\n/g' \
         -e 's/#include \+"\/\(.*\)"/#include "\1"/'  \
         $file
    echo "Preparing" $file
done

cpp -nostdinc -CC -P -I . -I $directory -o "$directory.dict.in" $main

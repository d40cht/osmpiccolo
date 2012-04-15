bzcat  british_isles.osm.bz2 | osmosis --read-xml enableDateParsing=no file=- --bounding-box top=51.939 bottom=51.663 left=-1.678 right=-1.144 --write-xml file=- | bzip2 > westOfOxford.osm.bz2

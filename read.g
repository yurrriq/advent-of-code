#
# AoC: Advent of Code solutions in GAP
#
# Reading the implementation part of the package.
#
ReadPackage( "AoC", "gap/AoC.gi" );

for file in Filtered( DirectoryContents( DirectoriesPackageLibrary( "AoC", "gap/Year2019" )[1] ), string -> EndsWith(string, ".g") ) do
  ReadPackage( "AoC", Filename( Directory("gap/Year2019"), file ) );
od;

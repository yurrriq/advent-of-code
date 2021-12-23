#
# AoC: Advent of Code solutions in GAP
#
#! @Chapter Introduction
#!
#! AoC is a package which does some
#! interesting and cool things
#!
#! @Chapter Functionality
#!
#!
#! @Section Example Methods
#!
#! This section will describe the example
#! methods of AoC

#! @Description
#!   Insert documentation for your function here
BindGlobal( "AoC", rec() );

DeclareCategory( "IsPuzzle", IsObject );
BindGlobal( "PuzzleFamily",
    NewFamily( "PuzzleFamily", IsPuzzle ) );
BindGlobal( "TYPE_PUZZLE",
    NewType( PuzzleFamily, IsPuzzle ) );

DeclareOperation( "Puzzle", [IsString, IsString, IsFunction, IsFunction] );
DeclareAttribute( "Input", IsPuzzle );
DeclareAttribute( "PartOne", IsPuzzle );
DeclareAttribute( "PartTwo", IsPuzzle );

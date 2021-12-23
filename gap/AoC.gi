#
# AoC: Advent of Code solutions in GAP
#
# Implementations
#
AoC.Year2019 := rec();

InstallMethod( Puzzle,
    [IsString, IsString, IsFunction, IsFunction],
    function( Year, Day, PartOne, PartTwo )
        local puzzle;
        puzzle := rec(
            Input := function( )
                return InputTextFile(
                    Filename(
                        DirectoriesPackageLibrary( "AoC", StringFormatted( "input/{1}", Year ) )[1],
                        StringFormatted( "day{1}.txt", Day )
                    )
                );
            end,
            PartOne := PartOne,
            PartTwo := PartTwo
        );
        Objectify( TYPE_PUZZLE, puzzle );
        return puzzle;
    end );
InstallMethod( Input,
  "of a puzzle",
  [ IsPuzzle ],
  puzzle -> puzzle!.Input() );
InstallMethod( PartOne,
  "of a puzzle",
  [ IsPuzzle ],
  puzzle -> puzzle!.PartOne );
InstallMethod( PartTwo,
  "of a puzzle",
  [ IsPuzzle ],
  puzzle -> puzzle!.PartTwo );

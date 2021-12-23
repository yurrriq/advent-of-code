FuelRequiredModule := function( mass )
    return Int( Float( mass / 3 ) ) - 2;
end;;


PartOne := function( )
    local input, line, mass, sum;;
    sum := 0;
    input := AoC.Year2019.Day01.Input();
    line := ReadLine( input );
    repeat
        mass := Int( Chomp( line ) );
        sum := sum + FuelRequiredModule( mass );
        line := ReadLine( input );
    until line = fail or IsEndOfStream( input );
    return sum;
end;;


TotalFuelRequiredModule := function( mass )
    local fuel;;
    fuel := FuelRequiredModule( mass );
    if IsPosInt( fuel ) then
        return fuel + TotalFuelRequiredModule( fuel );
    else
        return 0;
    fi;
end;;


PartTwo := function( )
    local input, line, mass, sum;;
    sum := 0;
    input := Input(AoC.Year2019.Day01);
    line := ReadLine( input );
    repeat
        mass := Int( Chomp( line ) );
        sum := sum + TotalFuelRequiredModule( mass );
        line := ReadLine( input );
    until line = fail or IsEndOfStream( input );
    return sum;
end;;


AoC.Year2019.Day01 := Puzzle( "2019", "01", PartOne, PartTwo );

SetPackageInfo( rec(

PackageName := "AoC",
Subtitle := "Advent of Code solutions in GAP",
Version := "0.0.1",
Date := "23/12/2021", # dd/mm/yyyy format
License := "GPL-2.0-or-later",

Persons := [
  rec(
    FirstNames := "Eric",
    LastName := "Bailey",
    WWWHome := "https://github.com/yurrriq",
    Email := "eric@ericb.me",
    IsAuthor := true,
    IsMaintainer := true,
    # PostalAddress := ...,
    Place := "Minneapolis, MN, USA",
    Institution := "Electric Wizardry, LLC",
  ),
],

SourceRepository := rec(
    Type := "git",
    URL := "https://github.com/yurrriq/advent-of-code",
),
IssueTrackerURL := Concatenation( ~.SourceRepository.URL, "/issues" ),
PackageWWWHome  := "https://yurrriq.github.io/advent-of-code/",
PackageInfoURL  := Concatenation( ~.PackageWWWHome, "PackageInfo.g" ),
README_URL      := Concatenation( ~.PackageWWWHome, "README.md" ),
ArchiveURL      := Concatenation( ~.SourceRepository.URL,
                                 "/releases/download/v", ~.Version,
                                 "/", ~.PackageName, "-", ~.Version ),

ArchiveFormats := ".tar.gz",

Status := "other",

AbstractHTML   :=  "",

PackageDoc := rec(
  BookName  := "AoC",
  ArchiveURLSubset := ["doc"],
  HTMLStart := "doc/chap0.html",
  PDFFile   := "doc/manual.pdf",
  SixFile   := "doc/manual.six",
  LongTitle := "Advent of Code solutions in GAP",
),

Dependencies := rec(
  GAP := ">= 4.11",
  NeededOtherPackages := [ ],
  SuggestedOtherPackages := [ ],
  ExternalConditions := [ ],
),

AvailabilityTest := ReturnTrue,

TestFile := "tst/testall.g",

# Keywords := [ "TODO" ],

));

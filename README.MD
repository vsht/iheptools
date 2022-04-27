# Description

IHEPTools is a Mathematica interface to access some parts of the INSPIRE-HEP
library and arXiv using Wolfram Mathematica via REST-API. It requires at least Mathematica 12.

The code is still under development and not 100% ready for productive use.



# Installation

Just download the file `IHEPTools.m` into `FileNameJoin[{$UserBaseDirectory,"Applications","IHEPTools"}]`
so that you can load it via

```
<<IHEPTools`
```

# Typical usage scenarios

## Daily arXiv reading

### Basic usage

Create an empty notebook (call it e.g. `arXivReading.nb`) in a dedicated folder and paste the following code into it

```
<< IHEPTools`
```

```
(*
    Fetch preprints from the categories hep-th, hep-ph and hep-lat
    that appeared yesterday (you can also a use a different date range!)
*)
newPreprints = IHEPGetPreprints[{"hep-th", "hep-ph", "hep-lat"},
 {DayPlus[Yesterday, -1], Yesterday}];
```

```
(*
    Go through the descriptions of the preprints. 
    Use the buttons Prev and Next to move forwards or backwards
    If you are interested in a particular preprint, just click 
    on the link button
*)
IHEPShowPreprints[newPreprints]
```

When you are done for today, you can evaluate the command

```
IHEPUpdateDailyReadingLog[{"hep-th", "hep-ph", "hep-lat"}, Today]
```
which adds the corresponding entries to your reading log. You can
check the status of your reading log via

```
IHEPShowDailyReadingLog[]
```
  
## Filtering
You might want to make sure that preprints containing certain keywords in their description or written by specific people are showed to you first. To that aim you can define your own filtering function.

The following function will select all preprints by `Bodwin`, `Braaten` or `Lepage` containing words `factorization`, `quarkonium`, `quarkonia`, `nonrelativistic`, `charmonium`, `bottomonium`, `NRQCD` in the abstract.
```
mySelector = ! (StringFreeQ[#["Authors"], {"Bodwin", "Braaten", 
        "Lepage" }] && 
      StringFreeQ[#["Abstract"], {"factorization", "quarkonium", 
        "quarkonia", "nonrelativistic", "charmonium", 
        "bottomonium", "NRQCD"}, IgnoreCase -> True]) &;
```
with
```
preprintsReadFirst = newPreprints[Select[mySelector]];
Print["Number of the prioritized preprints: ", Length[preprintsReadFirst]]
preprintsRemaining = newPreprints[Select[(mySelector /. Not[x_] -> x)]];
Print["Number of the remaining preprints: ", Length[preprintsRemaining]]
```
you can save all relevant preprints satisfying the above criteria to
`preprintsReadFirst`, while the remaining preprints will go into `preprintsRemaining`. Then you can just use 

```
IHEPShowPreprints[preprintsReadFirst]
```
and
```
IHEPShowPreprints[preprintsRemaining]
```
as usual.

## Looking for a job

```
<< IHEPTools`
```

```
(*
    Fetch current job offers for postdoc positions in theory and
    phenomenology at institutions in Europe and North America
    from INSPIRE Jobs

*)
jobOffers = IHEPGetJobOffers[{"hep-th", "hep-ph"}, {"Europe", 
   "North America"}, {"POSTDOC"}]
```

```
(*
    Review the existing job offers
*)
IHEPShowJobOffers[jobOffers]
```

## Checking out upcoming conferences

```
<< IHEPTools`
```


```
(*
    Get the list of upcoming conferences from INSPIRE
*)
conferences = IHEPGetUpcomingConfrences[];
```


```
(*
    Select only conferences taking place in the next 6 months
*)
conferencesNext6Months = Select[conferences, 
    DateObject[#[["From"]]] < DatePlus[Now, Quantity[6, "Months"]] &];
```

```
(*
    Review the relevant conferences
*)
IHEPShowConferences[conferencesNext6Months]
```
# License

IHEPTools is covered by the GNU General Public License 3.

Copyright (C) 2020-2022 Vladyslav Shtabovenko

IHEPTools is free software: you can redistribute it and/or modify
it under the terms of the GNU General Public License as
published by the Free Software Foundation, either version 3 of
the License, or (at your option) any later version.

IHEPTools is distributed in the hope that it will be useful,
but WITHOUT ANY WARRANTY; without even the implied warranty of
MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
GNU General Public License for more details.

You should have received a copy of the GNU General Public License
along with IHEPTools.  If not, see <http://www.gnu.org/licenses/>.
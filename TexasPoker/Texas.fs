module Texas

open System
open System.Linq

type Suit = Spade|Heart|Club|Diamond
type Rank = Two|Three|Four|Five|Six|Seven|Eight|Nine|Ten|Jack|Queen|King|Ace
type Card = Rank * Suit  
type Combination = HighCard|OnePair|TwoPair|ThreeSame|Straight|Flush|FullHouse|FourSame|StraightFlush|RoyalStraightFlush

let (|HasOnePair|HasTwoPair|HasThreeSame|HasFullHouse|HasFourSame|HasNothing|) (cards:Set<Card>) =
    let groups = query { for (rank,suit) in cards do 
                            groupBy rank into rg 
                            groupBy (Seq.length rg) into scg
                            select (Seq.length scg, scg.Key, scg |> Seq.map Seq.toList |> Seq.toList)
                            } |> Seq.toList
    match groups with
    | (1,4,_)::[_] -> HasFourSame 
    | (1,3,_)::(1,2,_)::[_] -> HasFullHouse 
    | (1,3,_)::[_] -> HasThreeSame 
    | (1,2,_)::[_] -> HasOnePair 
    | (2,2,_)::[_] -> HasTwoPair
    | _ -> HasNothing

let findBestCombination (cards:Set<Card>) =
    match cards with
    | HasFourSame -> FourSame
    | HasFullHouse -> FullHouse
    | HasThreeSame -> ThreeSame
    | HasOnePair -> OnePair
    | HasTwoPair -> TwoPair
    | _ -> HighCard

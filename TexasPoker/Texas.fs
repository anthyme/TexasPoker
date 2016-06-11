module Texas

type Suit = Spade|Heart|Club|Diamond
type Rank = Two|Three|Four|Five|Six|Seven|Eight|Nine|Ten|Jack|Queen|King|Ace
type Card = Rank * Suit  
type Combination = HighCard|OnePair|TwoPair|ThreeSame|Straight|Flush|FullHouse|FourSame|StraightFlush|RoyalStraightFlush

let parseSuit = function 's' -> Spade | 'h' -> Heart | 'c' -> Club | 'd' -> Diamond | x -> failwith (sprintf "unknown suit %c" x)

let parseRank = function 
    | 'T' -> Ten | 'J' -> Jack | 'Q' -> Queen | 'K' -> King | 'A' -> Ace
    | x when Seq.contains (x |> string |> int) [2..9] -> [Two;Three;Four;Five;Six;Seven;Eight;Nine].[(x |> string |> int) - 2]
    | x -> failwith (sprintf "unknown rank %c" x)
            
let convertCardSet (txt:string) = txt.Split([|' '|]) |> Seq.map (fun x -> parseRank x.[0], parseSuit x.[1]) |> List.ofSeq

let findCombination (cardSet:string) = HighCard




open FsUnit.Xunit
open Xunit

type ``Given a card set`` () =
    let shouldConvertTo result cardSet = cardSet |> convertCardSet |> should equal result
    let shouldMatchCombination result cardSet = cardSet |> findCombination |> should equal result

    [<Fact>] 
    let ``is convertible to model``() = 
        "Ad 2h 3d 8h Qh Tc Kc" |> shouldConvertTo [Ace,Diamond; Two,Heart; Three,Diamond; Eight,Heart; Queen,Heart; Ten,Club; King,Club]

    [<Fact>] let ``is a high card`` ()                      = "Ad 2h 3d 8h Qh Tc Kc" |> shouldMatchCombination HighCard
    [<Fact>] let ``is a one pair`` ()                       = "3d 8h Qh Tc Kc 2d 2h" |> shouldMatchCombination OnePair
    [<Fact>] let ``is a two pair`` ()                       = "2d 2h 3d 3h Qh Tc Kc" |> shouldMatchCombination TwoPair
    [<Fact>] let ``is a three same`` ()                     = "2d 2h 2c 4d 3h Qh Tc" |> shouldMatchCombination ThreeSame
    [<Fact>] let ``is a straight`` ()                       = "2d 3h 4c 5d 6h Qh Tc" |> shouldMatchCombination Straight
    [<Fact>] let ``is a straight starting by ace`` ()       = "Ad 2h 3c 4d 5h Qh Tc" |> shouldMatchCombination Straight
    [<Fact>] let ``is a straight ending by ace`` ()         = "Ad Kh Qc Jd Th Qh Tc" |> shouldMatchCombination Straight
    [<Fact>] let ``is a flush`` ()                          = "2h 5h 3h 8h Qh Tc Kc" |> shouldMatchCombination Flush
    [<Fact>] let ``is a full house`` ()                     = "2d 2h 2c 3d 3h Qh Tc" |> shouldMatchCombination FullHouse
    [<Fact>] let ``is a four same`` ()                      = "2d 2h 2c 2c 3h Qh Tc" |> shouldMatchCombination FourSame
    [<Fact>] let ``is a straight flush`` ()                 = "2h 3h 4h 5h 6h Qh Tc" |> shouldMatchCombination Straight
    [<Fact>] let ``is a straight flush starting by ace`` () = "Ah 2h 3h 4h 5h Qh Tc" |> shouldMatchCombination StraightFlush
    [<Fact>] let ``is a royal straight flush`` ()           = "Ah Kh Qh Jh Th Qh Tc" |> shouldMatchCombination RoyalStraightFlush

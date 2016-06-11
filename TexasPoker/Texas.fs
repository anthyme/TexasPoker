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


open FsUnit.Xunit
open Xunit

type ``Given a card set`` () =
    let shouldConvertTo result cardSet = cardSet |> convertCardSet |> should equal result

    [<Fact>] 
    let ``is convertible to model``() = 
        "Ad 2h 3d 8h Qh Tc Kc" |> shouldConvertTo [Ace,Diamond; Two,Heart; Three,Diamond; Eight,Heart; Queen,Heart; Ten,Club; King,Club]



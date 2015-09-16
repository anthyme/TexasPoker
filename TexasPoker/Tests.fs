module Tests

open FsUnit.Xunit
open Xunit
open Texas

type ``Given a Hand`` () = 
    [<Fact>] 
    let ``is a high card`` () = 
        set [Ace,Diamond; Two,Heart; Three,Diamond; Eight,Heart; Queen,Heart; Ten,Club; King,Club]
        |> findBestCombination |> should equal HighCard
    
    [<Fact>] 
    let ``is a one pair`` () = 
        set [Three,Diamond; Eight,Heart; Queen,Heart; Ten,Club; King,Club;Two,Diamond; Two,Heart;]
        |> findBestCombination |> should equal OnePair

    [<Fact>] 
    let ``is a two pair`` () = 
        set [Two,Diamond; Two,Heart; Three,Diamond; Three,Heart; Queen,Heart; Ten,Club; King,Club]
        |> findBestCombination |> should equal TwoPair

    [<Fact>] 
    let ``is a three same`` () = 
        set [Two,Diamond; Two,Heart; Two,Club; Four,Diamond; Three,Heart; Queen,Heart; Ten,Club]
        |> findBestCombination |> should equal ThreeSame
        
    [<Fact>] 
    let ``is a straight`` () = 
        set [Two,Diamond; Three,Heart; Four,Club; Five,Diamond; Six,Heart; Queen,Heart; Ten,Club]
        |> findBestCombination |> should equal Straight
    
    [<Fact>] 
    let ``is a straight starting by ace`` () = 
        set [Ace,Diamond; Two,Heart; Three,Club; Four,Diamond; Five,Heart; Queen,Heart; Ten,Club]
        |> findBestCombination |> should equal Straight
    
    [<Fact>] 
    let ``is a straight ending by ace`` () = 
        set [Ace,Diamond; King,Heart; Queen,Club; Jack,Diamond; Ten,Heart; Queen,Heart; Ten,Club]
        |> findBestCombination |> should equal Straight
    
    [<Fact>] 
    let ``is a flush`` () = 
        set [Two,Heart; Five,Heart; Three,Heart; Eight,Heart; Queen,Heart; Ten,Club; King,Club]
        |> findBestCombination |> should equal Flush

    [<Fact>] 
    let ``is a full house`` () = 
        set [Two,Diamond; Two,Heart; Two,Club; Three,Diamond; Three,Heart; Queen,Heart; Ten,Club]
        |> findBestCombination |> should equal FullHouse
    
    [<Fact>] 
    let ``is a four same`` () = 
        set [Two,Diamond; Two,Heart; Two,Club; Two,Club; Three,Heart; Queen,Heart; Ten,Club]
        |> findBestCombination |> should equal FourSame

    [<Fact>] 
    let ``is a Straight Flush`` () = 
        set [Two,Diamond; Two,Heart; Two,Club; Two,Club; Three,Heart; Queen,Heart; Ten,Club]
        |> findBestCombination |> should equal FourSame
   
    [<Fact>] 
    let ``is a straight flush`` () = 
        set [Two,Heart; Three,Heart; Four,Heart; Five,Heart; Six,Heart; Queen,Heart; Ten,Club]
        |> findBestCombination |> should equal Straight
    
    [<Fact>] 
    let ``is a straight flush starting by ace`` () = 
        set [Ace,Heart; Two,Heart; Three,Heart; Four,Heart; Five,Heart; Queen,Heart; Ten,Club]
        |> findBestCombination |> should equal Straight
    
    [<Fact>] 
    let ``is a royal straight flush`` () = 
        set [Ace,Heart; King,Heart; Queen,Heart; Jack,Heart; Ten,Heart; Queen,Heart; Ten,Club]
        |> findBestCombination |> should equal Straight

type Combination = HighCard|OnePair|TwoPair|ThreeSame|Straight|Flush|FullHouse|FourSame|StraightFlush

package poker

import utest._

object PokerSuite extends TestSuite:
  val tests = Tests {
    test("royal flush") {
      val rfh = royalFlush(Heart)
      val rfs = royalFlush(Spade)

      RoyalFlush.matching(rfh) ==> true
      RoyalFlush.matching(rfs) ==> true

      HeadsUp(rfh, rfs).winner ==> Draw
    }
    test("straight flush") {
      val lowHeart = straightFlush(Heart, Six)
      val lowDiamond = straightFlush(Diamond, Six)
      val highSpade = straightFlush(Spade, King)

      StraightFlush.matching(lowHeart) ==> true
      StraightFlush.matching(lowDiamond) ==> true
      StraightFlush.matching(highSpade) ==> true

      HeadsUp(lowHeart, lowDiamond).winner ==> Draw
      HeadsUp(lowHeart, highSpade).winner ==> RightWinner
      HeadsUp(highSpade, lowDiamond).winner ==> LeftWinner
    }
    test("4") {
      val top1 = fourOfAKind(Ace, King)
      val top2 = fourOfAKind(Ace, King)
      val topLowHighCard = fourOfAKind(Ace, Two)
      val kings = fourOfAKind(King, Ace)

      FourOfAKind.matching(top1) ==> true

      HeadsUp(top1, top2).winner ==> Draw
      HeadsUp(top1, topLowHighCard).winner ==> LeftWinner
      HeadsUp(topLowHighCard, top1).winner ==> RightWinner

      HeadsUp(kings, top1).winner ==> RightWinner
      HeadsUp(top1, kings).winner ==> LeftWinner
    }
    test("Full house") {
      val top = fullHouse(Ace, King)
      val topLow = fullHouse(Ace, Queen)
      val kingAce = fullHouse(King, Ace)

      FullHouse.matching(top) ==> true
      FullHouse.matching(kingAce) ==> true

      HeadsUp(top, kingAce).winner ==> LeftWinner
      HeadsUp(kingAce, top).winner ==> RightWinner
      HeadsUp(top, topLow).winner ==> LeftWinner
      HeadsUp(topLow, top).winner ==> RightWinner
    }
    test("flush") {
      val flush1 = flush(Heart, Seq(Two, Four, Five, Six, Seven))
      val flushSame = flush(Diamond, Seq(Two, Four, Five, Six, Seven))
      val flushHigher = flush(Diamond, Seq(Two, Four, Ten, Knight, King))
      val flushHigher2 = flush(Diamond, Seq(Two, Four, Knight, Queen, King))

      Flush.matching(flush1) ==> true

      HeadsUp(flush1, flushSame).winner ==> Draw
      HeadsUp(flush1, flushHigher).winner ==> RightWinner
      HeadsUp(flushHigher2, flushHigher).winner ==> LeftWinner
      HeadsUp(flushHigher, flushHigher2).winner ==> RightWinner
    }
    test("Straight") {
      val straightAceHigh = straight(Ace)
      val straightTenHigh = straight(Ten)

      Straight.matching(straightAceHigh) ==> true

      HeadsUp(straightAceHigh, straightAceHigh).winner ==> Draw
      HeadsUp(straightAceHigh, straightTenHigh).winner ==> LeftWinner
      HeadsUp(straightTenHigh, straightAceHigh).winner ==> RightWinner
    }
    test("3") {
      val tripAcesThreeHigh = threeOfAKind(Ace, Two, Three)
      val tripAcesFourHigh = threeOfAKind(Ace, Two, Four)
      val tripKingsAceHigh = threeOfAKind(King, Ace, Queen)

      ThreeOfAKind.matching(tripAcesThreeHigh) ==> true

      HeadsUp(tripAcesThreeHigh, tripAcesThreeHigh).winner ==> Draw

      HeadsUp(tripKingsAceHigh, tripAcesThreeHigh).winner ==> RightWinner
      HeadsUp(tripAcesThreeHigh, tripKingsAceHigh).winner ==> LeftWinner

      HeadsUp(tripAcesThreeHigh, tripAcesFourHigh).winner ==> RightWinner
      HeadsUp(tripAcesFourHigh, tripAcesThreeHigh).winner ==> LeftWinner
    }
    test("2 + 2") {
      val twoPairs1 = twoPairs(Ace, King, Queen)
      val twoPairs1same = twoPairs(King, Ace, Queen)
      val twoPairs1LowKicker = twoPairs(King, Ace, Knight)
      val twoPairs2 = twoPairs(King, Ace, Two)
      val twoPairsLow = twoPairs(Ace, Two, Three)

      TwoPairs.matching(twoPairs1) ==> true

      HeadsUp(twoPairs1, twoPairs1).winner ==> Draw
      HeadsUp(twoPairs1, twoPairs1same).winner ==> Draw
      HeadsUp(twoPairs1, twoPairsLow).winner ==> LeftWinner
      HeadsUp(twoPairsLow, twoPairs1).winner ==> RightWinner
      HeadsUp(twoPairs1, twoPairs1LowKicker).winner ==> LeftWinner
      HeadsUp(twoPairs1LowKicker, twoPairs1).winner ==> RightWinner

      HeadsUp(twoPairs1, twoPairs2).winner ==> LeftWinner
      HeadsUp(twoPairs2, twoPairs1).winner ==> RightWinner
    }
    test("2") {
      val p1 = pair(Ace, Six, Five, Three)
      val p1highKicker = pair(Ace, Six, Five, Four)
      val lowerPair = pair(Two, Ace, King, Queen)
      val left = pair(Five, Seven, Six, Three)
      val right = pair(Five, Knight, Two, Three)

      Pair.matching(p1) ==> true

      HeadsUp(left, right).winner ==> RightWinner
      HeadsUp(right, left).winner ==> LeftWinner

      HeadsUp(p1, p1).winner ==> Draw
      HeadsUp(p1highKicker, p1).winner ==> LeftWinner
      HeadsUp(p1, p1highKicker).winner ==> RightWinner

      HeadsUp(p1highKicker, lowerPair).winner ==> LeftWinner
      HeadsUp(lowerPair, p1highKicker).winner ==> RightWinner
    }
    test("parse 1") {
      val seq = HeadsUp.parseFile(
        os.pwd / "server" / "resources" / "poker" / "hands.txt"
      )
      seq.size ==> 1000
    }
    test("heads up 1") {
      HeadsUp(PAIR, THREE_OF_A_KIND).winner ==> RightWinner
      HeadsUp(THREE_OF_A_KIND, PAIR).winner ==> LeftWinner
    }
    test("examples") {
      val h1 = HeadsUp.parseHand("5H 5C 6S 7S KD 2C 3S 8S 8D TD")
      val h2 = HeadsUp.parseHand("5D 8C 9S JS AC 2C 5C 7D 8S QH")
      val h3 = HeadsUp.parseHand("2D 9C AS AH AC 3D 6D 7D TD QD")
      val h4 = HeadsUp.parseHand("4D 6S 9H QH QC 3D 6D 7H QD QS")
      val h5 = HeadsUp.parseHand("2H 2D 4C 4D 4S 3C 3D 3S 9S 9D")

      h1.winner ==> RightWinner
      h2.winner ==> LeftWinner
      h3.winner ==> RightWinner
      h4.winner ==> LeftWinner
      h5.winner ==> LeftWinner
    }
    test("boom") {
      val h1 = HeadsUp.parseHand("6D 7C 5D 5H 3S 5C JC 2H 5S 3D")

      h1.winner ==> RightWinner
    }
    test("results") {
      val expectedLeft = os.read
        .lines(
          os.pwd / "server" / "resources" / "poker" / "playerOneWinners.txt"
        )
        .map(_.toInt)
      val result = HeadsUp
        .parseFile(
          os.pwd / "server" / "resources" / "poker" / "hands.txt"
        )
        .zipWithIndex
        .map((hu, i) => {
          val result = hu.winner
          result match {
            case LeftWinner if !expectedLeft.contains(i) => println(i)
            case _                                       =>
          }
          result
        })

      val right = result.filter(_ == RightWinner).size
      val draw = result.filter(_ == Draw).size
      val left = result.filter(_ == LeftWinner).size

      right ==> 624
      draw ==> 0
      left ==> 376
    }
  }



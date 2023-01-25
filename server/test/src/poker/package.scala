package object poker {
  def pair(pair: Rank, high1: Rank, high2: Rank, high3: Rank) =
    require(pair != high1, "pair may not be same as highCard numbnuts")
    require(pair != high2, "pair may not be same as highCard numbnuts")
    require(pair != high3, "pair may not be same as highCard numbnuts")

    val ranks = Seq(pair, pair, high1, high2, high3)
    val faces = Face.all :+ Spade
    val cards = ranks.zip(faces).map((r, f) => Card(f, r))

    Hand(cards)

  val PAIR = pair(Ace, Four, Three, Two)

  def twoPairs(pair1: Rank, pair2: Rank, highCard: Rank) =
    require(pair1 != pair2, "pair may not be same as pair2 numbnuts")
    require(pair1 != highCard, "pair may not be same as highCard numbnuts")
    require(pair2 != highCard, "pair may not be same as highCard numbnuts")

    val ranks = Seq(pair1, pair1, pair2, pair2, highCard)
    val faces = Face.all :+ Spade
    val cards = ranks.zip(faces).map((r, f) => Card(f, r))

    Hand(cards)

  val TWO_PAIRS = twoPairs(Ace, Queen, Ten)

  def royalFlush(face: Face) = Hand(
    Seq(
      Card(face, Ace),
      Card(face, King),
      Card(face, Queen),
      Card(face, Knight),
      Card(face, Ten)
    )
  )

  def straight(high: Rank) =
    val faces = Face.all :+ Heart
    val ranks = Rank.all.reverse.dropWhile(r => r != high).take(5)
    val cards = faces.zip(ranks).map((f, r) => Card(f, r))
    Hand(cards)

  def straightFlush(face: Face, high: Rank) =
    val ranks = Rank.all.reverse.dropWhile(r => r != high).take(5)
    Hand(
      ranks
        .map(r => Card(face, r))
    )

  val ROYAL_FLUSH = royalFlush(Heart)

  def threeOfAKind(rank: Rank, highCard1: Rank, highCard2: Rank) =
    require(rank != highCard1)
    require(rank != highCard2)

    val cs = Seq(
      Card(Heart, rank),
      Card(Club, rank),
      Card(Diamond, rank),
      Card(Spade, highCard1),
      Card(Diamond, highCard2)
    )

    Hand(cs)

  val THREE_OF_A_KIND = threeOfAKind(Ace, Ten, Nine)

  val FULL_HOUSE = fullHouse(Ace, Ten)

  def fullHouse(toak: Rank, pair: Rank) =
    require(toak != pair)
    Hand(
      Seq(
        Card(Heart, toak),
        Card(Club, toak),
        Card(Diamond, toak),
        Card(Spade, pair),
        Card(Heart, pair)
      )
    )

  def flush(face: Face, ranks: Seq[Rank]) =
    require(ranks.toSet.size == 5, "Five different ranks required")
    Hand(
      ranks.map(rank => Card(face, rank))
    )

  val FOUR_OF_A_KIND = fourOfAKind(Ace, Ten)

  def fourOfAKind(foak: Rank, highCard: Rank) =
    require(foak != highCard)
    Hand(
      Seq(
        Card(Heart, foak),
        Card(Club, foak),
        Card(Diamond, foak),
        Card(Spade, foak),
        Card(Heart, highCard)
      )
    )
}

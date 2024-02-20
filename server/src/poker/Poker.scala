package poker

case class HeadsUp(leftHand: Hand, rightHand: Hand):
  lazy val left = PokerHand.fromHand(leftHand)
  lazy val right = PokerHand.fromHand(rightHand)
  def winner: Result =
    if left.value < right.value then LeftWinner
    else if left.value > right.value then RightWinner
    else left.high(leftHand, rightHand)

object HeadsUp:
  def parseFile(path: os.Path): Seq[HeadsUp] =
    os.read
      .lines(path)
      .map(parseHand)
  def parseHand(s: String): HeadsUp =
    var cs = s
      .split(" ")
      .map(Card.fromCode)
      .toSeq

    HeadsUp(
      Hand(cs.take(5)),
      Hand(cs.drop(5).take(5))
    )

case class Hand(cards: Seq[Card]):
  require(cards.size == 5, "A hand must contain 5 cards")

  lazy val sorted = cards.sorted
  lazy val highestRank = sorted.last.rank.n

  def nOfAKind(n: Int, howMany: Int = 1) =
    cards
      .groupBy(_.rank.n)
      .values
      .map(_.length)
      .filter(_ == n)
      .toList
      .length == howMany

  def ofAKind(n: Int) =
    val x = cards
      .groupBy(_.rank.n)
      .filter((_, cards) => cards.size == n)

    x.head._2

  def notOfAKind(n: Int): Seq[Card] =
    val x = cards
      .groupBy(_.rank.n)
      .filterNot((_, cards) => cards.size == n)

    x.flatMap(_._2).toSeq.sorted

  def inSequence: Boolean =
    val cs = cards
      .map(c => c.rank.n)
      .toSet
      .toList
      .sorted

    def inSeq = cs
      .sliding(2)
      .forall {
        case List(left, right) => left == right - 1
        case _                 => true
      }

    cs.length == 5 && inSeq

sealed trait PokerHand:
  def matching(hand: Hand): Boolean
  def value = PokerHand.all.zipWithIndex.find((ph, i) => ph == this).get._2
  def high(left: Hand, right: Hand): Result
  def _highCards(left: Seq[Card], right: Seq[Card]): Result =
    _high(left.map(_.rank.n), right.map(_.rank.n))
  def _high(left: Seq[Int], right: Seq[Int]): Result =
    val equal = left
      .zip(right)
      .dropWhile((l, r) => l == r)

    if equal.size == 0 then Draw
    else
      val (l, r) = (equal.head)
      _high(l, r)

  def _high(left: Int, right: Int): Result =
    if left < right then RightWinner
    else if left > right then LeftWinner
    else Draw

  def rank(h: Hand, size: Int) = h.cards
    .groupBy(_.rank.n)
    .values
    .filter(_.size == size)
    .flatten
    .toSet
    .map(_.rank.n)
    .toSeq
    .sorted
    .reverse

object PokerHand:
  val all = Seq(
    RoyalFlush,
    StraightFlush,
    FourOfAKind,
    FullHouse,
    Flush,
    Straight,
    ThreeOfAKind,
    TwoPairs,
    Pair,
    HighCard
  )
  def fromHand(hand: Hand): PokerHand = all.find(_.matching(hand)).get

case object RoyalFlush extends PokerHand:
  def matching(ph: Hand): Boolean =
    StraightFlush.matching(ph) && ph.cards.filter(_.rank == Ace).nonEmpty
  def high(left: Hand, right: Hand) =
    _high(left.highestRank, right.highestRank)

case object StraightFlush extends PokerHand:
  def matching(ph: Hand): Boolean =
    Flush.matching(ph) && Straight.matching(ph)
  def high(left: Hand, right: Hand) =
    _high(left.highestRank, right.highestRank)

case object FourOfAKind extends PokerHand:
  def matching(ph: Hand): Boolean =
    ph.cards.groupBy(_.rank.n).values.map(_.length).filter(_ == 4).nonEmpty
  def high(left: Hand, right: Hand) =
    val l = rank(left, 4) ++ rank(left, 1)
    val r = rank(right, 4) ++ rank(right, 1)
    _high(l, r)

case object FullHouse extends PokerHand:
  def matching(ph: Hand): Boolean =
    ph.nOfAKind(2) && ph.nOfAKind(3)
  def high(left: Hand, right: Hand) =
    val l = rank(left, 3) ++ rank(left, 2)
    val r = rank(right, 3) ++ rank(right, 2)
    _high(l, r)

case object Flush extends PokerHand:
  def matching(ph: Hand): Boolean = ph.cards.groupBy(_.face).size == 1
  def high(left: Hand, right: Hand) =
    _highCards(left.sorted, right.sorted)

case object Straight extends PokerHand:
  def matching(ph: Hand): Boolean = ph.inSequence
  def high(left: Hand, right: Hand) =
    _high(left.highestRank, right.highestRank)

case object ThreeOfAKind extends PokerHand:
  def matching(ph: Hand): Boolean = ph.nOfAKind(3)
  def high(left: Hand, right: Hand) =
    val l = rank(left, 3) ++ rank(left, 1)
    val r = rank(right, 3) ++ rank(right, 1)
    _high(l, r)

case object TwoPairs extends PokerHand:
  def matching(ph: Hand): Boolean = ph.nOfAKind(2, 2)
  def high(left: Hand, right: Hand) =
    val l = rank(left, 2) ++ rank(left, 1)
    val r = rank(right, 2) ++ rank(right, 1)
    _high(l, r)

case object Pair extends PokerHand:
  def matching(ph: Hand): Boolean = ph.nOfAKind(2)
  def high(left: Hand, right: Hand) =
    val l = rank(left, 2) ++ rank(left, 1)
    val r = rank(right, 2) ++ rank(right, 1)
    _high(l, r)

case object HighCard extends PokerHand:
  def matching(hand: Hand): Boolean = true
  def high(left: Hand, right: Hand) =
    _high(left.highestRank, right.highestRank)

sealed trait Result
case object LeftWinner extends Result
case object Draw extends Result
case object RightWinner extends Result

case class Card(face: Face, rank: Rank)
object Card {
  implicit val ord: Ordering[Card] = Ordering.by(_.rank.n)
  def fromCode(code: String): Card =
    val (rankCode, faceCode) = (code(0), code(1))
    val face = Face.fromCode(faceCode)
    val rank = Rank.fromCode(rankCode)
    Card(face, rank)
}

sealed trait Face(val code: Char)
object Heart extends Face('H')
object Diamond extends Face('D')
object Club extends Face('C')
object Spade extends Face('S')
object Face:
  val all = Seq(Heart, Diamond, Club, Spade)
  def fromCode(code: Char): Face = all.find(_.code == code) match {
    case Some(c) => c
    case _ => throw RuntimeException(s"Unable to parse Face from code: $code")
  }

sealed trait Rank(val n: Int, val code: Char)
object Rank:
  val all = Seq(
    Two,
    Three,
    Four,
    Five,
    Six,
    Seven,
    Eight,
    Nine,
    Ten,
    Knight,
    Queen,
    King,
    Ace
  )
  def fromCode(code: Char) = all.find(_.code == code) match {
    case Some(r) => r
    case _ => throw RuntimeException(s"Unable to parse Rank from code: $code")
  }
case object Two extends Rank(2, '2')
case object Three extends Rank(3, '3')
case object Four extends Rank(4, '4')
case object Five extends Rank(5, '5')
case object Six extends Rank(6, '6')
case object Seven extends Rank(7, '7')
case object Eight extends Rank(8, '8')
case object Nine extends Rank(9, '9')
case object Ten extends Rank(10, 'T')
case object Knight extends Rank(11, 'J')
case object Queen extends Rank(12, 'Q')
case object King extends Rank(13, 'K')
case object Ace extends Rank(14, 'A')

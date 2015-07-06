package main.scala.sets

trait _Set

trait PureSet extends _Set {
  def map(f: PureSet => PureSet): PureSet

  def isEmpty: Boolean
  def size: Int
  def cardinality: Int
  def members: List[PureSet]
  def is(s: PureSet): Boolean
  def hasMember(el: PureSet): Boolean
  def hasSubset(s: PureSet): Boolean
  def isSubsetOf(s: PureSet): Boolean
  def isProperSubsetOf(s: PureSet): Boolean
  def powerSet: PureSet
  def unionWith(s: PureSet): PureSet
  def union: PureSet
  def intersectionWith(s: PureSet): PureSet
  def intersection: PureSet
  def relativeComplementIn(b: PureSet): PureSet

  def reachIn(i: Int): PureSet  //Order doesn't matter, so this operation is theoretically non-deterministic
  def largest: PureSet

  def listMem(): Unit
}

object PureSet {
  def apply(): PureSet = new EmptyPureSet
  def apply(l: List[PureSet]): PureSet = if (l.isEmpty) new EmptyPureSet else new NonEmptyPureSet(l)

  def pair(s: PureSet, t: PureSet): PureSet = PureSet(List(s, t))
  def unit(s: PureSet): PureSet = pair(s, s)

  def oPair(s: PureSet, t: PureSet): PureSet = PureSet(List(s, PureSet(List(s, t))))
  def oTuple(l: List[PureSet]): PureSet = l.reduceLeft((s, t) => {
    PureSet.oPair(s, t)
  })  //But how can we unwind this, given that the first set in the tuple might have a size > 1?
  def fst(p: PureSet): PureSet = {
    val s = p.reachIn(0)
    val t = p.reachIn(1)
    if (s.isSubsetOf(t)) s else t //This assumes we have an oPair
  }
  def snd(p: PureSet): PureSet = {
    val s = p.reachIn(0)
    val t = p.reachIn(1)
    if (s.isSubsetOf(t)) t else s //This assumes we have an oPair
  }

  val zero = new EmptyPureSet
  def succ(n: PureSet): PureSet = n.unionWith(PureSet.unit(n))
  def pred(n: PureSet): PureSet = n.relativeComplementIn(n.largest)
  def plus(n1: PureSet, n2: PureSet): PureSet = n2 match {
    case z if z.is(zero) => n1
    case _ => succ(plus(n1, pred(n2)))
  }
  def times(n1: PureSet, n2: PureSet): PureSet = n2 match {
    case z if z.is(zero) => zero
    case _ => plus(n1, times(n1, pred(n2)))
  }
  def minus(n1: PureSet, n2: PureSet): PureSet = n2 match {
    case z if z.is(zero) => n1
    case _ => pred(minus(n1, pred(n2)))
  }
  def gt(n1: PureSet, n2: PureSet): Boolean = n2.isProperSubsetOf(n1)
  def lt(n1: PureSet, n2: PureSet): Boolean = n1.isProperSubsetOf(n2)
  def gte(n1: PureSet, n2: PureSet): Boolean = n2.isSubsetOf(n1)
  def lte(n1: PureSet, n2: PureSet): Boolean = n1.isSubsetOf(n2)
  def eq(n1: PureSet, n2: PureSet): Boolean = n1.is(n2)

  def deduplicate(l: List[PureSet]) = l.foldLeft(List[PureSet]())((acc: List[PureSet], x: PureSet) => {
    if (acc.exists((s: PureSet) => s.is(x))) acc else acc :+ x
  })

  def withMember(s: PureSet) = unit(s)
}

case class EmptyPureSet extends PureSet {
  def map(f: PureSet => PureSet): PureSet = this

  def isEmpty: Boolean = true
  def size: Int = 0
  def cardinality: Int = size
  def members: List[PureSet] = Nil
  def is(s: PureSet): Boolean = s.isEmpty
  def hasMember(el: PureSet): Boolean = false
  def hasSubset(s: PureSet): Boolean = false
  def isSubsetOf(s: PureSet): Boolean = true
  def isProperSubsetOf(s: PureSet): Boolean = !s.isEmpty
  def powerSet: PureSet = PureSet.unit(this)
  def unionWith(s: PureSet): PureSet = s
  def union: PureSet = this
  def intersectionWith(s: PureSet): PureSet = this
  def intersection: PureSet = this
  def relativeComplementIn(b: PureSet): PureSet = this

  def reachIn(i: Int): PureSet = this
  def largest: PureSet = this

  override def toString: String = "∅"
  def listMem(): Unit = println("")
}

case class NonEmptyPureSet(l: List[PureSet]) extends PureSet {
  val ms: List[PureSet] = PureSet.deduplicate(l).sortBy(_.size)

  def map(f: PureSet => PureSet): PureSet = PureSet(ms.map(f))

  def isEmpty: Boolean = false
  def size: Int = ms.size
  def cardinality: Int = size
  def members: List[PureSet] = ms
  def is(s: PureSet): Boolean = this.isSubsetOf(s) && s.size == size
  def hasSubset(s: PureSet): Boolean = s.isSubsetOf(this)
  def isSubsetOf(s: PureSet): Boolean = ms.forall(s.hasMember _)
  def isProperSubsetOf(s: PureSet): Boolean = this.isSubsetOf(s) && !s.isSubsetOf(this)
  def hasMember(el: PureSet): Boolean = ms.exists((m: PureSet) => m.is(el))
  def powerSet: PureSet = pureSetCombinations(ms)
  def unionWith(s: PureSet): PureSet = PureSet(ms ++ s.members)
  def union: PureSet = ms.foldLeft(PureSet())((acc: PureSet, x: PureSet) => acc.unionWith(x))
  def intersectionWith(s: PureSet): PureSet = {
    ms.foldLeft(PureSet())((acc: PureSet, x: PureSet) => x match {
      case a if s.hasMember(a) => acc.unionWith(PureSet.unit(a))
      case _ => acc
    })
  }
  def intersection: PureSet = ms.foldLeft(PureSet())((acc: PureSet, x: PureSet) => acc.intersectionWith(x))
  def relativeComplementIn(b: PureSet): PureSet = {
    ms.foldLeft(PureSet())((acc: PureSet, x: PureSet) => x match {
      case a if !b.hasMember(a) => acc.unionWith(PureSet.unit(a))
      case _ => acc
    })
  }

  def reachIn(i: Int): PureSet = ms(i) //Order doesn't matter, so this operation is theoretically non-deterministic
  def largest: PureSet = ms.sortBy(_.size).reverse.head

  override def toString: String = "{" + membersToString + "}"
  def listMem(): Unit = for (el <- ms) println(el)

  private def membersToString: String = ms.foldLeft("")((acc, x) => acc + x.toString + ", ").dropRight(2)

  private def pureSetCombinations(l: List[PureSet]): PureSet = {
    def prefix(el: PureSet, ss: List[PureSet]): List[PureSet] = {
      ss.map(PureSet.unit _).map(_.unionWith(PureSet.unit(el))) :+ el
    }

    def naryGroups(lst: List[PureSet], n: Int): List[PureSet] = {
      def loop(l: List[PureSet], acc: List[PureSet]): List[PureSet] = {
        l match {
          case Nil => acc
          case hd :: tl if n == 1 => prefix(hd, tl)
          case h :: t => prefix(h, naryGroups(t, n - 1)) ++ naryGroups(t, n)
        }
      }

      loop(lst, Nil)
    }

    def combinations(ms: List[PureSet]): PureSet = {
      def loop(l: List[PureSet], acc: List[PureSet], k: Int): List[PureSet] = {
        k match {
          case x if x > ms.size => acc
          case 0 => loop(ms, acc :+ PureSet(), 1)
          case 1 => {
            val singles = ms ++ ms.map(PureSet.unit _)
            loop(ms, acc ++ singles, 2)
          }
          case n => loop(ms, acc ++ naryGroups(ms, n), k + 1)
        }
      }

      val members = loop(ms, Nil, 0)

      PureSet(members)
    }

    combinations(l)
  }
}





//ASSERTIONS

//val p0 = PureSet()
//val p1 = p0.powerSet
//val p2 = p1.powerSet
//val p3 = p2.powerSet
//val p4 = p3.powerSet
////val p5 = p4.powerSet
//
//assert(p0.size == 0)
//assert(p1.size == Math.pow(2, 0))
//assert(p2.size == Math.pow(2, 1))
//assert(p3.size == Math.pow(2, 2))
//assert(p4.size == Math.pow(2, 4))
////  assert(p5.size == Math.pow(2, 16))
//
//val zero = PureSet.zero
//val one = PureSet.succ(zero)
//val two = PureSet.succ(one)
//val three = PureSet.succ(two)
//val four = PureSet.succ(three)
//val five = PureSet.succ(four)
//val six = PureSet.succ(five)
//val seven = PureSet.succ(six)
//val eight = PureSet.succ(seven)
//val nine = PureSet.succ(eight)
//val ten = PureSet.succ(nine)

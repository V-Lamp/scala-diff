package vlamp.scalaDiff

import vlamp.scalaDiff.Delta.{Added, Modified, Removed, Unchanged}

import scala.collection.generic.CanBuildFrom
import scala.collection.{SeqLike, TraversableLike}
//
//sealed trait Expr
//object Expr {
//  trait Literal extends Expr
//  case class Number(double: Double) extends Literal
//  case class Variable(symbol: String) extends Literal
//
//  trait Func extends Expr
//  trait UnaryF extends Func {
//    val inner: Expr
//  }
//
//  case class Minus(inner: Expr) extends UnaryF
//  case class Abs(inner: Expr) extends UnaryF
//  case class Sqrt(inner: Expr) extends UnaryF
//  case class Square(inner: Expr) extends UnaryF
//
//  trait BinaryF extends Func {
//    val left: Expr
//    val right: Expr
//  }
//  case class Add(left: Expr, right: Expr) extends BinaryF
//  case class Subtract(left: Expr, right: Expr) extends BinaryF
//  case class Multiply(left: Expr, right: Expr) extends BinaryF
//  case class Divide(left: Expr, right: Expr) extends BinaryF
//}
//
//import Expr._
//
//trait Eq[T] {
//  def apply(first: T, second: T): Boolean
//}
//object Eq {
//  def makeFor[T](func: (T, T) => Boolean): Eq[T] = new Eq[T] {
//    override def apply(first: T, second: T): Boolean = func(first, second)
//  }
//  def apply[T: Eq]: Eq[T] = implicitly[Eq[T]]
//  def apply[T: Eq](first: T, second: T): Boolean = implicitly[Eq[T]].apply(first, second)
//
//  val swapedAdd = Eq.makeFor[Add]((firstAdd, secondAdd) => firstAdd.)
//
//}
//trait DefaultEqTypeClasses {
//  val exactMatch = Eq.makeFor[Expr]((a, b) => a == b)
//}
//object Calculator {
//  type Id[A] = A
//
//  object Simplifications {
//    def equalMinus = {
//    }
//  }
//}

sealed trait Delta[T] {
  def isChange: Boolean = this match {
    case Unchanged(value) => false
    case _ => true
  }
}

object Delta {
  case class Added[T](value: T) extends Delta[T]
  case class Removed[T](value: T) extends Delta[T]
  case class Unchanged[T](value: T) extends Delta[T]
  case class Modified[T](first: T, second: T) extends Delta[T]
}

trait Diff[T, D] {
  def apply(first: T, second: T): D
  def recoverFirst(second: T, delta: D): T
  def recoverSecond(first: T, delta: D): T
  def hasChanges(delta:D): Boolean
}

object Diff extends DiffTypeClassInstances{
  def apply[T, D](implicit diff: Diff[T, D]) = diff
  def apply[T, D](first: T, second: T)(implicit diff: Diff[T, D]): D = diff(first, second)
}
trait DiffTypeClassInstances extends LowPriorityDiffRes{
  implicit def setDiff[T]: Diff[Set[T], Set[Delta[T]]] = new Diff[Set[T], Set[Delta[T]]] {
    override def apply(first: Set[T], second: Set[T]): Set[Delta[T]] = {
      val unchangedAndRemoved: Set[Delta[T]] = first.map(e => if (second.contains(e)) Unchanged(e) else Removed(e))
      val added = second.diff(first).map(Added(_))
      unchangedAndRemoved ++ added
    }
    override def recoverFirst(second: Set[T], delta: Set[Delta[T]]): Set[T] = {
      val removed = delta.collect{ case Removed(v) => v}
      val added = delta.collect{ case Added(v) => v}
      val modified = delta.collect{ case Modified(a,b) => (a,b)}
      second.diff(added ++ modified.map(_._2)) ++ removed ++ modified.map(_._1)
    }
    override def recoverSecond(first: Set[T], delta: Set[Delta[T]]): Set[T] = {
      val removed = delta.collect{ case Removed(v) => v}
      val added = delta.collect{ case Added(v) => v}
      val modified = delta.collect{ case Modified(a,b) => (a,b)}
      first.diff(removed ++ modified.map(_._1)) ++ added ++ modified.map(_._2)
    }
    override def hasChanges(delta: Set[Delta[T]]): Boolean = delta.exists(_.isChange)
  }
  implicit def mapDiff[K, V, KD, VD](implicit valueDiff: Diff[V, VD]) = new Diff[Map[K, V], Map[Delta[K], VD]] {
    override def apply(first: Map[K, V], second: Map[K, V]): Map[Delta[K], VD] = ???
    override def recoverFirst(second: Map[K, V], delta: Map[Delta[K], VD]): Map[K, V] = ???
    override def recoverSecond(first: Map[K, V], delta: Map[Delta[K], VD]): Map[K, V] = ???
    override def hasChanges(delta: Map[Delta[K], VD]): Boolean = ???
  }
  implicit def optionDiff[T, D](implicit diff: Diff[T, D]): Diff[Option[T], Option[Delta[T]]] = new Diff[Option[T], Option[Delta[T]]] {
    override def apply(first: Option[T], second: Option[T]): Option[Delta[T]] = (first,second) match {
      case (Some(a), Some(b)) =>
        val delta = diff.apply(a,b)
        Some(Modified(a,b))
      case (Some(a), None) => Some(Removed(a))
      case (None, Some(b)) => Some(Added(b))
      case (None, None) => None
    }
    override def recoverFirst(second: Option[T], delta: Option[Delta[T]]): Option[T] = delta match {
      case Some(Modified(a,b)) => Some(a)
      case Some(Removed(a)) => Some(a)
      case Some(Added(b)) => None
      case None => None
    }
    override def recoverSecond(first: Option[T], delta: Option[Delta[T]]): Option[T] = delta match {
      case Some(Modified(a,b)) => Some(b)
      case Some(Added(b)) => Some(b)
      case Some(Removed(a)) => None
      case None => None
    }
    override def hasChanges(delta: Option[Delta[T]]): Boolean = delta.fold(false)(_.isChange)
  }
  
  implicit def tuple2Diff[A, B, DA, DB](implicit aDiff: Diff[A, DA], bDiff: Diff[B, DB]): Diff[(A, B), (DA, DB)] = new Diff[(A, B), (DA, DB)] {
    override def apply(first: (A, B), second: (A, B)): (DA, DB) = (aDiff(first._1, second._1), bDiff(first._2, second._2))
    override def recoverFirst(second: (A, B), delta: (DA, DB)): (A, B) = (aDiff.recoverFirst(second._1, delta._1), bDiff.recoverFirst(second._2, delta._2))
    override def recoverSecond(first: (A, B), delta: (DA, DB)): (A, B) = (aDiff.recoverSecond(first._1, delta._1), bDiff.recoverSecond(first._2, delta._2))
    override def hasChanges(delta: (DA, DB)): Boolean = aDiff.hasChanges(delta._1) || bDiff.hasChanges(delta._2)
  }
}
trait LowPriorityDiffRes {
  implicit def defaultPairDiff[T](implicit equiv: Equiv[T]) = new Diff[T, Delta[T]] {
    override def apply(first: T, second: T): Delta[T] = if (equiv.equiv(first, second)) Unchanged(first) else Modified(first, second)
    override def recoverFirst(second: T, delta: Delta[T]): T = delta match {
      case Unchanged(value) => value
      case Modified(first, second) => first
      case _ => ???
    }
    override def recoverSecond(first: T, delta: Delta[T]): T = delta match {
      case Unchanged(value) => value
      case Modified(first, second) => second
      case _ => ???
    }
    override def hasChanges(delta: Delta[T]): Boolean = delta.isChange
  }
}

//sealed trait Node
//
//object Node {
//  case object Empty extends Node
//  case class IntN(value: Int) extends Node
//  case class StringN(value: String) extends Node
//  case class SeqN(seq: Seq[Node]) extends Node
//  case class MapN(m: Map[Node, Node]) extends Node
//  case class ObjectN(nameValue: collection.Map[String, Node])
//}
//trait LowerImportanceImplicits extends TypeClassCompanion[ToNode] {
//  object typeClass extends TypeClass[ToNode] {
//    override def coproduct[L, R <: Coproduct](cl: => ToNode[L], cr: => ToNode[R]): ToNode[:+:[L, R]] = ???
//
//    override def emptyCoproduct: ToNode[CNil] = ???
//
//    override def product[H, T <: HList](ch: ToNode[H], ct: ToNode[T]): ToNode[::[H, T]] = ???
//
//    override def emptyProduct: ToNode[HNil] = ???
//
//    override def project[F, G](instance: => ToNode[G], to: (F) => G, from: (G) => F): ToNode[F] = ???
//  }
//}
//trait ToNode[T] {
//  def apply(value: T): Node
//}
//object ToNode {
//
//  import Node._
//
//  implicit object intToNode extends ToNode[Int] {
//    override def apply(value: Int): Node = IntN(value)
//  }
//  implicit object stringToNode extends ToNode[String] {
//    override def apply(value: String): Node = StringN(value)
//  }
//  implicit def seqToNode[C[X] <: SeqLike[X, C[X]], A](implicit toNode: ToNode[A]): ToNode[C[A]] = new ToNode[C[A]] {
//
//    override def apply(value: C[A]): Node = SeqN(value.map(toNode(_)).toSeq)
//
//  }
//  implicit def mapToNode[K, V](implicit toNodeK: ToNode[K], toNodeV: ToNode[V]): ToNode[Map[K, V]] = new ToNode[Map[K, V]] {
//    override def apply(value: Map[K, V]): Node = MapN(value.map { case (k, v) => (toNodeK(k), toNodeV(v)) })
//  }
//
//  def apply[T](value: T)(implicit toNode: ToNode[T]) = toNode(value)
//}

package examples.pickle 

import org.scalatest.FlatSpec
import org.scalatest.matchers.ShouldMatchers

/*
 * (sbt)> test-only examples.pickle.TestPickle
 */
class TestPickle extends FlatSpec with ShouldMatchers { 
  import scala.io.BytePickle._ 

  abstract class Term // term of lambda calculus
  case class Var(s: String) extends Term // variable
  case class Lam(s: String, t: Term) extends Term // lambda abstraction
  case class App(t1: Term, t2: Option[Term] = None) extends Term // function application
  object App2 { def apply(t1: Term, t2: Term) = App(t1, Some(t2)) }

  implicit def wrapByteArray(bytes: Array[Byte]) =
    new AnyRef { def hex: String = bytes.foldLeft("0x")((x, b) => x + "%02X".format(b & 0xFF)) }

  """The maybe pickler""" should "be implemented as Option[T] pickler." in {
    val ptn = App(Lam("x", Var("hoge")), Some(Var("moge")))
    println(pickle(termPickler, ptn).hex)
    ptn should be === unpickle(termPickler, pickle(termPickler, ptn))
    val ptn2 = App(Lam("x", Var("hoge")))
    println(pickle(termPickler, ptn2).hex)
    ptn2 should be === unpickle(termPickler, pickle(termPickler, ptn2))
  }

  """The pickled value""" should "be equal to unpicked one." in {
    val x = Var("x") 
    val i = Lam("x", x) 
    val k = Lam("x", Lam("y", x))
    val kki = App2(k, App2(k, i)) 
    println(pickle(termPickler, kki).hex)
    kki should be === unpickle(termPickler, pickle(termPickler, kki))
 }

  /* maybe pickler
   * http://research.microsoft.com/en-us/um/people/akenn/fun/picklercombinators.pdf
   *  pMaybe :: PU a -> PU (Maybe a)
   *  pMaybe pa = alt tag [lift Nothing, wrap (Just, fromJust) pa]
   *              where tag Nothing = 0; tag (Just x) = 1
   */
  implicit def optPickler[T: SPU]: SPU[Option[T]] =
    data(
      (t: Option[T]) => t match {
        case None => 0
        case Some(_) => 1
      },
      List(() => nonePickler[T], () => somePickler[T])
    )
  def nonePickler[T]: SPU[Option[T]] = lift(None)
  def somePickler[T: SPU]: SPU[Option[T]] =
    wrap[T, Option[T]]((x: T) => Option(x), x => x.get, implicitly[SPU[T]])

  implicit def termPickler: SPU[Term] =
    data(
      (t: Term) => t match {
        case Var(_) => 0
        case Lam(_,_) => 1
        case App(_,_) => 2
      },
      List(()=>varPickler, ()=>lamPickler, ()=>appPickler)
    )
  def varPickler: SPU[Term] = 
    wrap(Var, (t: Term) => t match { case Var(x) => x }, string)
  def lamPickler: SPU[Term] = 
    wrap((p: Pair[String,Term]) => Lam(p._1, p._2),
         (t: Term) => t match { case Lam(s, t) => Pair(s, t) },
         pair(string, termPickler))
  def appPickler: SPU[Term] = 
    wrap((p: Pair[Term, Option[Term]]) => App(p._1, p._2),
         (t: Term) => t match { case App(t1, t2) => Pair(t1, t2) },
         pair(termPickler, optPickler[Term]))
}

/*
 * def packInt3(i: int, j: int, k: int): SPU[Tuple3[int]] = wrap(packInt3(i, j, k), unpackInt3(i, j, k), byte) 
 * def packInt3(i: int, j: int, k: int)(b: byte): Tuple3[int] = // produce tuple out of byte 
 * def unpackInt3(i: int, j: int, k: int)(t: Tuple3[int]): byte = // produce byte out of tuple 
 */

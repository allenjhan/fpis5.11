object StreamUnfoldTest extends App{
  val myStream = Stream.unfold(0)(s => Some(s+1, s+1))

  println(myStream.take(10).toList)

}


import Stream._
trait Stream[+A] {

  def toList: List[A] = this match {
    case Empty => Nil
    case Cons(h, t) => h() :: t().toList
  }

  def foldRight[B](z: => B)(f: (A, => B) => B): B = // The arrow `=>` in front of the argument type `B` means that the function `f` takes its second argument by name and may choose not to evaluate it.
    this match {
      case Cons(h,t) => f(h(), t().foldRight(z)(f)) // If `f` doesn't evaluate its second argument, the recursion never occurs.
      case _ => z
    }

  def exists(p: A => Boolean): Boolean =
    foldRight(false)((a, b) => p(a) || b) // Here `b` is the unevaluated recursive step that folds the tail of the stream. If `p(a)` returns `true`, `b` will never be evaluated and the computation terminates early.

  @annotation.tailrec
  final def find(f: A => Boolean): Option[A] = this match {
    case Empty => None
    case Cons(h, t) => if (f(h())) Some(h()) else t().find(f)
  }
  def take(n: Int): Stream[A] = this match {
    case Empty => Empty
    case Cons(h, t) if n == 0 => Empty
    case Cons(h, t) => Cons(h, () => t().take(n-1))
  }

  def drop(n: Int): Stream[A] = this match {
    case Empty => Empty
    case Cons(h, t) if n == 0 => t()
    case Cons(h, t) => t().drop(n-1)
  }

  def takeWhile(p: A => Boolean): Stream[A] = {
    this.foldRight(Empty: Stream[A]){ (e, acc) =>
      if (p(e)) Cons(()=>e, ()=>acc) else Empty
    }
  }

  def forAll(p: A => Boolean): Boolean = {
    this.foldRight(true){ (e, acc) =>
      acc && p(e)
    }
  }

  def headOption: Option[A] = {
    this.foldRight(None: Option[A]){(e, acc) =>
      Some(e)
    }
  }

  def map[B](f: A => B): Stream[B] = this.foldRight(Empty:Stream[B])((e, acc)=>Cons(()=>f(e), ()=>acc))

  def filter(f: A => Boolean): Stream[A] = this.foldRight(Empty:Stream[A])((e, acc)=>if(f(e)) Cons(()=>e, ()=>acc) else acc)

  def flatMap[B](f: A => Stream[B]): Stream[B] = this.foldRight(Empty:Stream[B]){(e, acc)=>
    f(e).foldRight(acc)((e1, acc1)=>Cons(()=>e1,()=>acc1))
  }

  // 5.7 map, filter, append, flatmap using foldRight. Part of the exercise is
  // writing your own function signatures.

  def startsWith[B](s: Stream[B]): Boolean = ???
}
case object Empty extends Stream[Nothing]
case class Cons[+A](h: () => A, t: () => Stream[A]) extends Stream[A]

object Stream {
  def cons[A](hd: => A, tl: => Stream[A]): Stream[A] = {
    lazy val head = hd
    lazy val tail = tl
    Cons(() => head, () => tail)
  }

  def empty[A]: Stream[A] = Empty

  def apply[A](as: A*): Stream[A] =
    if (as.isEmpty) empty
    else cons(as.head, apply(as.tail: _*))

  val ones: Stream[Int] = Stream.cons(1, ones)
  def constant(n: Int): Stream[Int] = Stream.cons(n, constant(n))
  def from(n: Int): Stream[Int] = Stream.cons(n, from(n+1))
  def fibs: Stream[Int] = {
    def fib(n: Int) = {
      def inner(k1: Tuple2[Int, Int], k2: Tuple2[Int, Int]): Int = {
        if (k2._1 == n) k2._2
        else inner(k2, (k2._1+1, k1._2 + k2._2))
      }
      if (n==0) 0
      else if (n==1) 1
      else inner((0,0), (1, 1))
    }
    from(0).map(fib)
  }

  def unfold[A, S](z: S)(f: S => Option[(A, S)]): Stream[A] = f(z) match {
    case Some(v) => Cons(()=>v._1, ()=>unfold(v._2)(f))
    case None => Empty
  }
}

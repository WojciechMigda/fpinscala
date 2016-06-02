package fpinscala.datastructures

sealed trait List[+A] // `List` data type, parameterized on a type, `A`
case object Nil extends List[Nothing] // A `List` data constructor representing the empty list
/* Another data constructor, representing nonempty lists. Note that `tail` is another `List[A]`,
which may be `Nil` or another `Cons`.
 */
case class Cons[+A](head: A, tail: List[A]) extends List[A]

object List
{ // `List` companion object. Contains functions for creating and working with lists.
    def sum(ints: List[Int]): Int = ints match
    { // A function that uses pattern matching to add up a list of integers
        case Nil => 0 // The sum of the empty list is 0.
        case Cons(x, xs) => x + sum(xs) // The sum of a list starting with `x` is `x` plus the sum of the rest of the list.
    }

    def product(ds: List[Double]): Double = ds match
    {
        case Nil => 1.0
        case Cons(0.0, _) => 0.0
        case Cons(x, xs) => x * product(xs)
    }

    def apply[A](as: A*): List[A] = // Variadic function syntax
        if (as.isEmpty) Nil
        else Cons(as.head, apply(as.tail: _*))

    val x = List(1, 2, 3, 4, 5) match
    {
        case Cons(x, Cons(2, Cons(4, _))) => x
        case Nil => 42
        case Cons(x, Cons(y, Cons(3, Cons(4, _)))) => x + y
        case Cons(h, t) => h + sum(t)
        case _ => 101
    }

    def append[A](a1: List[A], a2: List[A]): List[A] =
        a1 match {
            case Nil => a2
            case Cons(h, t) => Cons(h, append(t, a2))
        }

    def foldRight[A, B](as: List[A], z: B)(f: (A, B) => B): B = // Utility functions
        as match
        {
            case Nil => z
            case Cons(x, xs) => f(x, foldRight(xs, z)(f))
        }

    def sum2(ns: List[Int]) =
        foldRight(ns, 0)((x, y) => x + y)

    def product2(ns: List[Double]) =
        foldRight(ns, 1.0)(_ * _) // `_ * _` is more concise notation for `(x,y) => x * y`; see sidebar

    def tail[A](l: List[A]): List[A] =
        l match
        {
            case Nil => l
            case Cons(h, t) => t
        }

    def setHead[A](l: List[A], h: A): List[A] =
        l match
        {
            case Nil => l
            case Cons(lh, lt) => Cons(h, lt)
        }

    def drop[A](l: List[A], n: Int): List[A] =
//        l match
//        {
//            case Nil => Nil
//            case _ =>
                n match
                {
                    case 0 => l
                    case _ => drop(tail(l), n - 1)
                }
//        }

    def dropWhile[A](l: List[A], f: A => Boolean): List[A] =
        l match
        {
            case Nil => Nil
            case Cons(h, t) =>
                if (f(h)) dropWhile(t, f)
                else l
        }

    def init[A](l: List[A]): List[A] =
        l match
        {
            case Nil => Nil
            case Cons(h, Nil) => Nil
            case Cons(h, t) => Cons(h, init(t))
        }

    def length[A](l: List[A]): Int = foldRight(l, 0)((_, a) => a + 1)

    def foldLeft[A, B](l: List[A], z: B)(f: (B, A) => B): B =
        l match
        {
            case Nil => z
            case Cons(h, t) => foldLeft(t, f(z, h))(f)
        }

    def sum3(l: List[Int]) = foldLeft(l, 0)(_ + _)
    def product3(l: List[Double]) = foldLeft(l, 1.0)(_ * _)
    def length2[A](l: List[A]) = foldLeft(l, 0)((a, _) => a + 1)

    def reverse[A](l: List[A]) = foldLeft(l, Nil:List[A])((xs, x) => Cons(x, xs))

    def foldLeft2[A, B](l: List[A], z: B)(f: (B, A) => B): B =
        foldRight(l, z)((a, b) => f(b, a))

    def foldRight2[A, B](l: List[A], z: B)(f: (A, B) => B): B =
        foldLeft(l, z)((b, a) => f(a, b))

    def append2[A](lhs: List[A], rhs: List[A]): List[A] =
        foldLeft(reverse(lhs), rhs)((xs, x) => Cons(x, xs))

    def map[A, B](l: List[A])(f: A => B): List[B] = sys.error("todo")
}

object TestList
{

    import List._

    // test implementation of `isSorted`
    def main(args: Array[String]): Unit =
    {
        println("[tail] Expected: Nil")
        println("[tail] Actual: %s\n".format(tail(Nil)))

        println("[tail] Expected: Cons(2,Cons(3,Nil))")
        println("[tail] Actual: %s\n".format(tail(List(1, 2, 3))))

        println("[setHead] Expected: Nil")
        println("[setHead] Actual: %s\n".format(setHead(Nil, 2)))

        println("[setHead] Expected: Cons(4,Cons(2,Nil))")
        println("[setHead] Actual: %s\n".format(setHead(List(1, 2), 4)))

        println("[drop] Expected: Nil")
        println("[drop] Actual: %s\n".format(drop(Nil, 0)))

        println("[drop] Expected: Nil")
        println("[drop] Actual: %s\n".format(drop(Nil, 1)))

        println("[drop] Expected: Cons(1,Cons(2,Cons(3,Nil)))")
        println("[drop] Actual: %s\n".format(drop(List(1, 2, 3), 0)))

        println("[drop] Expected: Cons(3,Nil)")
        println("[drop] Actual: %s\n".format(drop(List(1, 2, 3), 2)))

        println("[dropWhile] Expected: Nil")
        println("[dropWhile] Actual: %s\n".format(dropWhile(Nil, (h: Int) => h < 2)))

        println("[dropWhile] Expected: Cons(2,Cons(3,Nil))")
        println("[dropWhile] Actual: %s\n".format(dropWhile(List(1, 2, 3), (h: Int) => h < 2)))

        println("[dropWhile] Expected: Cons(2,Cons(3,Nil))")
        println("[dropWhile] Actual: %s\n".format(dropWhile(List(0, 1, 2, 3), (h: Int) => h < 2)))

        println("[init] Expected: Nil")
        println("[init] Actual: %s\n".format(init(Nil)))

        println("[init] Expected: Nil")
        println("[init] Actual: %s\n".format(init(List(1))))

        println("[init] Expected: Cons(1,Nil)")
        println("[init] Actual: %s\n".format(init(List(1, 2))))

        println("[init] Expected: Cons(1,Cons(2,Nil))")
        println("[init] Actual: %s\n".format(init(List(1, 2, 3))))

        println("[length] Expected: 0")
        println("[length] Actual: %d\n".format(length(Nil)))

        println("[length] Expected: 1")
        println("[length] Actual: %d\n".format(length(List(5))))

        println("[length] Expected: 3")
        println("[length] Actual: %d\n".format(length(List(5, 8, 10))))

        println("[foldLeft] Expected: 0")
        println("[foldLeft] Actual: %d\n".format(foldLeft(Nil:List[Int], 0)(_ + _)))

        println("[foldLeft] Expected: 5")
        println("[foldLeft] Actual: %d\n".format(foldLeft(List(3, 2), 0)(_ + _)))

        println("[foldLeft] Expected: 10")
        println("[foldLeft] Actual: %d\n".format(foldLeft(List(4, 3, 1, 2), 0)(_ + _)))

        println("[foldLeft] Expected: 1")
        println("[foldLeft] Actual: %d\n".format(foldLeft(Nil:List[Int], 1)(_ * _)))

        println("[foldLeft] Expected: 120")
        println("[foldLeft] Actual: %d\n".format(foldLeft(List(4, 3, 5, 2), 1)(_ * _)))

        println("[reverse] Expected: Nil")
        println("[reverse] Actual: %s\n".format(reverse(Nil)))

        println("[reverse] Expected: Cons(3,Nil)")
        println("[reverse] Actual: %s\n".format(reverse(List(3))))

        println("[reverse] Expected: Cons(3,Cons(4,Cons(5,Nil)))")
        println("[reverse] Actual: %s\n".format(reverse(List(5, 4, 3))))

        println("[foldLeft2] Expected: 120")
        println("[foldLeft2] Actual: %d\n".format(foldLeft2(List(4, 3, 5, 2), 1)(_ * _)))

        println("[foldRight2] Expected: 120")
        println("[foldRight2] Actual: %d\n".format(foldRight2(List(4, 3, 5, 2), 1)(_ * _)))

        println("[append2] Expected: Cons(3,Cons(4,Cons(5,Cons(6,Nil))))")
        println("[reverse] Actual: %s\n".format(append2(List(3, 4), List(5, 6))))
    }
}

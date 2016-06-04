package fpinscala.datastructures

sealed trait Tree[+A]
case class Leaf[A](value: A) extends Tree[A]
case class Branch[A](left: Tree[A], right: Tree[A]) extends Tree[A]


object Tree
{
    def size[A](t: Tree[A]): Int =
    {
//        def loop[A](l: Tree[A], r: Tree[A], acc: Int): Int =
//        (l, r) match
//        {
//            case (Leaf(_), Leaf(_)) => acc + 2
//            case (Leaf(_), Branch(p, q)) => loop(p, q, acc + 1)
//            case (Branch(p, q), Leaf(_)) => loop(p, q, acc + 1)
//            case (Branch(p1, q1), Branch(p2, q2)) => loop(p1, q1, loop(p2, q2, acc))
//        }
//
//        t match
//        {
//            case Leaf(_) => 1
//            case Branch(l, r) => loop(l, r, 0)
//        }
        def loop[A](t: Tree[A], acc: Int): Int =
            t match
            {
                case Leaf(_) =>      1 + acc
                case Branch(l, r) => 1 + acc + loop(l, 0) + loop(r, 0)
            }

        loop(t, 0)
    }

    def maximum(t: Tree[Int]): Int =
    {
        def loop(t: Tree[Int], x: Int): Int =
            t match
            {
                case Leaf(v) =>  x.max(v)
                case Branch(l, r) => loop(l, x).max(loop(r, x))
            }

        loop(t, 0)
    }

    def depth[A](t: Tree[A]): Int =
        ???

    def map[A, B](t: Tree[A])(f: A => B): Tree[B] =
        ???

    // 3.29
//    def fold[A, B](t: Tree[A])(
}

object TestTree
{

    import Tree._

    def main(args: Array[String]): Unit =
    {
        println("[tail] Expected: 1")
        println("[tail] Actual: %s\n".format(size(Leaf(5))))

        println("[tail] Expected: 3")
        println("[tail] Actual: %s\n".format(size(Branch(Leaf(5), Leaf(7)))))

        println("[tail] Expected: 5")
        println("[tail] Actual: %s\n".format(size(Branch(Leaf(5), Branch(Leaf(9), Leaf(7))))))

        println("[tail] Expected: 9")
        println("[tail] Actual: %s\n".format(maximum(Branch(Leaf(5), Branch(Leaf(9), Leaf(7))))))
    }
}

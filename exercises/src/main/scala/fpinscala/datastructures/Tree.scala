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
    {
        def loop[A](t: Tree[A], d: Int): Int =
            t match
            {
                case Leaf(_) => d
                case Branch(l, r) => 1 + d + loop(l, 0).max(loop(r, 0))
            }

        loop(t, 0)
    }

    def map[A, B](t: Tree[A])(f: A => B): Tree[B] =
        t match
        {
            case Leaf(a) => Leaf(f(a))
            case Branch(a1, a2) => Branch(map(a1)(f), map(a2)(f))
        }

    // 3.29
    def fold[A, B](t: Tree[A])(l: A => B)(b: (B, B) => B): B =
    {
        def loop(t: Tree[A]): B =
            t match
            {
                case Leaf(a) => l(a)
                case Branch(left, right) => b(loop(left), loop(right))
            }

        loop(t)
    }

    def folded_size[A](t: Tree[A]): Int =
        fold(t)(_ => 1)((l, r) => l + r + 1)


}

object TestTree
{

    import Tree._

    def main(args: Array[String]): Unit =
    {
        println("[size] Expected: 1")
        println("[size] Actual: %s\n".format(size(Leaf(5))))

        println("[size] Expected: 3")
        println("[size] Actual: %s\n".format(size(Branch(Leaf(5), Leaf(7)))))

        println("[size] Expected: 5")
        println("[size] Actual: %s\n".format(size(Branch(Leaf(5), Branch(Leaf(9), Leaf(7))))))

        println("[maximum] Expected: 9")
        println("[maximum] Actual: %s\n".format(maximum(Branch(Leaf(5), Branch(Leaf(9), Leaf(7))))))

        println("[depth] Expected: 0")
        println("[depth] Actual: %s\n".format(depth(Leaf(5))))

        println("[depth] Expected: 1")
        println("[depth] Actual: %s\n".format(depth(Branch(Leaf(5), Leaf(7)))))

        println("[depth] Expected: 2")
        println("[depth] Actual: %s\n".format(depth(Branch(Leaf(5), Branch(Leaf(9), Leaf(7))))))

        println("[map] Actual: %s\n".format(
                map(Branch(Leaf(5), Branch(Leaf(9), Leaf(7))))(x => x * x))
                )

        println("[folded_size] Expected: 1")
        println("[folded_size] Actual: %s\n".format(folded_size(Leaf(5))))

        println("[folded_size] Expected: 3")
        println("[folded_size] Actual: %s\n".format(folded_size(Branch(Leaf(5), Leaf(7)))))

        println("[folded_size] Expected: 5")
        println("[folded_size] Actual: %s\n".format(folded_size(Branch(Leaf(5), Branch(Leaf(9), Leaf(7))))))

    }
}

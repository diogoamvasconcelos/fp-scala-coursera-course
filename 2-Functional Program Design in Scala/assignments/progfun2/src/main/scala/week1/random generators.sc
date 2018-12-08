

trait Generator[+T] {
    self =>

    def generate: T

    def map[S](f: T => S): Generator[S] = new Generator[S] {
        def generate: S = f(self.generate)
    }

    def flatMap[S](f: T => Generator[S]): Generator[S] = new Generator[S] {
        def generate: S = f(self.generate).generate
    }
}

def single[T](x: T): Generator[T] = new Generator[T] {
    def generate: T = x
}

def integers: Generator[Int] = new Generator[Int] {
    override def generate: Int = scala.util.Random.nextInt()
}

def booleans = integers.map(_ >= 0)


//Trees
trait Tree

case class Inner(left: Tree, right: Tree) extends  Tree
case class Leaf(x: Int) extends Tree

def leafs: Generator[Leaf] = for {
    x <- integers
} yield Leaf(x)

def inners: Generator[Inner] = for {
    l <- trees
    r <- trees
} yield Inner(l, r)

def trees: Generator[Tree] = for {
    isLeaf <- booleans
    tree <- if (isLeaf) leafs else inners
} yield tree


trees.generate
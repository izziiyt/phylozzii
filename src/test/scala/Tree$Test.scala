import org.scalatest.FunSuite
import scala.io.Source

/**
 * Created by yuto on 14/08/18.
 */
/**class Tree$Test extends FunSuite {

  val source = Source.fromFile("/home/yuto/data/myFdur/sample.nh")
  val query = source.getLines().foldLeft("")(_ + _)
  source.close()
  val root = Tree(query)
  root.setAlignment(Array[Int](1,3,2))

  test("decode"){
    val x = Tree.decode(query)
    x.foreach(println(_))
  }

  test("apply"){
    def f(arg:Tree){
      arg match{
        case Node(left,right,_)  => f(left); f(right)
        case Leaf(species,_,_) => println(species)
      }
    }
    f(root)
  }

  test("setAlignment"){
    def f(arg:Tree){
      arg match{
        case Node(left,right,_)  => f(left); f(right)
        case Leaf(species,_,nuc) => print(species); println(" " + nuc)
      }
    }
    f(root)
  }

  test("inside"){
    def f(arg:Tree){
      arg match{
        case Node(left,right,_)  => f(left); f(right)
        case Leaf(species,_,nuc) => println(species); println(arg.inside)
      }
    }
    f(root)
  }
}
**/
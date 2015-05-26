import collection._
import scala.collection.mutable.{Builder,MapBuilder}
import scala.collection.generic.CanBuildFrom
import scala.reflect.ClassTag

class PrefixMap[T1,T2] extends mutable.Map[Iterable[T1],T2] with mutable.MapLike[Iterable[T1],T2,PrefixMap[T1,T2]] {
  var suffixes: immutable.Map[T1,PrefixMap[T1,T2]] = Map.empty
  var value: Option[T2] = None

  def get(s: Iterable[T1]): Option[T2] =
    if(s.isEmpty) value
    else suffixes get s.head flatMap (_.get(s.drop(1)))

  def withPrefix(s: Iterable[T1]): PrefixMap[T1,T2] =
    if(s.isEmpty) this
    else {
      val leading = s.head
      suffixes get leading match {
        case None =>
          suffixes = suffixes + (leading -> empty)
        case _ => Unit
      }
      suffixes(leading) withPrefix s.drop(1)
    }

  //def withPrefix(s: T1): PrefixMap[T1,T2] = withPrefix(Iterable(s))

  override def update(s: Iterable[T1],elem: T2): Unit =
    withPrefix(s).value = Some(elem)

  //def update(s: T1,elem: T2): Unit = update(Iterable(s),elem)

  override def remove(s: Iterable[T1]): Option[T2] =
    if(s.isEmpty) {val prev = value; value = None; prev}
    else suffixes get s.head flatMap (_.remove(s.drop(1)))

  //def remove(s: T1): Option[T2] = remove(Iterable(s))

  def iterator:Iterator[(Iterable[T1],T2)] =
    (for (v <- value.iterator) yield (Iterable[T1](),v)) ++
    (for ((i,m) <- suffixes.iterator; (s,v) <- m.iterator) yield (s,v))
    

  def += (kv: (Iterable[T1],T2)): this.type = {update(kv._1,kv._2);this}

  //def += (kv: (T1,T2)): this.type = {update(kv._1,kv._2);this}

  def -= (s: Iterable[T1]): this.type = {remove(s);this}

  //def -= (s: T1): this.type = {remove(s);this}

  override def empty = new PrefixMap[T1,T2]

}

object PrefixMap extends {
  def empty[T1,T2] = new PrefixMap[T1,T2]

  def apply[T1,T2](kvs: (Iterable[T1],T2)*): PrefixMap[T1,T2] = {
    val m:PrefixMap[T1,T2] = empty
    for(kv <- kvs) m += kv
    m
  }

  /*def newBuilder[T1,T2]: Builder[(T1,T2),PrefixMap[T1,T2]] = new MapBuilder[T1,T2,PrefixMap[T1,T2]](empty)

  implicit def canBuildFrom[T1,T2]:CanBuildFrom[PrefixMap[_,_],(T1,T2),PrefixMap[T1,T2]] =
  new CanBuildFrom[PrefixMap[_,_],(T1,T2),PrefixMap[T1,T2]] {
    def apply(from:PrefixMap[_,_]) = newBuilder[T1,T2]
    def apply() = newBuilder[T1,T2]
  }*/
}
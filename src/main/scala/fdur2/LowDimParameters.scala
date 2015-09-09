package fdur2

case class LowDimParameters(pix:Array[Double],bx:Array[Double]) {
  require(pix.length == 4)
  require(bx.length == 6)
  require(pix.forall(_ >= 0.0))
  require(bx.forall(_ >= 0.0))
}

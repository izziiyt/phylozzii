object DNA {
  object A extends DNA(0)
  object C extends DNA(1)
  object G extends DNA(2)
  object T extends DNA(3)
  object N extends DNA(4){
    override def isNuc = false
  }

  val values = Array(A, C, G, T, N)
  val exceptN = Array(A, C, G, T)

  private val codeTable = Array(A, C, G, T, N, N, N, N)

  def complement(code:Int) : DNA = codeTable((~code & 0x03) | (code & 0x04))
}

sealed abstract class DNA(val code:Int) {
  val name = this.getClass.getSimpleName.replaceAll("""\$""", "")
  override def toString = name
  def complement = DNA.complement(code)
  implicit def toInt:Int = code
  def isNuc = true
}
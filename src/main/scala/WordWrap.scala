package FormatTextForScreen
class WordWrap(t: String) {
  //implementation that uses tail recursion directly, as opposed to using foldLeft, which is shorter.
  type Text = String
  type Word = String
  type Line = List[Word]

  def words(t: Text): List[Word] = t.split("\\s+").toList
  def lineLength(w: List[Word]): Int = w.size-1 + w.foldLeft(0)(_ + _.size)

  def lines(lineWidth: Int): List[Line] = {
    //main algorithm
    def linesRec(found: List[Line], remaining: List[Word]): List[Line] = {
      if(remaining.size == 0) return found
      val newLine = nextLine(remaining, lineWidth)
      linesRec(found :+ newLine, remaining.drop(newLine.size))
    }

    linesRec(List[Line](), words(t))
  }

  def nextLine(t: List[Word], lineWidth: Int): Line= {
    //helper function that return next line <= lineWidth
    def nextLineRec(line: List[Word], t: List[Word]): Line = {
      if (lineLength(line) > lineWidth) return if (line.size==1) line else line.dropRight(1)
      if( t.size == 0) return line
      val newLine:List[Word] = line :+ t.take(1)(0)
      val remaining:List[Word] = t.drop(1)
      nextLineRec(newLine, remaining)
    }

    nextLineRec(List[Word](), t)
  }
}

object WordWrap{
  def apply(t: String):WordWrap = new WordWrap(t)
}

object Test extends App{
  //I used code below to compare my solution to the one at:
  //https://www.rosettacode.org/wiki/Word_wrap#Scala
  def printListLine(l: List[String]): Unit = println((l mkString " ") drop 1)

  val txtAsLines =
    WordWrap("Vijftig jaar geleden publiceerde Edsger Dijkstra zijn kortstepadalgoritme. Daarom een kleine ode" +
      " aan de in 2002 overleden Dijkstra, iemand waar we als Nederlanders best wat trotser op mogen zijn. Dijkstra was" +
      " een van de eerste programmeurs van Nederland. Toen hij in 1957 trouwde, werd het beroep computerprogrammeur door" +
      " de burgerlijke stand nog niet erkend en uiteindelijk gaf hij maar `theoretische natuurkundige’ op.\nZijn" +
      " beroemdste resultaat is het kortstepadalgoritme, dat de kortste verbinding vindt tussen twee knopen in een graaf" +
      " (een verzameling punten waarvan sommigen verbonden zijn). Denk bijvoorbeeld aan het vinden van de kortste route" +
      " tussen twee steden. Het slimme van Dijkstra’s algoritme is dat het niet alle mogelijke routes met elkaar" +
      " vergelijkt, maar dat het stap voor stap de kortst mogelijke afstanden tot elk punt opbouwt. In de eerste stap" +
      " kijk je naar alle punten die vanaf het beginpunt te bereiken zijn en markeer je al die punten met de afstand tot" +
      " het beginpunt. Daarna kijk je steeds vanaf het punt dat op dat moment de kortste afstand heeft tot het beginpunt" +
      " naar alle punten die je vanaf daar kunt bereiken. Als je een buurpunt via een nieuwe verbinding op een snellere" +
      " manier kunt bereiken, schrijf je de nieuwe, kortere afstand tot het beginpunt bij zo’n punt. Zo ga je steeds een" +
      " stukje verder tot je alle punten hebt gehad en je de kortste route tot het eindpunt hebt gevonden.")
      .lines(120)

  txtAsLines foreach printListLine
}

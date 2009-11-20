//TicTacToe in Konsole

//Build: scalac -deprecation TicTacToe.scala
//Run: scala TicTacToe star

import java.util.Calendar;

object TicTacToe {
  
  // wer hat in eine Zelle gesetzt
  // wer hat gewonnen
  object who extends Enumeration {
    val Empty = Value("_")
    val Me = Value("o")
    val You = Value("x");
    val Nobody = Value("Niemand");
  }

  type Who = who.Value

  // falls Spiel entschieden -> Ausstieg aus Eingabeschleife
  // falls Gewinnerposition ermittelt -> Ausstieg aus Suche
  case class GameOverException(
    val player:Who, 
    //das ist der ermittelte Zug (direkter oder indirekter Siegeszug)
    val nextMove:Option[(int,int)]) extends Exception

  type Position = (Int,Int)
    type Triple = (Position,Position,Position)

      //Liste von Gewinnerpositionen
      val winnerTriples = List[Triple](
        //Zeilen
        ((0,0),(0,1),(0,2)), 
        ((1,0),(1,1),(1,2)), 
        ((2,0),(2,1),(2,2)),
        //Spalten
        ((0,0),(1,0),(2,0)), 
        ((0,1),(1,1),(2,1)), 
        ((0,2),(1,2),(2,2)),
        //Diagonalen
        ((0,0), (1,1), (2,2)), 
        ((0,2), (1,1), (2,0)))

  // Dient zum Parsen der Konsoleneingabe für Zug in eine Zelle
  // Exctractor: "l,c" => Paar 
  object Cell {
    def unapply(str: String): Option[Position] = { 
      val parts = str split ","
      if (parts.length == 2) Some((parts(0).toInt, parts(1).toInt)) else None
    } 
  }

  //Spielfeld: 3x3 Array
  var master = new Array[Array[Who]](3,3)


  // Argument "start": Programm macht ersten Zug
  // Argument "auto": Programm spielt gegen sich selbst
  def main(args: Array[String]) {
    val autoMove = args.exists( _ == "auto")
    if (!autoMove) {
      println("Du setzt: " + who.You + "\nProgramm setzt: " + who.Me)
      println("Eingabe 0,0 bedeutet Zug in Zelle links oben");
    }
    init()
    var line = ""

    var move = autoMove || args.exists( _ == "start")
    try {
      while(true) {
        if(move) {
          makeMove(who.Me)
          checkGameOver()
        }
        render(master)
        if (autoMove) {
          println()
          makeMove(who.You)
          checkGameOver()
        } else {
          line = readLine()
          line match { 
            case Cell(l,c) => 
              if ( l < 0 || l > 2  || c < 0 || c > 2) {
                println("Ungültiger Index")
                move = false
              } else if (getWho(master, (l,c)) == who.Empty) { 
                move = true
                setWho(master,(l,c), who.You)
                checkGameOver()
              } else {
                println ("Zeile: " + l + " Spalte: " + c + " nicht leer.")
                move = false
              }
            case _ => println("Ungültige Zelle: " + line); move = false
          }
        }
      } 
    } catch {
      case ex: GameOverException => println( ex.player + " hat gewonnen");render(master)
    }
  }
  
  // Siegstellung oder Remis => GameOverException
  def checkGameOver() {
    findWinner(master) match {
      case Some(winner) => throw new GameOverException(winner, None)
      case None => {}
    }
    if (emptyCells(master).isEmpty) {
      throw new GameOverException(who.Nobody, None)
    }
  }

  
  type Field = Array[Array[Who]];

  // Programm zieht
  def makeMove(player:Who):Unit = {
    
    var stack = new scala.collection.mutable.Stack[(Int,Position)]
    for( myCell <-emptyCells(master)) {
      val clone = cloneField(master)
      setWho(clone, myCell, player)
      var utilVal = utility(player, clone, getOtherPlayer(player), 0)
      stack.push((utilVal, myCell))
    }
    val rand = new scala.util.Random(Calendar.getInstance().getTimeInMillis())

    def chooseOneOfMaxUtility(u:(Int,Position), v:(Int,Position)):(Int,Position) = {
      if (u._1 == v._1) {
        return if(rand.nextBoolean) u else v
      }
      if (u._1 > v._1) return u else v
    }

    val randomMax = stack.toList.reduceLeft( chooseOneOfMaxUtility(_,_))
    setWho(master, randomMax._2, player)
  }

  // jetzt richtiger MiniMax Algo
  def utility(player:Who, field:Field, mover:Who, depth:Int):Int = {
    findWinner(field) match {
      case Some(winner) => {return if(winner == player) (10 -depth) else (-10 + depth) }
      case None => {}
    }
    if(emptyCells(field).isEmpty) {
      return 0;
    } 
    var stack = new scala.collection.mutable.Stack[Int]
    for( myCell <-emptyCells(field)) {
      val clone = cloneField(field)
      setWho(clone, myCell, mover)
      stack.push(utility(player, clone, getOtherPlayer(mover), depth +1))
    }

    def xor(a:Boolean,b:Boolean):Boolean = if (a == b) false else true 
    def miniMax(x:Int,y:Int):Int = if (xor(x > y, player == mover)) y else x;

    return stack.toList.reduceLeft( miniMax(_,_))
  }

  def findWinner(field:Field):Option[Who] = {
    
    def isWinner(triple:Triple):Boolean = {
      getWho(field, triple._1) != who.Empty  && getWho(field, triple._2) == getWho(field, triple._1) && 
      getWho(field, triple._3) == getWho(field,triple._1)
    }
    
    winnerTriples.find(isWinner(_)) match {
      case Some(triple) => Some(getWho(field, triple._1))
      case None => None
    }
  }

  // liefert Gegner von player
  def getOtherPlayer(player:Who):Who = {
    if (player.equals(who.Me)) who.You else who.Me
  }

  // liefert eine geklontes Spielfeld (Besetzung identisch zu Parameter field)
  def cloneField(field:Field):Field = { 
    // funktioniert tatsächlich
    return field.map( l => l.map( v => v))
  }

  // zeigt Spielfeld an
  def render(field:Field) {
    for(l <- field) {
      for (c <- l) {
        print(c + "\t")
      }
      println()
    }
  }
  
  // welcher player hat in die Zelle t des Feldes gesetzt
  def getWho(field:Field, t:Position): Who = {
    field(t._1)(t._2)
  }

  // player setzt in Zelle t
  def setWho(field:Field, t:Position, player:Who) {
    field(t._1)(t._2) = player
  }

  // Spielfeld initialisieren
  def init() {
    //Comprehension ist verständlicher
    for(l <- master; i <- 0 until l.length) {
      l(i) = who.Empty
    }
  }
  
  // liefert freie Zellen
  def emptyCells(field:Field):Seq[Position] = {
    for (
      l <- 0 to 2; 
      c <- 0 to 2
      if who.Empty.equals(field(l)(c))
    ) yield(l,c)
  }

}

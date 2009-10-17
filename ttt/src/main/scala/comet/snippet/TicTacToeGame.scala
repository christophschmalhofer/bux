package comet.snippet

import _root_.net.liftweb.util.Log
import comet.model._
import comet.model.TicTacToe._
import _root_.net.liftweb.http.SHtml._
import _root_.net.liftweb.http.S._
import _root_.net.liftweb.http.js.JE._
import _root_.net.liftweb.http.js.JsCmds._
import _root_.net.liftweb.http.js.JsCmd


import _root_.net.liftweb.util.Log
import _root_.scala.xml._

class TicTacToeGame {

  def render = {

    TicTacToe.init
    var lineIndex = -1
    
    def renderLine(l:Array[who.Value]): Node = {
      lineIndex += 1 
      var columnIndex = -1

      def renderCell(cell:who.Value):Node = { 
        columnIndex += 1

        // das sind Closure Variablen für callback
        val currentColumn = columnIndex
        val currentLine = lineIndex

        def idStr() = currentLine.toString + currentColumn.toString

        // das ist die Ajax Callback Methode (enthält die Spiellogik)
        def callback():JsCmd =  {

          Log.info("In Zelle (" + currentLine + ", " + currentColumn +") gesetzt")

          //Spielfeld wird readonly angezeigt
          def displayResultBoard():JsCmd = {
            var cmds = Noop
            for( l <- 0 to 2; c <- 0 to 2) {
              cmds = cmds & SetHtml( l.toString + c.toString, Text( TicTacToe.getCellValue((l,c)).toString)) 
            } 
            cmds
          }
          
          try {
            TicTacToe.moveToPosition((currentLine, currentColumn), TicTacToe.who.You)
            val myMove = TicTacToe.computeMove(TicTacToe.who.Me)
            TicTacToe.checkGameOver()
            CmdPair(SetHtml( idStr(), Text(TicTacToe.who.You.toString)), 
                    SetHtml( myMove._1.toString + myMove._2.toString, Text(TicTacToe.who.Me.toString)))
          } catch {
            case ex: TicTacToe.GameOverException => { 
              Log.info( ex.player + " hat gewonnen")
              notice( ex.player + " hat gewonnen")
              displayResultBoard}
          }
        }

        // eine Zelle einer Zeile
        <td>
        <span id={idStr()}>
        {
          a(callback _, Text(cell.toString))
        }
        </span>
        </td> 
      }
      // eine Zeile des Bretts
      <tr> 
      { 
        l.map( cell => renderCell(cell))
      }
      </tr>
    }
    // das gesamte Spielbrett
    <div>
    <table border="1" style="font-size:300%;">
    { 
      TicTacToe.master.map( l => renderLine(l)) 
    }
    </table> 
    </div>
  }
}


import scala.xml._
import scala.xml.Elem._

object gen extends Application {

val html = 
  <table> {
    for(l <- 0 to 2; 
      line = <tr>  {
        for(c <- 0 to 2;
            column = <td id={"cell" + l + c} />
          ) yield column
      }
      </tr>
    ) yield line
  }
  </table>

  println(html)
}

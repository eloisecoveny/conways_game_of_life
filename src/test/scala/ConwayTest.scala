import com.github.eloisecoveny.{AliveCell, Board, MainObject}
import org.scalatest.{Matchers, WordSpec}

class ConwayTest extends WordSpec with Matchers {

  def assertBoard(board: Board, expectedPattern: Set[(Int, Int)]): Unit = {
    val aliveCells = board.cells.collect {
      case aliveCell: AliveCell => aliveCell.coordinates
    }
    assert(aliveCells == expectedPattern)
  }

  def assertMultipleIterations(board: Board, expectedPatterns: List[Set[(Int, Int)]]): Unit = {
    expectedPatterns.foldLeft(board){
      case (prevBoardState, currentExpectedPattern) =>
        val iteratedBoard = MainObject.iterate(prevBoardState)
        assertBoard(iteratedBoard, currentExpectedPattern)
        iteratedBoard
    }
  }

  "Conways Game of Life" should {

    "show correct states for Blinker" in {
      val blinker = Set((3, 2), (3, 3), (3, 4)).map(AliveCell.apply)
      val firstIteration = MainObject.init(5, 5, blinker)
      val secondIteration = MainObject.iterate(firstIteration)
      assertBoard(secondIteration, Set((2, 3), (3, 3), (4, 3)))
    }

    "show correct states for Glider" in {
      val stateOne = Set((3, 2), (1, 3), (3, 3), (3, 4), (2, 4)).map(AliveCell.apply)
      val firstIteration = MainObject.init(5, 5, stateOne)

      assertMultipleIterations(firstIteration, List(
        Set((2, 2), (3, 3), (4, 3), (2, 4), (3, 4)),
        Set((3, 2), (4, 3), (2, 4), (3, 4), (4, 4)),
        Set((2, 3), (4, 3), (3, 4), (4, 4), (3, 5)),
        Set((4, 3), (2, 4), (4, 4), (3, 5), (4, 5))
      ))
    }
  }
}

package com.github.eloisecoveny

object MainObject {

  def main(args: Array[String]): Unit = {

  }

  def iterate(prevIt: Board): Board = {
    def cntNbrs(coord: (Int, Int)) = countLiveNeighbours(coord, prevIt)
    val newCells: Set[Cell] = prevIt.cells.map {
      case AliveCell(coord) if cntNbrs(coord) < 2 => DeadCell(coord)
      case alive@ AliveCell(coord) if (2 to 3).contains(cntNbrs(coord)) => alive
      case AliveCell(coord) if cntNbrs(coord) > 3 => DeadCell(coord)
      case DeadCell(coord) if cntNbrs(coord) == 3 => AliveCell(coord)
      case cell => cell
    }
    val res = prevIt.copy(cells = newCells)
    println()
    println(res)
    println()
    res
  }

  def countLiveNeighbours(coord: (Int, Int), board: Board): Int = {
    val (x, y) = coord
    val xRange = ((x - 1) to (x + 1)).toSet
    val yRange = ((y - 1) to (y + 1)).toSet
    val neighboursCoord = xRange.flatMap(xCell => yRange.map(yCell => (xCell, yCell))).filterNot(neighbourCoord => neighbourCoord == coord)
    neighboursCoord.count(neighbour => board.cells.exists(boardCell => boardCell.coordinates == neighbour && boardCell.isInstanceOf[AliveCell]))
  }

  def init(colNum: Int, rowNum: Int, liveCells: Set[AliveCell]): Board = {
    val emptyBoard = Board(colNum, rowNum)
    val columns = (1 to colNum).toSet
    val rows = (1 to rowNum).toSet
    val popBoard = columns.flatMap(column => rows.map(row => DeadCell(column, row)))
    val newCellSet: Set[Cell] = popBoard.map {
      case DeadCell((c, r)) if liveCells.contains(AliveCell((c, r))) => AliveCell(c, r)
      case deadCell => deadCell
    }

    val res = emptyBoard.copy(cells = newCellSet)
    println("new board")
    println(res)
    println()
    res
  }
}

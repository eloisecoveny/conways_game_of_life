package com.github.eloisecoveny

case class Board(columns: Int, rows: Int, cells: Set[Cell] = Set.empty) {
  override def toString: String = {
    (1 to rows).map {
      yRow =>
        (1 to columns).map {
          xCol =>
            cells.find(_.coordinates == (xCol, yRow)).map(_.toString).getOrElse("")
        }.mkString("")
    }.mkString("\n")
  }
}

trait Cell {
  val coordinates: (Int, Int)
}

case class AliveCell(coordinates: (Int, Int)) extends Cell {
  override def toString: String = "■"
}

case class DeadCell(coordinates: (Int, Int)) extends Cell {
  override def toString: String = "□"
}



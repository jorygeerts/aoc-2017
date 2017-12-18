class Day1 {

  def solveA(input: String) : Int = {
    this.solveForPairs(input, this.pairA);
  }

  def pairA(arr: Array[Char]) : Iterator[Array[Char]] = {
    val circledArr = arr :+ arr(0);
    circledArr.sliding(2, 1)
  }

  def solveB(input: String) : Int = {
    this.solveForPairs(input, this.pairB);
  }

  def pairB(arr: Array[Char]) : Iterator[Array[Char]] = {
    val chunkSize = (arr.length / 2) + 1;
    val circledArr = arr ++ arr.slice(0, chunkSize-1);

//    for (item <- circledArr) {
//      println(item);
//    }

    val chunks = circledArr.sliding(chunkSize, 1).map(
      chunk => Array(chunk(0), chunk(chunkSize - 1))
    );

//    for (chunk <- chunks) {
//      println("Chunk " + chunk(0) + " & " + chunk(1));
//    }

    chunks;
  }

  def solveForPairs(input: String, pairUp: Array[Char] => Iterator[Array[Char]]) : Int = {
    pairUp(input.toCharArray())
      .map(
        pair => if (pair(0) == pair(1))
          pair(0).toString().toInt
        else
          0
      )
      .sum;
  }
}

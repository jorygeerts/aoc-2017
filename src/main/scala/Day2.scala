class Day2 {

  def solveA(input: String) : Int = {
    input.lines.map(
      line => line.split("\\s+")
        .filter(char => char.size > 0)
        .filter(char => !char.isEmpty)
        .map(char => char.toInt)
      )
      .filter(nums => nums.length > 0)
      .map(nums => nums.max - nums.min)
      .sum;
  }

  def solveB(input: String) : Int = {
    val mapped = input.lines.map(
      line => line.split("\\s+")
        .filter(char => char.size > 0)
        .filter(char => !char.isEmpty)
        .map(char => char.toInt)
    ).filter(nums => nums.length > 0)
      .map(
        nums => nums.flatMap(a => nums.map(b => a -> b))
          .filter(pair => pair._1 != pair._2)
          .map(
            pair =>
              if (pair._1 % pair._2 == 0)
                pair._1 / pair._2
              else
                0
          )

      )
      ;

    mapped.map(nums => nums.sum).sum;
  }

}

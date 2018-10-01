import scala.util.matching._

object hw4 {
  
  def firstMatch(input: String, reported: List[Regex]) : String =
    if (reported.isEmpty) null
    else {
      val m = reported.head.findPrefixOf(input).getOrElse(null)
        if (m == null) firstMatch(input, reported.tail)
      else m
      }                                           
  
  def tokens(input: String, reported : List[Regex], ignored: List[Regex]) : (List[String], Int) = {
    def tokensHelper(input: String, reported: List[Regex]) : List[String] =
    {
      if (input.length == 0) Nil else {
       val m = firstMatch(input, ignored)
        if (m == null) {
          val n = firstMatch(input, reported)
          if (n == null) Nil
          else  n :: tokensHelper(input.substring(n.length), reported)
          }
        else tokensHelper(input.substring(m.length), reported)
       }
    }
    def tokensIndexer(input: String, reported: List[Regex]) : Int =
    {
      if (input.length == 0) 0 else {
       val m = firstMatch(input, ignored)
        if (m == null) {
          val n = firstMatch(input, reported)
          if (n == null) 0
          else  n.length + tokensIndexer(input.substring(n.length), reported)
          }
        else m.length + tokensIndexer(input.substring(m.length), reported)
       }
    }
    def lastIndexChecker(input: String, reported: List[Regex]) : Boolean =
    {
      if (input.length == 0) true else {
       val m = firstMatch(input, ignored)
        if (m == null) {
          val n = firstMatch(input, reported)
          if (n == null) false
          else  lastIndexChecker(input.substring(n.length), reported)
          }
        else lastIndexChecker(input.substring(m.length), reported)
       }
    }
    
    val tokenList = tokensHelper(input, reported)
    val index = tokensIndexer(input, reported)
    val entireStringMatched = lastIndexChecker(input, reported)
    if (entireStringMatched) (tokenList, -1)
    else (tokenList, index)
    }                                       
      
         
  val phoneLetters = Map("2" -> "ABC", "3" -> "DEF", "4" -> "GHI", "5" -> "JKL", "6" -> "MNO",
   "7" -> "PRS", "8" -> "TUV", "9" -> "WXY")     
   
  val characters = (s: String) => s.toList.map("" + _)
                                        
  
  val letters = phoneLetters.map(e => (e._1, characters(e._2)))
                              
  val cats = (s: List[String], t: List[String]) => t.flatMap(y => s.map(x => x + y))
              
  val words = io.Source.fromURL("http://horstmann.com/sjsu/spring2018/cs152/words").
    getLines.filter(w => Character.isLowerCase(w(0)) && w.length > 1).
    map(_.toUpperCase).toSet + "SCALA"     
                                               
  val wordsForDigits = (digits: String) => digits.toList.map(e => letters(e.toString)).reduceLeft((x, y) => cats(x,y)).filter(words)
                                               

  val catsSpaces = (s: List[String], t: List[String]) => t.flatMap(y => s.map(x => x + " " + y))
                                                      
  val wordsForDigitsSequence = (seq: List[String]) =>
    seq.map(e => wordsForDigits(e)).reduceLeft(catsSpaces)
          
  val grow1 = (c: String, substringLists: List[List[String]]) => substringLists.map(s => c :: s)
                  

  val grow2 = (c: String, substringLists: List[List[String]]) => substringLists.map(s => c + s.head :: s.tail)
                                            
      
  val grow = (c: String, a: List[List[String]]) => grow1(c, a) ++ grow2(c, a)
  
  val substrings = (s: String) => (characters(s.substring(0, s.length - 1)) :\ List(List(s.substring(s.length-1)))) (grow(_, _))
                                                  
  val phoneMnemonics = (digits: String) => substrings(digits).map(wordsForDigitsSequence).flatten
                   
  
}
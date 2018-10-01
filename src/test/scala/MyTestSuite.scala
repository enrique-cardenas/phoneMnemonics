import org.scalatest.FunSuite

class MyTestSuite extends FunSuite {
  
  test("Testing tokens") {
      val keeps = List("if|def|val".r, """\p{L}(\p{L}|\p{N}|_)*""".r,
    """[+-]?\p{N}+""".r, "[+*/%<=>-]".r, "[(){};]".r)
     val igs = List("""\p{Z}+""".r)
     assert(hw4.tokens("if(x<0) 0 else root(x);", keeps, igs)._1.equals(List("if", "(", "x", "<", "0", ")", "0", "else", "root", "(", "x", ")", ";")))
     assert(hw4.tokens("if(x<0) 0&else root(x);", keeps, igs).equals(List("if", "(", "x", "<", "0", ")", "0"),9))
     assert(!(hw4.tokens("if(x<0) 0&else root(x);", keeps, igs).equals(List("if", "(", "x", "<", "0", ")", "0",20))))
  }
  
  
  test("Testing catsSpaces") {
    
    assert(hw4.catsSpaces(hw4.letters("2"), hw4.letters("3")).toSet == Set("A D", "B D", "C D", "A E", "B E", "C E", "A F", "B F", "C F"))
    
  }
  
  test("Testing wordsForDigits"){
    assert(hw4.wordsForDigits("72252").contains("SCALA"))
  }
  
  test("Testing wordsForDigitSequence"){
    assert(hw4.wordsForDigitsSequence(List("72252", "47", "386")).contains("SCALA IS FUN"))
  }
  
  test("Testing grow"){
    assert(hw4.grow("1", List(List("23"),
  List("2", "3"))) == List(List("1", "23"), List("1", "2", "3"), List("123"), List("12", "3")))
  }
  
  test("Testing substrings"){
    
    assert(hw4.substrings("21") == List(List("2", "1"), List("21")))
  }
  
  test("Testing phoneMnemonics"){
    assert(hw4.phoneMnemonics("7225247386").contains("SCALA IS FUN"))
  }
  
}
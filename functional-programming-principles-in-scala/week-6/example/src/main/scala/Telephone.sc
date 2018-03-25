import scala.io.Source
import java.text.Normalizer

def stripAccents(input: String): String =
  Normalizer.normalize(input, Normalizer.Form.NFD).replaceAll("\\p{InCombiningDiacriticalMarks}+", "")

val mnemonics = Map('2' -> "ABC", '3' -> "DEF", '4' -> "GHI", '5' -> "JKL",
  '6' -> "MNO", '7' -> "PQRS", '8' -> "TUV", '9' -> "WXYZ")

val words = Source.fromURL("https://pastebin.com/raw/7uPetKWh").getLines().toList
  .map(w => stripAccents(w))

def translate(phoneNumber: String): List[String] = ???

val charCode: Map[Char, Char] =
  for {(num, letters) <- mnemonics; letter <- letters} yield letter -> num

def wordCode(word: String): String = word.toUpperCase map charCode
val java = wordCode("Java")

val wordsForNum: Map[String, Seq[String]] = words groupBy wordCode

val phoneNumber = "7225247386"

translate(phoneNumber) contains "Scala is fun"


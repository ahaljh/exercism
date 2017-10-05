
object FoodChain {
  def song: String = {

    val lyricses: List[Lyrics] = List(
      Lyrics(1, "fly", None, None)
      , Lyrics(2, "spider", Some("fly"), Some("It wriggled and jiggled and tickled inside her."))
      , Lyrics(3, "bird", Some("spider that wriggled and jiggled and tickled inside her"), Some("How absurd to swallow a bird!"))
      , Lyrics(4, "cat", Some("bird"), Some("Imagine that, to swallow a cat!"))
      , Lyrics(5, "dog", Some("cat"), Some("What a hog, to swallow a dog!"))
      , Lyrics(6, "goat", Some("dog"), Some("Just opened her throat and swallowed a goat!"))
      , Lyrics(7, "cow", Some("goat"), Some("I don't know how she swallowed a cow!"))
      , Lyrics(1, "horse", None, None)
    )

    def midLyrics(lyrics: Lyrics): List[String] = {
      val mid = lyricses.filter(l => l.no <= lyrics.no && l.catched != None)
        .reverse
        .map(l => s"She swallowed the ${l.animal} to catch the ${l.catched.get}.")

      lyrics.desc match {
        case Some(sentence) => sentence :: mid
        case None => mid
      }
    }

    def lastLyrics(animal: String) =
      if (animal == "horse") "She's dead, of course!"
      else "I don't know why she swallowed the fly. Perhaps she'll die."


    def makeLyrics(lyrics: Lyrics): List[String] = {
      (s"I know an old lady who swallowed a ${lyrics.animal}." ::
        midLyrics(lyrics)) :+
        lastLyrics(lyrics.animal)
    }

    lyricses.map(makeLyrics(_).mkString("\n") +"\n\n").mkString

  }

}

case class Lyrics(no: Int, animal: String, catched: Option[String], desc: Option[String])
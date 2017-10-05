/**
  * Created by ahaljh on 2017. 4. 6..
  */
object House {
  def rhyme: String = {
    (1 to sentences.size).map {
      start +
        sentences.take(_).reverse
          .map { stc =>
            " " +stc.subject + (if (!stc.verb.isEmpty) "\n" +"that " +stc.verb else "")
          }.mkString("") +
        " " +end

    }.mkString("")
  }

  val start = "This is"
  val end = "that Jack built.\n\n"

  val sentences = List(
    sentence("the house" , "", "")
    , sentence("the malt" , "lay in", "the house")
    , sentence("the rat", "ate", "the malt")
    , sentence("the cat", "killed", "the rat")
    , sentence("the dog", "worried", "the cat")
    , sentence("the cow with the crumpled horn", "tossed", "the dog")
    , sentence("the maiden all forlorn", "milked", "the cow with the crumpled horn")
    , sentence("the man all tattered and torn", "kissed", "the maiden all forlorn")
    , sentence("the priest all shaven and shorn", "married", "the man all tattered and torn")
    , sentence("the rooster that crowed in the morn", "woke", "the priest all shaven and shorn")
    , sentence("the farmer sowing his corn", "kept", "the rooster that crowed in the morn")
    , sentence("the horse and the hound and the horn", "belonged to", "the farmer sowing his corn")
    )
}

case class sentence(subject: String, verb: String, obj: String)
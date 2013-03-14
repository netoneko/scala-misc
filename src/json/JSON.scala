import scala.util.parsing.json._

class GithubApi() {
  case class Commit(val sha: String, val message: String) {
    override def toString = s"{commit $sha: $message}"
  }

  val url = "https://api.github.com"

  private def parse(command: String): Option[Any] = {
    val request = s"$url/$command"
    println(s"Request: $request")

    val source = io.Source.fromURL(request).mkString
    JSON.parseFull(source)
  }

  private def getAs[T](option: Option[Any]): T = option.get.asInstanceOf[T]

  def getCommits(owner: String, repo: String): List[Commit] = {
    val json = parse(s"repos/$owner/$repo/commits")

    json.get.asInstanceOf[List[Map[String, Any]]].map { (item) =>
      val sha = getAs[String](item.get("sha"))
      val commit = getAs[Map[String, Any]](item.get("commit"))
      val message = getAs[String](commit.get("message"))

      Commit(sha, message)
    }
  }
}

object Test extends App {
  val api = new GithubApi

  println(api getCommits("netoneko", "scala-misc"))
}

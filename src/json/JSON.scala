import scala.util.parsing.json._

class GithubApi() {
  case class Commit(val sha: String, val message: String) {
    override def toString = s"commit $sha"
  }

  val url = "https://api.github.com"

  private def parse(command: String): Option[Any] = {
    val request = s"$url/$command"
    println(s"Request: $request")

    val source = io.Source.fromURL(request).mkString
    JSON.parseFull(source)
  }

  def getCommits(owner: String, repo: String): List[Commit] = {
    val json = parse(s"repos/$owner/$repo/commits")

    json.get.asInstanceOf[List[Map[String, String]]].map { (item) =>
      val sha = item.get("sha").get
//      val commit = item.get("commit").asInstanceOf[Map[String, String]]
//      val message = commit.get("message").get

      Commit(sha, "") //message)
    }
  }
}

object Test extends App {
  val api = new GithubApi

  println(api getCommits("netoneko", "scala-misc"))
}

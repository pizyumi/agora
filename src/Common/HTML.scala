package Common

object HTML {
  lazy val br: String = "<br/>"

  def createHTML(title: String, content: String): String = {
    var html: String = ""
    html += "<!DOCTYPE html>" + __.newlineString
    html += "<html>" + __.newlineString
    html += "<head>" + __.newlineString
    html += "<meta charset=\"utf-8\" />" + __.newlineString
    html += "<title>" + title + "</title>" + __.newlineString
    html += "</head>" + __.newlineString
    html += "<body>" + __.newlineString
    html += content + __.newlineString
    html += "</body>" + __.newlineString
    html += "</html>" + __.newlineString
    html
  }
  def createHeader(header: String, level: Int): String = "<h" + level.toString + ">" + header + "</h" + level.toString + ">"
  def createParagraph(paragraph: String): String = "<p>" + paragraph + "</p>"
  def createSpan(text: String): String = "<span>" + text + "</span>"
  def createImage(href: String): String = "<img src=\"" + href + "\"/>"
  def createForm(action: String, content: String): String = "<form method=\"post\" action=\"" + action + "\">" + content + "</form>"
  def createTextbox(name: String, size: Int): String = "<input type=\"text\" name=\"" + name + "\" size=\"" + size.toString + "\"/>"
  def createButton(name: String): String = "<input type=\"submit\" value=\"" + name + "\"/>"
}
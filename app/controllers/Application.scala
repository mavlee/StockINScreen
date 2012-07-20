package controllers

import io.Source
import java.net.URL
import java.io.FileNotFoundException
import play.api._
import play.api.mvc._
import org.scribe.oauth.OAuthService
import org.scribe.builder.ServiceBuilder
import org.scribe.builder.api.LinkedInApi
import org.scribe.builder.api.Api
import org.scribe.model.Verifier
import org.scribe.model.Token
import org.scribe.model.Response
import org.scribe.model.OAuthRequest
import org.scribe.model.Verb
import com.google.gson.Gson
import json.Profile
import json.Connections
import json.Company
import json.Positions
import scala.actors.Future
import scala.actors.Futures._

object Application extends Controller {

  //get the auth token out and get data otherwise redirect them
  def index = Action { implicit request =>
    val accToken = session.get("acc_token")
    val accSecret = session.get("acc_secret")
    accToken match {
      case Some(k:String) => {
        val oauthService = getOauthService
        val accessToken = new Token(k,accSecret.get)

        var totalChange = 1.0

        //get the data
        val profileData = getProfileData(oauthService,accessToken).getBody()
        val connectionData = getConnectionData(oauthService,accessToken).getBody()
        val gson = new Gson() //created to make the objects
        val connections = gson.fromJson(connectionData,classOf[Connections])

        //sort
        import scala.collection.JavaConversions._
        connections.values = sampleFromList(connections.values, 50)
        connections.values = connections.values.sortWith((x,y) => x.firstName < y.firstName)
        val myProfile = gson.fromJson(profileData,classOf[Profile])
        connections.values = (myProfile :: connections.values.toList).toArray
        val friendsFutures = connections.values.map{ friend => future{
          val theirStocks = getPositions(friend.positions)
          var theirChange = theirStocks.foldLeft(1.0){(x:scala.Double,y:(String, String, scala.Double, scala.Double, scala.Double))=>
            x.doubleValue*(y._5.doubleValue/100 + 1.0)
          }
          theirChange = scala.math.round((theirChange-1.0)*10000)/100.0
          (friend.firstName, friend.lastName, theirChange, theirStocks)
        }}
        val friendsScores= friendsFutures.map{future => future()}.filter(_._3!=0).sortWith((x,y) => 
          ((x._1==myProfile.firstName)&&x._2==myProfile.lastName)||
          ((!(y._1==myProfile.firstName&&y._2==myProfile.lastName))&&x._3 > y._3)).toList
        val sample = friendsScores.last
        println("%s %s %s".format(sample._1, sample._2, sample._3))
        sample._4.foreach{ x=> println("%s %s %s %s".format(x._1, x._3, x._4, x._5))}
        Ok(views.html.index.render(myProfile, friendsScores))
      }
      case _ =>{
        Logger.info("Redirecting to auth page")
        Redirect(routes.Application.auth())
      }
    }
  }

  def sampleFromList[A](xs : Array[A], n : Int):Array[A] = {
    val len = xs.length
    import java.util.Random
    val r = new Random
    var count = 0
    var index = 0
    var as = xs.take(n)
    while (count < n) {
      val stillNeed = n - count
      val haveLeft = len - index
      val prob = 1.0*stillNeed/haveLeft
      if (r.nextDouble < prob) {
        as(count) = xs(index)
        count = count + 1
      }
      index = index + 1
    }
    as
  }

  def getPositions(positions: Any):List[(String, String, scala.Double, scala.Double, scala.Double)] = {
    import scala.collection.JavaConversions._
    if (positions==null||(!positions.asInstanceOf[java.util.LinkedHashMap[String, Any]].containsKey("values")))
      List.empty[(String, String, Double, Double, Double)]
    else {
      var myPositions = positions.asInstanceOf[java.util.LinkedHashMap[String, Any]].get("values").asInstanceOf[java.util.ArrayList[java.util.HashMap[String, java.util.HashMap[String, Any]]]].toList
      myPositions = myPositions.filter(_.get("company").containsKey("name")).filter(_.containsKey("startDate"))
      val stocks = myPositions.map{ p =>
      val ticker = if (p.get("company").containsKey("ticker")) p.get("company").get("ticker").toString else ""
        val companyName = p.get("company").get("name").toString
        val startDate = p.get("startDate").asInstanceOf[java.util.HashMap[String, Double]]
        val startMonth = if (startDate.containsKey("month")) startDate.get("month").toInt else 0
        val startYear = startDate.get("year").toInt

        val endDate = if (p.containsKey("endDate")) p.get("endDate").asInstanceOf[java.util.HashMap[String, Double]] else new java.util.HashMap[String, Double]()
        val today = new java.util.Date
        val endMonth = if (endDate.contains("month")) endDate.get("month").toInt else (1+today.getMonth)
        val endYear = if (endDate.contains("year")) endDate.get("year").toInt else (1900+today.getYear)
        val stockInfo = getStockData(ticker, startMonth, startYear, endMonth, endYear)
        (companyName.toString, ticker.toString, stockInfo._1.toDouble, stockInfo._2.toDouble, stockInfo._3.toDouble)
      }
      stocks
    }
  }

  //the auth action step
  def auth = Action {
    val serv = getOauthService
    val reqT = serv.getRequestToken()
    val authURL = serv.getAuthorizationUrl(reqT)
    //redirect and store request in cookie
    Redirect(authURL).withSession(
        "req_token" -> reqT.getToken(),
        "req_secret" -> reqT.getSecret()
    )
  }

  //the callback route for oauth
  def callback(oauth_token: String, oauth_verifier: String) = Action { implicit request =>
    val reqToken = session.get("req_token")
    val reqSecret = session.get("req_secret")
    reqToken match {
      case Some(k:String) if k==oauth_token => {
        Logger.error("Tokens DID match")
        val serv = getOauthService
        val verifier = new Verifier(oauth_verifier)
        val reqT = new Token(k, request.session.get("req_secret").get)
        val accessToken = serv.getAccessToken(reqT,verifier)
        Redirect(routes.Application.index()).withSession(
              "acc_token" -> accessToken.getToken(),
              "acc_secret" -> accessToken.getSecret()
              )
      }
      case _ => {
        Logger.error("Tokens didn't match")
        Redirect(routes.Application.auth())
      }
    }
  }

  //the following functions should be in some helper class
  def getOauthService:OAuthService = {
    //could extract this to be checked on startup
    val key = Play.current.configuration.getString("linkedin.apiKey") match {
      case Some(k:String) => k
      case _ => throw new Exception("no linkedin api key set in configuration")
    }
    val secret = Play.current.configuration.getString("linkedin.apiSecret") match {
      case Some(k:String) => k
      case _ => throw new Exception("no linkedin api secret set in configuration")
    }
    val callback = Play.current.configuration.getString("linkedin.callback") match {
      case Some(k:String) => k
      case _ => throw new Exception("no linkedin api callback url set in configuration")
    }
    //get the oauth service
    val sb:ServiceBuilder = new ServiceBuilder()
        .provider(classOf[LinkedInApi])
        .apiKey(key)
        .apiSecret(secret)
        .callback(callback)
    sb.build
  }

  def getProfileData(oauthService:OAuthService,accessToken:Token):Response = {
    val fields = "(id,first-name,last-name,summary,industry,headline,picture-url,positions:(company:(name,ticker),start-date,end-date))"
    val requestURL = "http://api.linkedin.com/v1/people/~:"+fields+"?format=json"
    val req = new OAuthRequest(Verb.GET, requestURL);
    val oauthService = getOauthService
    oauthService.signRequest(accessToken, req);
    req.send();
  }

  def getConnectionData(oauthService:OAuthService,accessToken:Token):Response = {
    val fields = "(id,first-name,last-name,summary,industry,headline,picture-url,positions:(company:(name,ticker),start-date,end-date))"
    val requestURL = "http://api.linkedin.com/v1/people/~/connections:"+fields+"?format=json"
    val req = new OAuthRequest(Verb.GET, requestURL);
    oauthService.signRequest(accessToken, req);
    req.send();
  }

  def getStockData(ticker:String,sM:Int,sY:Int,eM:Int,eY:Int):(scala.Double, scala.Double, scala.Double) = {
    if (ticker == "") { 
      (1.0, 1.0, 0.0) 
    } else {
      val startPrice = scala.math.round(getStockPrice(ticker, sM, sY) * 10000) / 10000.0
      val endPrice = scala.math.round(getStockPrice(ticker, eM, eY) * 10000) / 10000.0
      val change = scala.math.round((endPrice-startPrice) * 10000/startPrice) / 100.0
      (startPrice, endPrice, change)
    }
  }

  def getStockPrice(stock:String, month:Int, year:Int):scala.Double = {
    try {
      val url = if (month > 0) 
      "http://ichart.yahoo.com/table.csv?s=%s&a=%d&b=1&c=%d&d=%d&e=31&f=%d&g=w&ignore=.csv".format(stock, (month-1), year, (month-1), year) 
      else
      "http://ichart.yahoo.com/table.csv?s=%s&a=0&b=1&c=%d&d=11&e=31&f=%d&g=m&ignore=.csv".format(stock, year, year) 

      val connection = new URL(url).openConnection
      var lines = Source.fromInputStream(connection.getInputStream).getLines.drop(1).toList
      lines = lines.filter{x:String => (x.split(",").head.split("-").head.toInt == year)}
      if (lines.size == 0) 1
      else {
        val res = lines.map(_.split(",").drop(1).dropRight(2).map(_.toDouble).fold(0.0)(_+_)/4).fold(0.0)(_+_)/lines.size
        res
      }
    } catch {
      case _ => 1
    }
  }
}
